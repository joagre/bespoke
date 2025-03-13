// -*- fill-column: 100 -*-

"use strict";

const AddTopMessage = (function () {
  const {html, render} = uhtml;
  let _titleUsername;
  let _attachmentCounter;
  let _formRecipientInput;
  let _formRecipientDropdown;
  let _recipientDropdown;
  let _formRecipientList;
  let _recipients;
  let _selectedRecipientUsernames = [];
  let _formSelectedRecipients;
  let _formTitle;
  let _formBody;
  let _addButton;
  let _uploadControllers = [];

  function init() {
    window.addEventListener("beforeunload", () => {
      // Abort still ongoing uploads
      for (const uploadController in _uploadControllers) {
        Bespoke.abortUpload(uploadController);
      }
    });
    Bespoke.onReady("add_top_message.html", () => _load());
  }

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get header elements
    _titleUsername = document.getElementById("title-username");
    _attachmentCounter = document.getElementById("attachment-counter");

    // Get recipient elements (and add event listeners)
    _formRecipientInput = document.getElementById("form-recipient-input");
    _formRecipientDropdown = document.getElementById("form-recipient-dropdown");
    _recipientDropdown = UIkit.dropdown(_formRecipientDropdown);
    _formRecipientList = document.getElementById("form-recipient-list");
    _formSelectedRecipients = document.getElementById("form-selected-recipients");
    _formRecipientInput.addEventListener("click", _searchRecipients);
    _formRecipientInput.addEventListener("input", _searchRecipients);

    // Get message title element (and add event listener)
    _formTitle = document.getElementById("form-title");
    _formTitle.addEventListener("input", () => _checkFormCompletion());

    // Get message body element (and add event listener)
    _formBody = document.getElementById("form-body");
    _formBody.addEventListener("input", () => _checkFormCompletion());

    // Get add button element
    _addButton = document.getElementById("add-button");

    // Only keep uploaded files if we're coming back from the attachments page
    const previousPage = Bespoke.getPreviousPage();
    if (!previousPage.endsWith("add_attachments.html")) {
      AddAttachments.clearUploadedFiles();
      Bespoke.setLocalItem("selectedRecipientUsernames", []);
      Bespoke.setRawLocalItem("title", "");
      Bespoke.setRawLocalItem("body", "");
    }

    _updatePage();
    Bespoke.initMobileKeyboardResizing("#form-body");
  }

  function _searchRecipients(event) {
    event.stopPropagation();

    // Extract query
    const query = event.currentTarget.value.trim().toLocaleLowerCase();
    if (query.length === 0) {
      _hideRecipientDropdown();
      return;
    }

    // REST: Read recipients
    const payload = {
      ignoredUsernames: _selectedRecipientUsernames,
      query: query
    };
    fetch("/api/search_recipients", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(payload),
    })
      .then(response => {
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          if (response.status === 401) {
            Bespoke.navigateTo("loader.html");
          }
          return;
        }
        return response.json();
      })
      .then(recipients => {
        if (recipients.length === 0) {
          return;
        }
        _recipients = recipients;
        _populateRecipients(query);
      })
      .catch(error => {
        console.error("Page update failed:", error);
      });
  }

  function _populateRecipients(query) {
    // Find usernames
    const usernames = _recipients
          .map(recipient => recipient.username)
          .map(username => ({original: username, lower: username.toLocaleLowerCase()}))
          .filter(recipientObj => recipientObj.lower.includes(query.toLocaleLowerCase()))
          .map(recipientObj => recipientObj.original);
    if (usernames.length > 0) {
      _formRecipientInput.classList.remove("uk-text-danger");
      let list = [];
      for (let i = 0; i < usernames.length; i++) {
        if (!_selectedRecipientUsernames.includes(usernames[i])) {
          const substrings = usernames[i].split(new RegExp(`(${query})`, 'gi'));
          for (let i = 0; i < substrings.length; i++) {
            if (substrings[i].toLocaleLowerCase() === query) {
              substrings[i] = html`<span class="substring">${substrings[i]}</span>`;
            } else {
              substrings[i] = html`${substrings[i]}`;
            }
          }
          const entry = html`<li class="recipient" onclick=${() => {
                           _addSelectedRecipient(usernames[i]);
                           _hideRecipientDropdown();
                         }}><span class="uk-margin-small-right"
                                  uk-icon="users"></span>${substrings}</li>`;
          list.push(entry);
        }
      }
      render(_formRecipientList, html`${list}`);
      _showRecipientDropdown();
    } else {
      _formRecipientInput.classList.add("uk-text-danger");
      _hideRecipientDropdown();
    }
  }

  function _checkFormCompletion() {
    _addButton.disabled = (_selectedRecipientUsernames.length === 0 ||
                           _formTitle.value.trim() === "" ||
                           _formBody.value.trim() === "");
  }

  function _updatePage() {
    // Update header
    const username = Bespoke.getCookieValue("username");
    _titleUsername.textContent = username;
    _attachmentCounter.textContent = AddAttachments.numberOfUploadedFiles();
    // Update form
    _selectedRecipientUsernames = Bespoke.getLocalItem("selectedRecipientUsernames", []);
    _refreshSelectedRecipients();
    const title = Bespoke.getRawLocalItem("title", "");
    _formTitle.value = title;
    const body = Bespoke.getRawLocalItem("body", "");
    _formBody.value = body;

    _checkFormCompletion();
    document.body.hidden = false;
    _formRecipientInput.focus();
  }

  function addNow(event) {
    Bespoke.ignoreEvent(event);
    _updateServer();
  }

  async function _updateServer() {
    // Prepare message body (all recpipents are hardcoded to get the same unencrypted message)
    const body = new Uint8Array(2 + _formTitle.value.length + _formBody.value.length);
    const view = new DataView(body.buffer);
    // Title size as a 16 bit unsigned big endian integer
    view.setUint16(0, _formTitle.value.length, false);
    const encoder = new TextEncoder();
    const titleBytes = encoder.encode(_formTitle.value);
    const bodyBytes = encoder.encode(_formBody.value);
    body.set(titleBytes, 2);
    body.set(bodyBytes, 2 + titleBytes.length);

    // Upload a message body for each recipient
    let bodyBlobs = [];
    for (const selectedRecipientUsername of _selectedRecipientUsernames) {
      try {
        // Upload body
        console.log(`Uploading body for ${selectedRecipientUsername}...`);
        const uploadController = Bespoke.uploadData(body, "body");
        _uploadControllers.push(uploadController);
        const uploadedFile = await Bespoke.waitForUpload(uploadController);
        console.log("Message body has been uploaded:", uploadedFile);

        // Prepare body blob
        const userId = _lookupUserId(selectedRecipientUsername);
        const filename = _absPathToFilename(uploadedFile.absPath);
        bodyBlobs.push({userId, filename})

        // Remove upload controller
        const index = _uploadControllers.indexOf(uploadController);
        _uploadControllers.splice(index, 1);
      } catch (error) {
        console.error("File upload failed:", error);
      }
    }

    // REST: Create top message
    console.log("Creating top message...");
    const payload = {
      bodyBlobs,
      attachmentBlobs: []
    }
    const response = await fetch("/api/create_message", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(payload),
    });
    if (!response.ok) {
      if (response.status === 401) {
        Bespoke.navigateTo("loader.html");
      } else {
        console.error(`Server error: ${response.status}`);
      }
      return;
    }
    response.json().then(result => {
      console.log("Top message has been created:", result);
    });


/*




    const uploadedFiles = AddAttachments.getUploadedFiles();
    const attachments =
          uploadedFiles.map(file => ({
            filename: file.absPath.replace("/tmp/", ""),
            contentType: file.contentType,
          }));
    try {
      // REST: Create top message
      const payload = {
        title: _formTitle.value,
        body: _formBody.value,
        attachments
      };
      const response = await fetch("/api/create_message", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(payload),
      });
      if (!response.ok) {
        if (response.status === 401) {
          Bespoke.navigateTo("loader.html");
        } else {
          console.error(`Server error: ${response.status}`);
        }
        return;
      }
      Bespoke.navigateTo("top_messages.html");
    } catch (error) {
      console.error("Addition of top message failed:", error);
      }
      */
  }

  function gotoAddAttachments(event) {
    Bespoke.setLocalItem("selectedRecipientUsernames", _selectedRecipientUsernames);
    Bespoke.setRawLocalItem("title", _formTitle.value);
    Bespoke.setRawLocalItem("body", _formBody.value);
    Bespoke.gotoPage(event, "add_attachments.html");
  }

  function _addSelectedRecipient(match) {
    _selectedRecipientUsernames.push(match);
    _refreshSelectedRecipients();
  }

  function _refreshSelectedRecipients() {
    _checkFormCompletion();
    _formSelectedRecipients.hidden = _selectedRecipientUsernames.length === 0;
    const recipients = [];
    for (const selectedRecipientUsername of _selectedRecipientUsernames) {
      const recipientSpan =
            html`<span class="uk-label no-uppercase uk-margin-small-right">${selectedRecipientUsername}<a class="uk-margin-small-left uk-link-reset" onclick=${(_event) => {
                    _selectedRecipientUsernames =
                      _selectedRecipientUsernames.filter(
                        username => username !== selectedRecipientUsername);
                    _refreshSelectedRecipients();
                  }}>&times;</a></span>`;
      recipients.push(recipientSpan);
    }
    render(_formSelectedRecipients, html`${recipients}`);
  }

  function _showRecipientDropdown() {
    _recipientDropdown.show();
  }

  function _hideRecipientDropdown() {
    _recipientDropdown.hide(false);
  }

  function _lookupUserId(selectedRecipientUsername) {
    for (const recipient of _recipients) {
      if (recipient.username === selectedRecipientUsername) {
        return recipient.userId;
      }
    }
    return null;
  }

  function _absPathToFilename(absPath) {
    return absPath.replace("/tmp/", "");
  }

  return {
    init,
    addNow,
    gotoAddAttachments
  };
})();

AddTopMessage.init();
