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
  let _formSelectedRecipients;
  let _formTitle;
  let _formBody;
  let _addButton;

  // FIXME: Remove
  const recipients =
        ["Apple", "Banana", "Cherry", "Date", "Elderberry", "Fig", "Grapes", "Honeydew"];
  const _selectedRecipients = new Set();

  function init() {
    Bespoke.onReady("add_top_message.html", () => _load());
  }

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Header
    _titleUsername = document.getElementById("title-username");
    _attachmentCounter = document.getElementById("attachment-counter");

    // Recipients
    _formRecipientInput = document.getElementById("form-recipient-input");
    _formRecipientDropdown = document.getElementById("form-recipient-dropdown");
    _recipientDropdown = UIkit.dropdown(_formRecipientDropdown);
    _formRecipientList = document.getElementById("form-recipient-list");
    _formSelectedRecipients = document.getElementById("form-selected-recipients");

    // Title
    _formTitle = document.getElementById("form-title");
    _formTitle.addEventListener("input", () => _checkFormCompletion());

    // Body
    _formBody = document.getElementById("form-body");
    _formBody.addEventListener("input", () => _checkFormCompletion());

    // Add button
    _addButton = document.getElementById("add-button");

    // Only keep uploaded files if we're coming back from the attachments page
    const previousPage = Bespoke.getPreviousPage();
    if (!previousPage.endsWith("add_attachments.html")) {
      AddAttachments.clearUploadedFiles();
      Bespoke.setRawLocalItem("title", "");
      Bespoke.setRawLocalItem("body", "");
    }

    // Recipients event listeners
    _formRecipientInput.addEventListener("click", _searchUsernames);
    _formRecipientInput.addEventListener("input", _searchUsernames);

    _updatePage();
    Bespoke.initMobileKeyboardResizing("#form-body");
  }

  function _searchUsernames(event) {
    event.stopPropagation();

    // Extract query
    const query = event.currentTarget.value.trim().toLocaleLowerCase();
    if (query.length === 0) {
      _hideRecipientDropdown();
      return;
    }

    // Find matches
    const matches = recipients
          .map(recipient => ({original: recipient, lower: recipient.toLocaleLowerCase()}))
          .filter(recipientObj => recipientObj.lower.includes(query.toLocaleLowerCase()))
          .map(recipientObj => recipientObj.original);
    if (matches.length > 0) {
      _formRecipientInput.classList.remove("uk-text-danger");
      let list = [];
      for (let i = 0; i < matches.length; i++) {
        if (!_selectedRecipients.has(matches[i])) {
          const substrings = matches[i].split(new RegExp(`(${query})`, 'gi'));
          for (let i = 0; i < substrings.length; i++) {
            if (substrings[i].toLocaleLowerCase() === query) {
              substrings[i] = html`<span class="substring">${substrings[i]}</span>`;
            } else {
              substrings[i] = html`${substrings[i]}`;
            }
          }
          const entry = html`<li class="recipient" onclick=${() => {
                           _addSelectedRecipient(matches[i]);
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
    _addButton.disabled = (_formTitle.value.trim() === "" || _formBody.value.trim() === "");
  }

  function _updatePage() {
    // Update header
    const username = Bespoke.getCookieValue("username");
    _titleUsername.textContent = username;
    _attachmentCounter.textContent = AddAttachments.numberOfUploadedFiles();

    // Update form
    const title = Bespoke.getRawLocalItem("title", "");
    _formTitle.value = title;
    const body = Bespoke.getRawLocalItem("body", "");
    _formBody.value = body;

    _checkFormCompletion();
    document.body.hidden = false;
    _formTitle.focus();
  }

  function addNow(event) {
    Bespoke.ignoreEvent(event);
    _updateServer();
  }

  async function _updateServer() {
    const uploadedFiles = AddAttachments.getUploadedFiles();
    const attachments =
          uploadedFiles.map(file => ({
            filename: file.absPath.replace("/tmp/", ""),
            contentType: file.contentType,
          }));
    try {
      const payload = {
        title: _formTitle.value,
        body: _formBody.value,
        attachments
      };
      // REST: Create top message
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
  }

  function gotoAddAttachments(event) {
    Bespoke.setRawLocalItem("title", _formTitle.value);
    Bespoke.setRawLocalItem("body", _formBody.value);
    Bespoke.gotoPage(event, "add_attachments.html");
  }

  function _addSelectedRecipient(match) {
    _selectedRecipients.add(match);
    _refreshSelectedRecipients();
  }

  function _refreshSelectedRecipients() {
    _formSelectedRecipients.hidden = _selectedRecipients.size === 0;
    const recipients = [];
    for (const selectedRecipient of _selectedRecipients) {
      const recipientSpan =
            html`<span class="uk-label no-uppercase uk-margin-small-right">${selectedRecipient}<a class="uk-margin-small-left uk-link-reset" onclick=${(_event) => {
                    _selectedRecipients.delete(selectedRecipient);
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

  return {
    init,
    addNow,
    gotoAddAttachments
  };
})();

AddTopMessage.init();
