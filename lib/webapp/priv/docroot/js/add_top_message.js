// -*- fill-column: 100 -*-

"use strict";

const AddTopMessage = (function () {
  const {html, render} = uhtml;
  const _username = Bespoke.getCookieValue("username");

  let _recipientDropdown;
  let _recipients;
  let _selectedRecipientUsernames = [];
  let _uploadControllers = [];

  let _titleUsernameElement;
  let _attachmentCounterElement;
  let _formRecipientInputElement;
  let _formRecipientDropdownElement;
  let _formRecipientListElement;
  let _formSelectedRecipientsElement;
  let _formTitleElement;
  let _formBodyElement;
  let _addButtonElement;

  window.addEventListener("beforeunload", () => {
    // Abort still ongoing uploads
    for (const uploadController in _uploadControllers) {
      Bespoke.abortUpload(uploadController);
    }
  });

  Bespoke.onReady("add_top_message.html", () => _load());

  async function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get header elements
    _titleUsernameElement = document.getElementById("title-username");
    _attachmentCounterElement = document.getElementById("attachment-counter");

    // Get recipient elements and add event listeners
    _formRecipientInputElement = document.getElementById("form-recipient-input");
    _formRecipientDropdownElement = document.getElementById("form-recipient-dropdown");
    _recipientDropdown = UIkit.dropdown(_formRecipientDropdownElement);
    _formRecipientListElement = document.getElementById("form-recipient-list");
    _formSelectedRecipientsElement = document.getElementById("form-selected-recipients");
    _formRecipientInputElement.addEventListener("click", _searchRecipients);
    _formRecipientInputElement.addEventListener("input", _searchRecipients);

    // Get message title element and add event listener
    _formTitleElement = document.getElementById("form-title");
    _formTitleElement.addEventListener("input", () => checkFormCompletion());

    // Get message body element and add event listener
    _formBodyElement = document.getElementById("form-body");
    _formBodyElement.addEventListener("input", () => checkFormCompletion());

    // Get add button element
    _addButtonElement = document.getElementById("add-button");

    // Only keep uploaded files if coming back from attachments page
    const previousPage = Bespoke.getPreviousPage();
    if (!previousPage.endsWith("add_attachments.html")) {
      await AddAttachments.clearFiles();
      Bespoke.setLocalItem("selectedRecipientUsernames", [_username]);
      Bespoke.setLocalItem("recipients", []);
      Bespoke.setRawLocalItem("title", "");
      Bespoke.setRawLocalItem("body", "");
    }

    _updatePage();
    Bespoke.initMobileKeyboardResizing("#form-body");
  }

  function checkFormCompletion() {
    // The message author is always a recipient
    _addButtonElement.disabled = (_selectedRecipientUsernames.length < 2 ||
                                  _formTitleElement.value.trim() === "" ||
                                  _formBodyElement.value.trim() === "");
  }

  function _searchRecipients(event) {
    MessageLib.searchRecipients(event, showRecipientDropdown, hideRecipientDropdown,
                                setRecipients, addSelectedRecipient, _selectedRecipientUsernames,
                                _formRecipientInputElement, _formRecipientListElement);
  }

  function showRecipientDropdown() {
    _recipientDropdown.show();
  }

  function hideRecipientDropdown() {
    _recipientDropdown.hide(false);
  }

  function setRecipients(recipients) {
    _recipients = recipients;
  }

  function addSelectedRecipient(match) {
    _selectedRecipientUsernames.push(match);
    _refreshSelectedRecipients();
  }

  function _refreshSelectedRecipients() {
    MessageLib.refreshSelectedRecipients(checkFormCompletion, refreshSelectedRecipientsNow,
                                         _selectedRecipientUsernames, _username,
                                         _formSelectedRecipientsElement);
  }

  function refreshSelectedRecipientsNow(selectedRecipientUsername) {
    _selectedRecipientUsernames =
      _selectedRecipientUsernames.filter(
        username => username !== selectedRecipientUsername);
    _refreshSelectedRecipients();
  }

  async function _updatePage() {
    // Update header
    _titleUsernameElement.textContent = _username;
    _attachmentCounterElement.textContent = await AddAttachments.numberOfFiles();

    // Update form
    _selectedRecipientUsernames = Bespoke.getLocalItem("selectedRecipientUsernames", [_username]);
    _recipients = Bespoke.getLocalItem("recipients", []);
    _refreshSelectedRecipients();
    const title = Bespoke.getRawLocalItem("title", "");
    _formTitleElement.value = title;
    const body = Bespoke.getRawLocalItem("body", "");
    _formBodyElement.value = body;

    checkFormCompletion();
    document.body.hidden = false;
    _formRecipientInputElement.focus();
  }

  function addNow(event) {
    Bespoke.ignoreEvent(event);
    MessageLib.createMessageAndBlobs(_formTitleElement.value, _formBodyElement.value, null, null,
                                     _recipients, _selectedRecipientUsernames, _uploadControllers);
  }

  function gotoAddAttachments(event) {
    Bespoke.setLocalItem("selectedRecipientUsernames", _selectedRecipientUsernames);
    Bespoke.setLocalItem("recipients", _recipients);
    Bespoke.setRawLocalItem("title", _formTitleElement.value);
    Bespoke.setRawLocalItem("body", _formBodyElement.value);
    Bespoke.gotoPage(event, "add_attachments.html");
  }

  return {
    checkFormCompletion,
    setRecipients,
    showRecipientDropdown,
    hideRecipientDropdown,
    addSelectedRecipient,
    refreshSelectedRecipientsNow,
    addNow,
    gotoAddAttachments
  };
})();
