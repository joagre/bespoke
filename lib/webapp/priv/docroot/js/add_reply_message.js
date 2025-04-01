// -*- fill-column: 100 -*-

"use strict";

const AddReplyMessage = (function () {
  const {html, render} = uhtml;
  const _userId = Bespoke.getCookieValue("userId");
  const _username = Bespoke.getCookieValue("username");
  const _parentMessageId = Bespoke.getLocalItem("parentMessageId", null);
  const _topMessageId = Bespoke.getLocalItem("topMessageId");

  let _recipientDropdown;
  let _recipients;
  let _selectedRecipientUsernames = [];
  let _uploadControllers = [];

  let _titleUsernameElement;
  let _attachmentCounterElement;
  let _parentTitleElement;
  let _parentAuthorElement;
  let _parentBodyElement;
  let _formRecipientInputElement;
  let _formRecipientDropdownElement;
  let _formRecipientListElement;
  let _formSelectedRecipientsElement;
  let _formBodyElement;
  let _addButtonElement;
  let _topMessage;
  let _parentMessageTitle;

  window.addEventListener("beforeunload", () => {
    // Abort still ongoing uploads
    for (const uploadController in _uploadControllers) {
      Bespoke.abortUpload(uploadController);
    }
  });

  Bespoke.onReady("add_reply_message.html", () => _load());

  async function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get header elements
    _titleUsernameElement = document.getElementById("title-username");
    _attachmentCounterElement = document.getElementById("attachment-counter");

    // Get parent message elements
    _parentTitleElement = document.getElementById("parent-title");
    _parentAuthorElement = document.getElementById("parent-author");
    _parentBodyElement = document.getElementById("parent-body");

    // Get recipient elements and add event listeners
    _formRecipientInputElement = document.getElementById("form-recipient-input");
    _formRecipientDropdownElement = document.getElementById("form-recipient-dropdown");
    _recipientDropdown = UIkit.dropdown(_formRecipientDropdownElement);
    _formRecipientListElement = document.getElementById("form-recipient-list");
    _formSelectedRecipientsElement = document.getElementById("form-selected-recipients");
    _formRecipientInputElement.addEventListener("click", _searchRecipients);
    _formRecipientInputElement.addEventListener("input", _searchRecipients);

    // Get message body element and add event listener
    _formBodyElement = document.getElementById("form-body");
    _formBodyElement.addEventListener("input", () => checkFormCompletion());

    // Get add button element
    _addButtonElement = document.getElementById("add-button");

    // Only keep uploaded files if coming back from attachments page
    const previousPage = Bespoke.getPreviousPage();
    if (!previousPage.endsWith("add_attachments.html")) {
      await AddAttachments.clearFiles();
      Bespoke.setLocalItem("selectedRecipientUsernames", []);
      Bespoke.setLocalItem("recipients", []);
      Bespoke.setRawLocalItem("body", "");
    }

    _updatePage();
    Bespoke.initMobileKeyboardResizing("#form-body");
  }

  function checkFormCompletion() {
    // The message author is always a recipient
    _addButtonElement.disabled = (_selectedRecipientUsernames.length < 2 ||
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

    // REST: Read top message
    const readMessageResult = await MessageLib.readMessage(_topMessageId, _userId);
    if (!readMessageResult.ok) {
      return;
    }
    _topMessage = readMessageResult.message;

    // REST: Read parent message and body
    const readMessageAndBlobResult = await MessageLib.readMessageAndBlob(_parentMessageId, _userId);
    if (!readMessageAndBlobResult.ok) {
      return;
    }
    let parentMessage;
    let parentMessageBody;
    if (readMessageAndBlobResult.body != null) {
      ({message: parentMessage, title: _parentMessageTitle, body: parentMessageBody} =
       readMessageAndBlobResult);
    } else {
      _parentMessageTitle = "<No content>";
      parentMessageBody = "<No content>";
    }

    // Update parent message
    _parentTitleElement.textContent = _parentMessageTitle;
    _parentAuthorElement.textContent = parentMessage.authorUsername;
    Bespoke.formatMarkdown(_parentBodyElement, parentMessageBody);

    // Update form
    _selectedRecipientUsernames = Bespoke.getLocalItem("selectedRecipientUsernames", []);
    if (_selectedRecipientUsernames.length === 0) {
      _selectedRecipientUsernames = _topMessage.recipients.map(recipient => recipient.username);
      _selectedRecipientUsernames.push(_username);
      _recipients = _topMessage.recipients;
    } else {
      _recipients = Bespoke.getLocalItem("recipients", []);
    }
    _refreshSelectedRecipients();
    const body = Bespoke.getRawLocalItem("body", "");
    _formBodyElement.value = body;

    checkFormCompletion();
    document.body.hidden = false;
    _formRecipientInputElement.focus();
  }

  function addNow(event) {
    Bespoke.ignoreEvent(event);
    MessageLib.createMessageAndBlobs(_parentMessageTitle, _formBodyElement.value, _parentMessageId,
                                     _topMessageId, _recipients, _selectedRecipientUsernames,
                                     _uploadControllers);
  }

  function gotoAddAttachments(event) {
    Bespoke.setLocalItem("selectedRecipientUsernames", _selectedRecipientUsernames);
    Bespoke.setLocalItem("recipients", _recipients);
    Bespoke.setRawLocalItem("body", _formBodyElement.value);
    Bespoke.gotoPage(event, "add_attachments.html");
  }

  function toggleQuote(event) {
    Bespoke.ignoreEvent(event);
    const quoteCard = document.querySelector(".quote-card");
    const isHidden = quoteCard.hidden;
    const quoteAction = document.getElementById("quote-action");
    if (isHidden) {
      quoteCard.hidden = false;
      quoteAction.setAttribute("uk-icon", "chevron-up");
    } else {
      quoteCard.hidden = true;
      quoteAction.setAttribute("uk-icon", "chevron-down");
    }
  }

  function gotoAddReplyPage(event, messageId) {
    if (messageId == null) {
      messageId = Number(event.currentTarget.getAttribute("data-message-id"));
    }
    Bespoke.setLocalItem("parentMessageId", messageId);
    Bespoke.gotoPage(event, "add_reply_message.html");
  }

  return {
    checkFormCompletion,
    setRecipients,
    showRecipientDropdown,
    hideRecipientDropdown,
    addSelectedRecipient,
    refreshSelectedRecipientsNow,
    addNow,
    gotoAddAttachments,
    toggleQuote,
    gotoAddReplyPage,
  };
})();
