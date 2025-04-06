// -*- fill-column: 100 -*-

"use strict";

const Message = (function () {
  const {html, render} = uhtml;
  const _REFRESH_INTERVAL = 30000;
  const _userId = Bespoke.getCookieValue("userId");
  const _username = Bespoke.getCookieValue("username");
  const _topMessageId = Bespoke.getLocalItem("topMessageId");

  let _isLoadingData = false;
  let _firstLoad = true;
  let _dataLoaded = false;
  let _domReady = false;
  let _refreshTimer = null;
  let _topMessage;
  let _topMessageTitle = null;
  let _topMessageBody = null;
  let _replyMessages;
  let _messageIdToDelete;

  let _stickyHeaderElement;
  let _titleUsernameElement;
  let _mainContentElement;
  let _topMessageElement;
  let _topTitleElement;
  let _topRecipientsElement;
  let _topBodyElement;
  let _topAttachmentsElement;
  let _topAuthorElement;
  let _topAgeElement;
  let _topHaveRepliesElement;
  let _topRepliesElement;
  let _topDeleteElement;
  let _topAddReplyElement;
  let _deleteMessageBodyElement;
  let _repliesElement;

  window.addEventListener("beforeunload", () => {
    if (_refreshTimer != null) {
      clearInterval(_refreshTimer);
    }
  });

  if (window.location.pathname == "/message.html") {
    _load();
  }

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get header elements
    _stickyHeaderElement = document.querySelector(".sticky-header");
    _titleUsernameElement = document.getElementById("title-username");

    // Get main content element
    _mainContentElement = document.getElementById("main-content");

    // Get top message elements
    _topMessageElement = document.getElementById("top-message");
    _topTitleElement = document.getElementById("top-title");
    _topRecipientsElement = document.getElementById("top-recipients");
    _topBodyElement = document.getElementById("top-body");
    _topAttachmentsElement = document.getElementById("top-attachments");
    _topAuthorElement = document.getElementById("top-author");
    _topAgeElement = document.getElementById("top-age");
    _topHaveRepliesElement = document.getElementById("top-have-replies");
    _topRepliesElement = document.getElementById("top-replies");
    _topDeleteElement = document.getElementById("top-delete");
    _topAddReplyElement = document.getElementById("top-add-reply");

    // Get dialog element
    _deleteMessageBodyElement = document.getElementById("delete-message-body");

    // Get replies element
    _repliesElement = document.getElementById("replies");

    // Add DOMContentLoaded event listener to populate page
    // Note: _dataLoaded may be set by _loadData() before DOMContentLoaded triggers (by design)
    document.addEventListener("DOMContentLoaded", () => {
      _domReady = true;
      if (_dataLoaded) {
        _populatePage();
      }
    });

    // Start refresh timer
    _refreshTimer = setInterval(() => _loadData(), _REFRESH_INTERVAL);

    // Dynamically load data
    _loadData();
  }

  async function _loadData() {
    if (_isLoadingData) {
      return;
    }

    _isLoadingData = true;
    try {
      // REST: Read top message and body
      const readMessageAndBlobResult = await MessageLib.readMessageAndBlob(_topMessageId, _userId);
      if (!readMessageAndBlobResult.ok) {
        return;
      }
      if (readMessageAndBlobResult.body != null) {
        ({message: _topMessage, title: _topMessageTitle, body: _topMessageBody} =
         readMessageAndBlobResult);
      } else {
        _topMessageTitle = "<No content>";
        _topMessageBody = "<No content>";
      }

      // Mark parent post as read
      Bespoke.markMessageAsRead(_topMessageId);

      // Remove delete button if not author
      if (_topMessage.authorId !== _userId) {
        _topDeleteElement.style.display = "none";
      }

      // REST: Read reply messages
      if (_topMessage.replyMessageIds.length == 0) {
        _replyMessages = [];
      } else {
        const readReplyMessagesResponse = await fetch("/api/read_reply_messages", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify(_topMessageId)
        });

        if (!readReplyMessagesResponse.ok) {
          if (readReplyMessagesResponse.status == 401) {
            Bespoke.navigateTo("loader.html");
            return;
          } else {
            console.error(`Server error: ${readReplyMessagesResponse.status}`);
            return;
          }
        }
        _replyMessages = await readReplyMessagesResponse.json();
      }

      _dataLoaded = true;

      // Populate page if DOM is ready
      if (_domReady) {
        _populatePage();
      }
    } catch (error) {
      console.error("Loading of data failed:", error);
    } finally {
      _isLoadingData = false;
    }
  }

  async function _populatePage() {
    // Adjust padding of main content
    setTimeout(() => {
      // Timeout is to ensure that header has been rendered :-9
      const headerHeight = _stickyHeaderElement.offsetHeight;
      _mainContentElement.style.paddingTop = headerHeight + "px";
    }, 10);

    document.body.hidden = false;

    // Populate header
    _titleUsernameElement.textContent = _username;

    // Populate top message
    _topMessageElement.setAttribute("data-message-id", _topMessageId);
    _topTitleElement.innerText = _topMessageTitle;
    const usernames = _topMessage.recipients.map(recipient => recipient.username);
    const index = usernames.indexOf(_topMessage.authorUsername);
    if (index > -1) {
      usernames.splice(index, 1);
    }
    _topRecipientsElement.textContent = "[" + usernames.join(", ") + "]";

    // Populate attachments
    const generateAttachmentsResult = await MessageLib.generateAttachments(_userId, _topMessage);
    if (!generateAttachmentsResult.ok) {
      return;
    }
    const attachments = generateAttachmentsResult.attachments;
    if (attachments != "") {
      render(_topAttachmentsElement, html`${attachments}`);
      Bespoke.initLongClick();
    }

    // Populate top message body
    Bespoke.formatMarkdown(_topBodyElement, _topMessageBody);
    _topAuthorElement.textContent = _topMessage.authorUsername;
    _topAgeElement.textContent = Bespoke.formatSecondsSinceEpoch(_topMessage.created);
    if (_topMessage.replyMessageIds.length == 0) {
      _topHaveRepliesElement.hidden = true;
    } else {
      _topHaveRepliesElement.hidden = false;
      _topRepliesElement.textContent = _topMessage.replyMessageIds.length;
    }
    _topDeleteElement.setAttribute("data-message-id", _topMessageId);
    _topAddReplyElement.setAttribute("data-message-id", _topMessageId);

    // Populate replies
    if (_replyMessages.length > 0) {
      // Render replies
      const replyTemplates = await Promise.all(
        _replyMessages.map(async replyMessage => {
          return await _createReplyTemplate(replyMessage);
        }));
      render(_repliesElement, html`${replyTemplates}`);
      Bespoke.refreshAllUIKitIcons();
      Bespoke.disableAllLinks(".reply-body");

      // Add observers to message-dividers
      HasBeenReadDivider.createObservers("message");

      // Scroll to correct position on first load
      if (_firstLoad) {
        console.log("Scrolling to unread message");
        const unreadMessageElement = document.getElementById("unread-message");
        if (unreadMessageElement) {
          unreadMessageElement.scrollIntoView({
            behavior: "smooth",
            block: "start",
            inline: "nearest"
          });
        }
        _firstLoad = false;
      }
    }
  }

  async function _createReplyTemplate(message) {
    // Read message body
    const readBodyResult = await MessageLib.readBody(message.id, _userId);
    if (!readBodyResult.ok) {
      return;
    }
    let messageBody;
    if (readBodyResult.body != null) {
      messageBody = readBodyResult.body;
    } else {
      messageBody = "<No content>";
    }

    // Reply quote
    const replyQuoteActionAttr = `reply-quote-action-${message.id}`;
    const replyQuoteAttr = `reply-quote-${message.id}`;
    const replyQuoteBodyAttr = `reply-quote-body-${message.id}`;
    let parentReplyMessage;
    if (message.parentMessageId == _topMessageId) {
      parentReplyMessage = _topMessage;
    } else {
      parentReplyMessage = _replyMessages.find(
        replyMessage => replyMessage.id == message.parentMessageId
      );
    }
    const replyQuoteAuthor = parentReplyMessage ? parentReplyMessage.authorUsername : "<Unknown>";
    const replyQuote = html`
      <div class="uk-text-meta">
        <div class="quote" onclick=${event => _toggleQuote(event)} uk-tooltip="Quote">
          <span id="${replyQuoteActionAttr}" class="uk-icon-link" uk-icon="chevron-right"></span>
          In reply to ${replyQuoteAuthor}...
        </div>
        <div id="${replyQuoteAttr}"
             class="uk-card uk-card-body uk-card-default uk-card-small uk-text-meta
                    uk-margin-small-bottom quote-card" hidden>
          <div id="${replyQuoteBodyAttr}" class="quote-body">
            <p>Loading...</p>
          </div>
        </div>
      </div>`;

    // Reply body
    const replyBodyAttr = `reply-body-${message.id}`;

    // Author
    const author = html`<span uk-tooltip="Author">${message.authorUsername}</span>`;

    // Age
    const age = html`
      <span uk-tooltip="Created">${Bespoke.formatSecondsSinceEpoch(message.created)}</span>`;

    // Delete button
    let deleteButton = "";
    if (message.authorId === _userId) {
      deleteButton = html`
        <button onclick=${event => openDeleteDialog(event)}
                class="toolbar-button" uk-icon="trash" uk-tooltip="Delete"></button>`;
    }

    // Generate attachments
    const generateAttachmentsResult = await MessageLib.generateAttachments(_userId, message);
    if (!generateAttachmentsResult.ok) {
      return;
    }
    const attachments = generateAttachmentsResult.attachments;

    return html`
      <div data-message-id="${message.id}"
           data-is-read="${message.isRead}"
           data-parent-message-id="${message.parentMessageId}">
        ${replyQuote}
        <div id="${replyBodyAttr}" class="reply-body uk-text-break">
          ${Bespoke.uhtmlFormatMarkdown(messageBody)}
        </div>
        ${attachments}
        <div class="uk-flex uk-flex-between uk-flex-middle">
          <!-- Reply meta-data -->
          <div class="uk-text-meta message-meta-data meta-data">
            <span class="unread" uk-icon="bolt"></span>
            ${author} •
            ${age}
          </div>
          <div>
            ${deleteButton}
            <button onclick=${event => AddReplyMessage.gotoAddReplyPage(event, message.id)}
                    class="toolbar-button" uk-icon="reply" uk-tooltip="Reply"></button>
          </div>
        </div>
        <hr class="uk-margin-small message-divider">
      </div>`;
  }

  function _toggleQuote(event) {
    Bespoke.ignoreEvent(event);

    // Extract current message id
    const messageElement = event.currentTarget.closest("[data-message-id]");
    const messageId = messageElement.getAttribute("data-message-id");

    // Extract current parent message id
    const parentMessageElement = event.currentTarget.closest("[data-parent-message-id]");
    const parentMessageId = parentMessageElement.getAttribute("data-parent-message-id");

    // Toggle visibility of reply quote
    const replyQuoteElement = document.getElementById(`reply-quote-${messageId}`);
    const replyQuoteActionElement = document.getElementById(`reply-quote-action-${messageId}`);
    if (replyQuoteElement.hidden) {
      // Copy parent message body to reply quote body
      const replyQuoteBodyElement = document.getElementById(`reply-quote-body-${messageId}`);
      if (parentMessageId == _topMessageId) {
        replyQuoteBodyElement.innerHTML = _topBodyElement.innerHTML;
      } else {
        const replyBodyElement = document.getElementById(`reply-body-${parentMessageId}`);
        if (replyBodyElement != null) {
          replyQuoteBodyElement.innerHTML = replyBodyElement.innerHTML;
        } else {
          replyQuoteBodyElement.innerHTML = "&lt;Deleted&gt;";
        }
      }

      // Show reply quote
      replyQuoteElement.hidden = false;
      replyQuoteActionElement.setAttribute("uk-icon", "chevron-down");
    } else {
      // Hide reply quote
      replyQuoteElement.hidden = true;
      replyQuoteActionElement.setAttribute("uk-icon", "chevron-right");
    }
  }

  function openDeleteDialog(event) {
    Bespoke.ignoreEvent(event);

    // Extract message to delete
    const messageElement = event.currentTarget.closest("[data-message-id]");
    _messageIdToDelete = Number(messageElement.getAttribute("data-message-id"));

    // Update delete dialog
    const replyMessage = _replyMessages.find(
      replyMessage => replyMessage.id === _messageIdToDelete
    );
    const author = replyMessage != null ? replyMessage.authorUsername : "you";
    _deleteMessageBodyElement.innerHTML =
      `Do you really want to delete this message written by ${author}?`;

    UIkit.modal("#delete-message-dialog").show();
  }

  function deleteMessage(event) {
    const updateServer = async () => {
      try {
        // REST API: Delete message
        const response = await fetch("/api/delete_message", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify(_messageIdToDelete)
        });
        if (!response.ok) {
          if (response.status === 401) {
            Bespoke.navigateTo("loader.html");
            return;
          } else {
            console.error(`Server error: ${response.status}`);
            return;
          }
        }

        // Is it top message itself that has been deleted?
        if (_messageIdToDelete === _topMessageId) {
          Bespoke.gotoPage(event, "top_messages.html");
        } else {
          Bespoke.gotoPage(event, "message.html");
        }

        _messageIdToDelete = null;
      } catch (error) {
        console.error("Deletion of message failed:", error);
      }
    };

    updateServer();
  }

  return {
    deleteMessage,
    openDeleteDialog
  };
})();
