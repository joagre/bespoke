// -*- fill-column: 100 -*-

"use strict";

const Message = (function () {
  const {html, render} = uhtml;
  const _REFRESH_INTERVAL = 30000;
  const _userId = Bespoke.getCookieValue("userId");
  const _username = Bespoke.getCookieValue("username");

  let _isLoadingData = false;
  let _firstLoad = true;
  let _dataLoaded = false;
  let _domReady = false;
  let _refreshTimer = null;

  let _parentMessage;
  let _parentMessageTitle = null;
  let _parentMessageBody = null;
  let _replyMessages;
  let _messageIdToDelete;
  let _stickyHeaderElement;
  let _titleUsernameElement;
  let _mainContentElement;
  let _parentMessageElement;
  let _parentTitleElement;
  let _parentRecipientsElement;
  let _parentBodyElement;
  let _parentAttachmentsElement;
  let _parentAuthorElement;
  let _parentAgeElement;
  let _parentRepliesElement;
  let _parentDeleteElement;
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

    // Get parent message elements
    _parentMessageElement = document.getElementById("parent-message");
    _parentTitleElement = document.getElementById("parent-title");
    _parentRecipientsElement = document.getElementById("parent-recipients");
    _parentBodyElement = document.getElementById("parent-body");
    _parentAttachmentsElement = document.getElementById("parent-attachments");
    _parentAuthorElement = document.getElementById("parent-author");
    _parentAgeElement = document.getElementById("parent-age");
    _parentRepliesElement = document.getElementById("parent-replies");
    _parentDeleteElement = document.getElementById("parent-delete");

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

    // Start page refresh interval timer
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
      // REST: Read top message
      const response = await fetch("/api/read_messages", {
        method: "POST",
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify([Bespoke.getLocalItem("topMessageId")])
      });
      if (!response.ok) {
        if (response.status == 401) {
          Bespoke.navigateTo("loader.html");
          return;
        } else {
          console.error(`Server error: ${response.status}`);
          return;
        }
      }
      const result = await response.json();
      Bespoke.assert(result.length === 1, "Expected exactly one message");
      _parentMessage = result[0];
      Bespoke.markMessageAsRead(_parentMessage.id);

      // Fetch message body
      const bodyResponse = await fetch(`/message/${_parentMessage.id}/${_userId}`);
      let noContent = false;
      if (!bodyResponse.ok) {
        if (response.status == 404) {
          noContent = true;
        } else if (bodyResponse.status === 401) {
          Bespoke.navigateTo("loader.html");
          return;
        } else {
          console.error(`Server error: ${bodyResponse.status}`);
          return;
        }
      }

      // Parse message body
      if (!noContent) {
        ({title: _parentMessageTitle, body: _parentMessageBody} =
         await TopMessages.parseMessageBlob(await bodyResponse.blob(), _userId));
      } else {
        _parentMessageTitle = "<No content>";
        _parentMessageBody = "<No content>";
      }

      // Remove delete button if not author
      if (_parentMessage.authorId !== _userId) {
        _parentDeleteElement.style.display = "none";
      }

      // REST: Read reply messages
      if (_parentMessage.replyMessageIds.length == 0) {
        _replyMessages = [];
      } else {
        const readReplyMessagesResponse = await fetch("/api/read_reply_messages", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify(_parentMessage.id)
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
    }, 100);

    // Populate header
    _titleUsernameElement.textContent = _username;

    // Populate parent message
    _parentMessageElement.setAttribute("data-message-id", _parentMessage.id);
    _parentTitleElement.innerText = _parentMessageTitle;
    const usernames = _parentMessage.recipients.map(recipient => recipient.username);
    const index = usernames.indexOf(_parentMessage.authorUsername);
    if (index > -1) {
      usernames.splice(index, 1);
    }
    _parentRecipientsElement.textContent = "[" + usernames.join(", ") + "]";
    const attachments = await TopMessages.generateAttachments(_userId, _parentMessage);
    if (attachments != "") {
      render(_parentAttachmentsElement, html`${attachments}`);
      Bespoke.initLongClick();
    }
    Bespoke.formatMarkdown(_parentBodyElement, _parentMessageBody);
    _parentAuthorElement.textContent = _parentMessage.authorUsername;
    _parentAgeElement.textContent = Bespoke.formatSecondsSinceEpoch(_parentMessage.created);
    _parentRepliesElement.textContent = _parentMessage.replyMessageIds.length;
    _parentDeleteElement.setAttribute("data-message-id", _parentMessage.id);

    document.body.hidden = false;

    // Populate replies
    if (_replyMessages.length > 0) {
      // Render replies
      const replyTemplates = await Promise.all(
        _replyMessages.map(replyMessage =>
          _createReplyTemplate(_parentMessage, replyMessage, _replyMessages)
        )
      );
      render(_repliesElement, html`${replyTemplates}`);
      Bespoke.refreshAllUIKitIcons();
      Bespoke.disableAllLinks(".reply-body");

      // Add observers to message-dividers
      _addHasBeenReadObservers();

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

  async function _createReplyTemplate(parentMessage, message, replyMessages) {
    // Fetch message body
    const bodyResponse = await fetch(`/message/${message.id}/${_userId}`);
    let noContent = false;
    if (!bodyResponse.ok) {
      if (bodyResponse.status == 404) {
        noContent = true;
      } else if (bodyResponse.status === 401) {
        Bespoke.navigateTo("loader.html");
        return;
      } else {
        console.error(`Server error: ${bodyResponse.status}`);
        return;
      }
    }

    // Parse message body
    let messageBody;
    if (!noContent) {
      ({body: messageBody} =
       await TopMessages.parseMessageBlob(await bodyResponse.blob(), _userId));
    } else {
      messageBody = "<No content>";
    }

    // Reply body
    const replyBodyAttr = `reply-body-${message.id}`;

    // Age
    const age = Bespoke.formatSecondsSinceEpoch(message.created);

    // Delete button
    let deleteButton = "";
    if (message.authorId === _userId) {
      deleteButton = html`
        <button onclick=${(event) => openDeleteDialog(event)}
                class="toolbar-button" uk-icon="trash" uk-tooltip="Delete"></button>`;
    }

    // Generate attachments
    const attachments = await TopMessages.generateAttachments(_userId, message);

    return html`
      <div data-message-id="${message.id}" data-is-read="${message.isRead}">
        <!-- Reply body -->
        <div id="${replyBodyAttr}" class="reply-body uk-text-break">
          ${Bespoke.uhtmlFormatMarkdown(messageBody)}
        </div>
        ${attachments}
        <div class="uk-flex uk-flex-between uk-flex-middle">
          <!-- Reply meta-data -->
          <div class="uk-text-meta meta-data">
            <span class="unread" uk-icon="bolt"></span>
            ${message.authorUsername} •
            ${age}
          </div>
          <!-- Buttons -->
          <div>
            ${deleteButton}
          </div>
        </div>
        <hr class="uk-margin-small message-divider">
      </div>`;
  }

  function _addHasBeenReadObservers() {
    // Add an observer to each Element that has class "message-divider"
    const messageDividerElements = document.getElementsByClassName("message-divider");
    let firstUnreadMessageFound = false;
    let messageElement = null;
    for (const messageDividerElement of messageDividerElements) {
      messageElement = messageDividerElement.closest("[data-message-id]");
      const messageId = Number(messageElement.getAttribute("data-message-id"));
      const isRead = messageElement.getAttribute("data-is-read") == "true";
      if (!isRead) {
        // Add observer to unread message
        _addHasBeenReadObserver(messageDividerElement);
        // Mark first unread message
        if (!firstUnreadMessageFound) {
          console.log(`${messageId} is first unread message`);
          messageElement.setAttribute("id", "unread-message");
          firstUnreadMessageFound = true;
        }
      } else {
        // Mark it as read in UI
        const metaDataElement = messageElement.querySelector(".meta-data");
        const hasBeenReadElement = metaDataElement.children[0];
        hasBeenReadElement.hidden = true;
      }
    }
    // If all messages are read, mark last message as unread to make sure that last message is shown
    if (!firstUnreadMessageFound && messageElement != null) {
      const messageId = Number(messageElement.getAttribute("data-message-id"));
      console.log(`${messageId} is last message`);
      messageElement.setAttribute("id", "unread-message");
    }
  }

  function _addHasBeenReadObserver(messageDividerElement) {
    const options = {
      root: null, // Uses viewport as root
      rootMargin: "0px", // No margin adjustments; full viewport
      threshold: 0 // Trigger when any part is visible
    };
    const callback = (entries, observer) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          const messageElement = messageDividerElement.closest("[data-message-id]");
          const messageId = Number(messageElement.getAttribute("data-message-id"));
          console.log(`${messageId} has been read`);
          Bespoke.markMessageAsRead(messageId);
          observer.unobserve(entry.target);
        }
      });
    };
    const observer = new IntersectionObserver(callback, options);
    observer.observe(messageDividerElement);
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
    const author = replyMessage != null ? replyMessage.authorUsername : "this message";
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

        // Is it parent message itself that has been deleted?
        if (_messageIdToDelete === _parentMessage.id) {
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
