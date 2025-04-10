// -*- fill-column: 100 -*-

"use strict";

const TopMessages = (function () {
  const {html, render} = uhtml;
  const _REFRESH_INTERVAL = 30000;
  const _userId = Bespoke.getCookieValue("userId");
  const _username = Bespoke.getCookieValue("username");

  let _refreshTimer = null;
  let _isUpdatingPage = false;

  let _stickyHeaderElement;
  let _titleUsernameElement;
  let _addButtonElement;
  let _mainContentElement;
  let _haveNoMessagesElement;
  let _haveMessagesElement;
  let _messagesElement;

  window.addEventListener("beforeunload", () => {
    if (_refreshTimer != null) {
      clearInterval(_refreshTimer);
    }
  });

  Bespoke.onReady("top_messages.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get header elements
    _stickyHeaderElement = document.querySelector(".sticky-header");
    _titleUsernameElement = document.getElementById("title-username");
    _addButtonElement = document.getElementById("add-button");

    // Get main content elements
    _mainContentElement = document.getElementById("main-content");
    _haveNoMessagesElement = document.getElementById("have-no-messages");
    _haveMessagesElement = document.getElementById("have-messages");
    _messagesElement = document.getElementById("messages");

    // Start refresh timer
    _refreshTimer = setInterval(() => _updatePage(), _REFRESH_INTERVAL);

    // Update header
    _titleUsernameElement.textContent = _username;

    // Adjust padding of main content
    StickyHeader.adjust(_stickyHeaderElement, _mainContentElement);

    document.body.hidden = false;
    _updatePage();
  }

  async function _updatePage() {
    if (_isUpdatingPage) {
      return;
    }

    _isUpdatingPage = true;
    try {
      // REST: Read top messages
      const response = await fetch("/api/read_top_messages");
      if (!response.ok) {
        if (response.status === 401) {
          Bespoke.navigateTo("loader.html");
          return;
        } else {
          console.error(`Server error: ${response.status}`);
          return;
        }
      }

      // Render top messages
      const topMessages = await response.json();
      _renderMessages(topMessages);
    } catch (error) {
      console.error("Page update failed:", error);
    } finally {
      _isUpdatingPage = false;
    }
  }

  async function _renderMessages(topMessages) {
    if (topMessages.length === 0) {
      _haveNoMessagesElement.hidden = false;
      _haveMessagesElement.hidden = true;
      _addButtonElement.classList.add("needs-action");
      return;
    } else {
      _haveNoMessagesElement.hidden = true;
      _haveMessagesElement.hidden = false;
    }
    const messageTemplates = await Promise.all(
      topMessages.map(async message => {
        return await _createMessageTemplate(message);
      }));
    render(_messagesElement, html`${messageTemplates}`);
    _scrollToLastKnownPosition();
  }

  async function _createMessageTemplate(message) {
    // Unread
    let unread = "";
    const replyCount = message.replyMessageIds.length;
    if (!message.isRead || !(message.readCount ==  0 || replyCount == message.readCount)) {
      unread = html`<span class="unread" uk-icon="bolt" uk-tooltip="Unread"></span>`;
    }

    // Author
    const author = html`<span uk-tooltip="Author">${message.authorUsername}</span>`;

    // Age
    const age = html`
      <span uk-tooltip="Created">${Bespoke.formatSecondsSinceEpoch(message.created)}</span>`;

    // Number of attachments
    let numberOfAttachments = "";
    if (message.attachmentIds.length > 0) {
      numberOfAttachments = html`
        • <span uk-tooltip="Attachments">
            <span uk-icon="download"></span>${message.attachmentIds.length}
          </span>`;
    }

    // Replies
    let replies;
    if (replyCount == 0) {
      replies = "";
    } else if (replyCount == message.readCount) {
      replies = html`
        • <span uk-tooltip="Have no unread replies">
            <span uk-icon="comment"></span>
            ${replyCount}
          </span>`;
    } else if (message.readCount == 0) {
      replies = html`
        • <span uk-tooltip="Have replies">
            <span uk-icon="comment"></span>
            ${message.readCount}
            (<span class="unread">${replyCount}</span>)
          </span>`;
    } else {
      replies = html`
        • <span uk-tooltip="Have unread replies">
            <span uk-icon="commenting"></span>
            ${message.readCount}
            (<span class="unread">${replyCount}</span>)
          </span>`;
    }

    // Read message title
    const readMessageAndBlobResult = await MessageLib.readMessageAndBlob(message.id, _userId);
    if (!readMessageAndBlobResult.ok) {
      return;
    }
    let title;
    if (readMessageAndBlobResult.title == null) {
      title = "<No content>";
    } else {
      title = readMessageAndBlobResult.title;
    }

    // Recipients
    const usernames = message.recipients.map(recipient => recipient.username);
    const index = usernames.indexOf(message.authorUsername);
    if (index > -1) {
      usernames.splice(index, 1);
    }
    const recipients = html`[${usernames.join(", ")}]`;

    return html`
      <div>
        <div>
          <span onclick=${event => {
                  _saveScrollPosition();
                  Bespoke.gotoPage(event, "message.html",
                                   () => {
                                     Bespoke.clearLocalItem("parentMessageId");
                                     Bespoke.setLocalItem("topMessageId", message.id);
                                   });
                }}
                class="message-link uk-text-bold">
            ${title}
          </span>
          <div class="uk-text-meta" uk-tooltip="Recipients">
            ${recipients}
          </div>
        </div>
          <div class="message-meta-data uk-text-meta">
           ${unread} ${author} •
           ${age}
           ${numberOfAttachments}
           ${replies}
         </div>
         <hr class="uk-margin-small message-divider">
      </div>`;
  }

  function _scrollToLastKnownPosition() {
    const scrollPosition = Bespoke.getLocalItem("topMessagesScrollPosition", null);
    if (scrollPosition != null) {
      window.scrollTo(scrollPosition.scrollX, scrollPosition.scrollY);
    }
  }

  function _saveScrollPosition() {
    Bespoke.setLocalItem("topMessagesScrollPosition",
                         {scrollX: window.scrollX, scrollY: window.scrollY});
  }

  function clearScrollPosition() {
    Bespoke.clearLocalItem("topMessagesScrollPosition");
  }

  return {
    clearScrollPosition
  };
})();
