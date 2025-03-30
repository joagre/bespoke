// -*- fill-column: 100 -*-

"use strict";

const TopMessages = (function () {
  const {html, render} = uhtml;
  const _REFRESH_INTERVAL = 30000;
  const _userId = Bespoke.getCookieValue("userId");

  let _refreshTimer = null;
  let _isUpdatingPage = false;
  let _stickyHeaderElement;
  let _titleUsernameElement;
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

    // Get header element
    _stickyHeaderElement = document.querySelector(".sticky-header");
    _titleUsernameElement = document.getElementById("title-username");

    // Get main content elements and start refresh timer
    _mainContentElement = document.getElementById("main-content");
    _haveNoMessagesElement = document.getElementById("have-no-messages");
    _haveMessagesElement = document.getElementById("have-messages");
    _messagesElement = document.getElementById("messages");
    _refreshTimer = setInterval(() => _updatePage(), _REFRESH_INTERVAL);

    // Update header
    const username = Bespoke.getCookieValue("username");
    _titleUsernameElement.textContent = username;

    // Adjust padding of main content
    setTimeout(() => {
      // The timeout is needed to ensure the header has been rendered :-9
      const headerHeight = _stickyHeaderElement.offsetHeight;
      _mainContentElement.style.paddingTop = headerHeight + "px";
    }, 100);

    document.body.hidden = false;

    _updatePage();
  }

  async function _updatePage() {
    if (_isUpdatingPage) {
      return;
    }

    // Render top messages
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
      const topMessages = await response.json();
      _renderMessages(topMessages);
    } catch (error) {
      console.error("Page update failed:", error);
    } finally {
      _isUpdatingPage = false;
    }
  }

  async function _renderMessages(topMessages) {
    const messagesContainer = document.getElementById("messages");
    if (topMessages.length === 0) {
      _haveNoMessagesElement.hidden = false;
      _haveMessagesElement.hidden = true;
      return;
    } else {
      _haveNoMessagesElement.hidden = true;
      _haveMessagesElement.hidden = false;
    }
    const messageTemplates = await Promise.all(
      topMessages.map(async (topMessage) => await _createTopMessageTemplate(topMessage))
    );
    render(messagesContainer, html`${messageTemplates}`);
    Bespoke.initLongClick();
    _scrollToLastKnownPosition();
  }

  async function _createTopMessageTemplate(topMessage) {
    // Age
    const age = Bespoke.formatSecondsSinceEpoch(topMessage.created);

    // Replies
    let replies;
    let unread = [];
    const replyCount = topMessage.replyMessageIds.length;
    if (!topMessage.isRead) {
      unread = html`<span class="unread" uk-icon="bolt"></span>`;
      if (replyCount == 0) {
        replies = html`• <span uk-icon="comment"></span> ${replyCount}`;
      } else {
        replies =
          html`• <span uk-icon="commenting"></span> ${topMessage.readCount} (${replyCount})`;
      }
    } else if (replyCount == topMessage.readCount) {
      replies = html`• <span uk-icon="comment"></span> ${replyCount}`;
    } else {
      if (topMessage.readCount > 0) {
        replies =
          html`• <span uk-icon="commenting"></span>
                 ${topMessage.readCount}
                 (<span class="unread">${replyCount}</span>)`;
        unread = html`<span class="unread" uk-icon="bolt"></span>`;
      } else {
        replies = html`• <span uk-icon="commenting"></span> ${topMessage.readCount} (${replyCount})`;
      }
    }

    // Get message title
    const readMessageAndBodyResult = await MessageLib.readMessageAndBody(topMessage.id, _userId);
    if (!readMessageAndBodyResult.ok) {
      return;
    }
    let title;
    if (readMessageAndBodyResult.title == null) {
      title = "<No content>";
    } else {
      title = readMessageAndBodyResult.title;
    }

    // Generate attachments
    const attachments = await MessageLib.generateAttachments(_userId, topMessage);

    // Recipients
    const usernames = topMessage.recipients.map(recipient => recipient.username);
    const index = usernames.indexOf(topMessage.authorUsername);
    if (index > -1) {
      usernames.splice(index, 1);
    }
    const recipients = html`[${usernames.join(", ")}]`;

    return html`
      <div>
        <div>
          <span onclick=${(event) => {
                  _saveScrollPosition();
                  Bespoke.gotoPage(event, "message.html",
                                   () => {
                                     Bespoke.clearLocalItem("parentMessageId");
                                     Bespoke.setLocalItem("topMessageId", topMessage.id);
                                   });
                }}
                class="message-link uk-text-bold">${title}</span>
          <div class="uk-text-meta">
            ${recipients}
          </div>
        </div>
        ${attachments}
        <div class="message-meta-data uk-text-meta">
          ${unread} ${topMessage.authorUsername} •
          ${age}
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
    clearScrollPosition,
  };
})();
