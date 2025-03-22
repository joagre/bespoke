// -*- fill-column: 100 -*-

"use strict";

const TopMessages = (function () {
  const {html, render} = uhtml;
  const _REFRESH_INTERVAL = 30000;
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
        console.error(`Server error: ${response.status}`);
        if (response.status === 401) {
          Bespoke.navigateTo("loader.html");
        }
        return;
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
      topMessages.map(async (message) => await _createMessageTemplate(message))
    );
    render(messagesContainer, html`${messageTemplates}`);
    Bespoke.initLongClick();
    _scrollToLastKnownPosition();
  }

  async function _createMessageTemplate(message) {
    // Age
    const age = Bespoke.formatSecondsSinceEpoch(message.created);

    // Replies
    let unread = "";
    if (!message.isRead) {
      unread = html`<span class="unread" uk-icon="bolt"></span>`;
    }
    let replies = html`<span uk-icon="comment"></span> ${message.replyCount}`;

    // Get message title
    const userId = Bespoke.getCookieValue("userId");
    const response = await fetch(`/message/${message.id}/${userId}`);
    if (!response.ok) {
      console.error(`Server error: ${response.status}`);
      if (response.status === 401) {
        Bespoke.navigateTo("loader.html");
      }
      return;
    }

    // Extract message title
    const blob = await response.blob();
    const unblobifiedBlob = await Crypto.unblobifyData(userId, blob);
    const buffer = await unblobifiedBlob.arrayBuffer();
    const view = new DataView(buffer);
    const titleSize = view.getUint16(0, false);
    const decoder = new TextDecoder();
    const title = decoder.decode(new Uint8Array(buffer, 2, titleSize));

    return html`
      <div>
        <div>
          <span onclick=${(event) => {
                  _saveScrollPosition();
                  Bespoke.gotoPage(event, "message.html",
                                   () => Bespoke.setLocalItem("messageId", message.id));
                }}
                class="message-link uk-text-bold">${title}</span>
        </div>
        <div class="message-meta-data uk-text-meta">
          ${unread} ${message.authorUsername} •
          ${age} •
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
