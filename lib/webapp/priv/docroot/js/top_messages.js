// -*- fill-column: 100 -*-

"use strict";

const TopMessages = (function () {
  const {html, render} = uhtml;
  const _REFRESH_INTERVAL = 30000;
  let _refreshTimer = null;
  let _stickyHeader;
  let _mainContent;
  let _haveNoMessages;
  let _haveMessages;
  let _isUpdatingPage = false;

  function init() {
    window.addEventListener("beforeunload", () => {
      if (_refreshTimer != null) {
        clearInterval(_refreshTimer);
      }
    });
    Bespoke.onReady("top_messages.html", () => _load());
  }

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }
    _stickyHeader = document.querySelector(".sticky-header");
    _mainContent = document.getElementById("main-content");
    _haveNoMessages = document.getElementById("have-no-messages");
    _haveMessages = document.getElementById("have-messages");
    _refreshTimer = setInterval(() => _updatePage(), _REFRESH_INTERVAL);
    // Update header
    const username = Bespoke.getCookieValue("username");
    document.getElementById("title-username").textContent = username;
    setTimeout(() => {
      // The timeout is needed to ensure the header has been rendered :-9
      const headerHeight = _stickyHeader.offsetHeight;
      _mainContent.style.paddingTop = headerHeight + "px";
    }, 100);
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

  function _renderMessages(topMessages) {
    const messagesContainer = document.getElementById("messages");
    if (topMessages.length === 0) {
      _haveNoMessages.hidden = false;
      _haveMessages.hidden = true;
      return;
    } else {
      _haveNoMessages.hidden = true;
      _haveMessages.hidden = false;
    }
    const messageTemplates = topMessages.map((message) => _createMessageTemplate(message));
    render(messagesContainer, html`${messageTemplates}`);
    Bespoke.initLongClick();
    _scrollToLastKnownPosition();
  }

  async function _createMessageTemplate(message) {
    // Age
    const age = Bespoke.formatSecondsSinceEpoch(message.created);
    // Replies
    let replies;
    let unread = "";
    if (!message.isRead) {
      unread = html`<span class="unread" uk-icon="bolt"></span>`;
      if (message.replyCount == 0) {
        replies = html`• <span uk-icon="comment"></span> ${message.replyCount}`;
      } else {
        replies =
          html`• <span uk-icon="commenting"></span>
               ${message.readCount} (${message.replyCount})`;
      }
    } else if (message.replyCount == message.readCount) {
      replies = html`• <span uk-icon="comment"></span> ${message.replyCount}`;
    } else {
      if (message.readCount > 0) {
        replies =
          html`• <span uk-icon="commenting"></span>
                 ${message.readCount}
                 (<span class="unread">${message.replyCount}</span>)`;
        unread = html`<span class="unread" uk-icon="bolt"></span>`;
      } else {
        replies =
          html`• <span uk-icon="commenting"></span>
                 ${message.readCount} (${message.replyCount})`;
      }
    }
    // Fetch message title
    const response = await fetch(`/post/${message.id}`);
    if (!response.ok) {
      console.error(`Server error: ${response.status}`);
      if (response.status === 401) {
        Bespoke.navigateTo("loader.html");
      }
      return;
    }
    const buffer = await response.arrayBuffer();
    const view = new DataView(buffer);
    const titleSize = view.getUint16(0, false);
    const decoder = new TextDecoder();
    const title = decoder.decode(new Uint8Array(buffer, 2, titleSize));
    //const attachments = generateAttachments(message);
    const attachments = "";
    return html`
      <div>
        <div>
          <span onclick=${(event) => {
                  _saveScrollPosition();
                  Bespoke.gotoPage(event, "message.html")
                }}
                class="message-link uk-text-bold">${title}</span>
        </div>
        ${attachments}
        <div class="message-meta-data uk-text-meta">
          ${unread} ${message.authorUsername} •
          ${age} •
          ${replies}
        </div>
        <hr class="uk-margin-small message-divider">
      </div>`;
  }

  function generateAttachments(post) {
    let attachments = "";
    if (post.attachments.length > 0) {
      const items = [];
      for (const attachment of post.attachments) {
        const absPath = `/post/${post.id}/${attachment.filename}`;
        let item;
        if (attachment.contentType.startsWith("image/")) {
          item = html`<div class="attachment-item long-click" uk-lightbox>
            <a href="${absPath}" data-caption="${attachment.filename}">
              <img class="attachment-content" src="${absPath}"
                   alt="${attachment.filename}">
            </a>
          </div>`;
        } else if (attachment.contentType.startsWith("video/")) {
          item = html`<div class="attachment-item long-click" uk-lightbox>
            <a href="${absPath}" data-caption="${attachment.filename}">
              <img class="attachment-content" src="/images/1x1.png"
                   alt="${attachment.filename}">
              <div class="uk-position-center uk-text-break uk-text-muted attachment-misc">${attachment.filename}</div>
            </a>
          </div>`;
        } else {
          item = html`<div class="attachment-item long-click">
            <a href="${absPath}" data-caption="${attachment.filename}" download>
              <img class="attachment-content" src="/images/1x1.png"
                   alt="${attachment.filename}">
              <div class="uk-position-center uk-text-break uk-text-muted attachment-misc">${attachment.filename}</div>
            </a>
          </div>`;
        }
        items.push(item);
      }
      attachments = html`<div id="attachments" class="uk-position-relative"
                              uk-slider>
        <div class="uk-slider-items">
          ${items}
        </div>
        <a class="uk-slidenav uk-position-small uk-position-center-left uk-overlay uk-overlay-default" uk-slidenav-previous uk-slider-item="previous"></a>
        <a class="uk-slidenav uk-position-small uk-position-center-right uk-overlay uk-overlay-default" uk-slidenav-next uk-slider-item="next"></a>
      </div>`;
    }
    return attachments;
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
    init,
    generateAttachments,
    clearScrollPosition
  };
})();

TopMessages.init();
