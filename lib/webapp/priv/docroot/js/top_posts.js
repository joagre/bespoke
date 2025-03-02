// -*- fill-column: 100 -*-

"use strict";

const TopPosts = (function () {
  const {html, render} = uhtml;
  const _REFRESH_INTERVAL = 30000;
  let _refreshTimer = null;
  let _stickyHeader;
  let _mainContent;
  let _haveNoPosts;
  let _havePosts;
  let _isUpdatingPage = false;

  function init() {
    window.addEventListener("beforeunload", () => {
      if (_refreshTimer != null) {
        clearInterval(_refreshTimer);
      }
    });
    Bespoke.onReady("top_posts.html", () => _load());
  }

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }
    _stickyHeader = document.querySelector(".sticky-header");
    _mainContent = document.getElementById("main-content");
    _haveNoPosts = document.getElementById("have-no-posts");
    _havePosts = document.getElementById("have-posts");
    Bespoke.clearPostStack();
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
      // REST: Read top posts
      const response = await fetch("/api/read_top_posts");
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        if (response.status === 401) {
          Bespoke.navigateTo("loader.html");
        }
        return;
      }
      const topPosts = await response.json();
      _renderPosts(topPosts);
      const postIds = topPosts.map((post) => post.id);
      Bespoke.subscribeOnChanges(postIds, () => _updatePage());
    } catch (error) {
      console.error("Page update failed:", error);
    } finally {
      _isUpdatingPage = false;
    }
  }

  function _renderPosts(topPosts) {
    const postsContainer = document.getElementById("posts");
    if (topPosts.length === 0) {
      _haveNoPosts.hidden = false;
      _havePosts.hidden = true;
      return;
    } else {
      _haveNoPosts.hidden = true;
      _havePosts.hidden = false;
    }
    const postTemplates = topPosts.map((post) => _createPostTemplate(post));
    render(postsContainer, html`${postTemplates}`);
    Bespoke.initLongClick();
    _scrollToLastKnownPosition();
  }

  function _createPostTemplate(post) {
    // Age
    const age = Bespoke.formatSecondsSinceEpoch(post.created);
    // Like
    let likeAttr = "";
    if (post.likers.includes(Bespoke.getCookieValue("userId"))) {
      likeAttr = "bleeding-heart";
    }
    // Replies
    let replies;
    let unread = "";
    if (!post.isRead) {
      unread = html`<span class="unread" uk-icon="bolt"></span>`;
      if (post.replyCount == 0) {
        replies = html`• <span uk-icon="comment"></span> ${post.replyCount}`;
      } else {
        replies =
          html`• <span uk-icon="commenting"></span>
               ${post.readCount} (${post.replyCount})`;
      }
    } else if (post.replyCount == post.readCount) {
      replies = html`• <span uk-icon="comment"></span> ${post.replyCount}`;
    } else {
      if (post.readCount > 0) {
        replies =
          html`• <span uk-icon="commenting"></span>
                 ${post.readCount}
                 (<span class="unread">${post.replyCount}</span>)`;
        unread = html`<span class="unread" uk-icon="bolt"></span>`;
      } else {
        replies =
          html`• <span uk-icon="commenting"></span>
                 ${post.readCount} (${post.replyCount})`;
      }
    }
    const attachments = generateAttachments(post);
    return html`
      <div>
        <div>
          <span onclick=${(event) => {
                  _saveScrollPosition();
                  Bespoke.gotoPage(event, "post.html", post.id)
                }}
                class="post-link uk-text-bold">${post.title}</span>
        </div>
        ${attachments}
        <div class="post-meta-data uk-text-meta">
          ${unread} ${post.authorUsername} •
          ${age} •
          <span class="${likeAttr}" uk-icon="icon: heart"></span>
          ${post.likers.length}
          ${replies}
        </div>
        <hr class="uk-margin-small post-divider">
      </div>`;
  }

  function generateAttachments(post) {
    let attachments = "";
    if (post.attachments.length > 0) {
      const items = [];
      for (const attachment of post.attachments) {
        const absPath = `/attachment/${post.id}/${attachment.filename}`;
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
    const scrollPosition = Bespoke.getLocalItem("topPostsScrollPosition", null);
    if (scrollPosition != null) {
      window.scrollTo(scrollPosition.scrollX, scrollPosition.scrollY);
    }
  }

  function _saveScrollPosition() {
    Bespoke.setLocalItem("topPostsScrollPosition",
                         {scrollX: window.scrollX, scrollY: window.scrollY});
  }

  function clearScrollPosition() {
    Bespoke.clearLocalItem("topPostsScrollPosition");
  }

  return {
    init,
    generateAttachments,
    clearScrollPosition
  };
})();

TopPosts.init();
