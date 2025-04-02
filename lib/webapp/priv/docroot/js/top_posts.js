// -*- fill-column: 100 -*-

"use strict";

const TopPosts = (function () {
  const {html, render} = uhtml;
  const _REFRESH_INTERVAL = 30000;
  const _userId = Bespoke.getCookieValue("userId");
  const _username = Bespoke.getCookieValue("username");

  let _refreshTimer = null;
  let _isUpdatingPage = false;

  let _stickyHeaderElement;
  let _titleUsernameElement;
  let _mainContentElement;
  let _haveNoPostsElement;
  let _havePostsElement;

  window.addEventListener("beforeunload", () => {
    if (_refreshTimer != null) {
      clearInterval(_refreshTimer);
    }
  });

  Bespoke.onReady("top_posts.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get header elements
    _stickyHeaderElement = document.querySelector(".sticky-header");
    _titleUsernameElement = document.getElementById("title-username");

    // Get main content elements
    _mainContentElement = document.getElementById("main-content");
    _haveNoPostsElement = document.getElementById("have-no-posts");
    _havePostsElement = document.getElementById("have-posts");

    Bespoke.clearPostStack();

    // Start refresh timer
    _refreshTimer = setInterval(() => _updatePage(), _REFRESH_INTERVAL);

    // Update header
    _titleUsernameElement.textContent = _username;

    // Adjust padding of main content
    setTimeout(() => {
      // The timeout is needed to ensure that the header has been rendered :-9
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

    _isUpdatingPage = true;
    try {
      // REST: Read top posts
      const response = await fetch("/api/read_top_posts");
      if (!response.ok) {
        if (response.status === 401) {
          Bespoke.navigateTo("loader.html");
          return;
        } else {
          console.error(`Server error: ${response.status}`);
          return;
        }
      }

      // Render top posts
      const topPosts = await response.json();
      _renderPosts(topPosts);

      // Subscribe on changes
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
      _haveNoPostsElement.hidden = false;
      _havePostsElement.hidden = true;
      return;
    } else {
      _haveNoPostsElement.hidden = true;
      _havePostsElement.hidden = false;
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

    // Attachments
    const attachments = PostLib.generateAttachments(post);

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
    clearScrollPosition
  };
})();
