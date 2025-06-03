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
  let _addButtonElement;
  let _mainContentElement;
  let _haveNoPostsElement;
  let _havePostsElement;
  let _postsElement;

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
    _addButtonElement = document.getElementById("add-button");

    // Get main content elements
    _mainContentElement = document.getElementById("main-content");
    _haveNoPostsElement = document.getElementById("have-no-posts");
    _havePostsElement = document.getElementById("have-posts");
    _postsElement = document.getElementById("posts");

    Bespoke.clearPostStack();

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
      await _renderPosts(topPosts);

      // Initialize scroll navigator
      const sections = Array.from(document.querySelectorAll(".post-section"));
      const keyDown = (key, section) => {
        if (key == "Enter") {
          clearScrollPosition();
          Bespoke.gotoPage(event, "add_top_post.html");
        } else if (key === "ArrowRight") {
          const postReplies = section.getAttribute("data-post-replies");
          if (postReplies > 0) {
            const postId = section.getAttribute("data-post-id");
            _saveScrollPosition();
            Bespoke.gotoPage(event, "post.html", postId);
          }
        } else if (key === "ArrowLeft" || key === "Escape") {
          Bespoke.gotoPage(event, "index.html");
        }
      };
      const headerHeight = StickyHeader.getHeaderHeight();
      ScrollNavigator.init({
        sections,
        headerHeight,
        keyDown
      });

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
    if (topPosts.length === 0) {
      _haveNoPostsElement.hidden = false;
      _havePostsElement.hidden = true;
      _addButtonElement.classList.add("needs-action");
      return;
    } else {
      _haveNoPostsElement.hidden = true;
      _havePostsElement.hidden = false;
    }
    const postTemplates = topPosts.map((post) => _createPostTemplate(post));
    render(_postsElement, html`${postTemplates}`);
    _scrollToLastKnownPosition();
  }

  function _createPostTemplate(post) {
    // Unread
    let unread = "";
    if (!post.isRead || !(post.readCount ==  0 || post.replyCount == post.readCount)) {
      unread = html`<span class="unread" uk-icon="bolt" title="Unread"></span>`;
    }

    // Author
    const author = html`<span title="Author">${post.authorUsername}</span>`;

    // Age
    const age = html`
      <span title="Created">${Bespoke.formatSecondsSinceEpoch(post.created)}</span>`;

    // Like
    let like;
    if (post.likers.includes(Bespoke.getCookieValue("userId"))) {
      like = html`<span class="bleeding-heart" uk-icon="icon: heart"></span>`;
    } else {
      like = html`<span uk-icon="icon: heart"></span>`;
    }

    // Replies
    let replies;
    if (post.replyCount == 0) {
      replies = "";
    } else if (post.replyCount == post.readCount) {
      replies = html`
        • <span title="Have no unread replies">
            <span uk-icon="comment"></span>
            ${post.replyCount}
          </span>`;
    } else if (post.readCount == 0) {
      replies = html`
        • <span title="Have replies">
            <span uk-icon="comment"></span>
            ${post.readCount}
            (<span class="unread">${post.replyCount}</span>)
          </span>`;
    } else {
      replies = html`
        • <span title="Have unread replies">
            <span uk-icon="commenting"></span>
            ${post.readCount}
            (<span class="unread">${post.replyCount}</span>)
          </span>`;
    }

    // Generate attachments
    const attachments = PostLib.generateAttachments(post);

    return html`
      <div class="post-section"
           data-post-id="${post.id}"
           data-post-replies="${post.replyCount}">
        <div>
          <span onclick=${event => {
                            _saveScrollPosition();
                            Bespoke.gotoPage(event, "post.html", post.id);
                          }}
                class="post-link uk-text-bold">
            ${post.title}
          </span>
        </div>
        ${attachments}
        <div class="post-meta-data uk-text-meta">
          ${unread} ${author} •
          ${age} •
          ${like}
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
