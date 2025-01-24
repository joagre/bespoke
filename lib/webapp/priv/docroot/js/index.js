// -*- fill-column: 100 -*-

"use strict";

const Index = (function () {
  function init() {
    Bespoke.onReady("index.html", () => _load());
  }

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }
    _updatePage();
  }

  function _updatePage() {
    const username = Bespoke.getCookieValue("username");
    document.getElementById("title-username").textContent = username;
    document.body.hidden = false;
    _updateUnreadForumPosts();
  }

  async function _updateUnreadForumPosts() {
    let unread = 0;
    // REST: Get top posts
    const response = await fetch("/api/list_top_posts");
    if (!response.ok) {
      console.error(`Server error: ${response.status}`);
      if (response.status === 401) {
        Bespoke.navigateTo("loader.html");
      }
      return;
    }
    const topPosts = await response.json();
    for (const post of topPosts) {
      if (!post.isRead) {
        unread++;
      }
      if (post.readCount > 0) {
        unread += post.replyCount - post.readCount;
      }
    }
    document.getElementById("unread-posts-counter").textContent = unread;
    document.getElementById("unread-posts").hidden = unread === 0;
  }

  return {
      init
  };
})();

Index.init();
