// -*- fill-column: 100 -*-

"use strict";

const Index = (function () {
  let _insecureWarning;
  let _titleUsername;
  let _unreadPostsCounter;
  let _unreadPosts;

  Bespoke.onReady("index.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }
    _insecureWarning = document.getElementById("insecure-warning");
    _titleUsername = document.getElementById("title-username");
    _unreadPostsCounter = document.getElementById("unread-posts-counter");
    _unreadPosts = document.getElementById("unread-posts");
    _updatePage();
  }

  function _updatePage() {
    const username = Bespoke.getCookieValue("username");
    _titleUsername.textContent = username;
    const insecureWarningMessage = localStorage.getItem("insecureWarningMessage");
    if (insecureWarningMessage != null) {
      _insecureWarning.textContent = "⚠️";
      const message = `You are not protected against a malicious server admin (${insecureWarningMessage})`;
      _insecureWarning.setAttribute("uk-tooltip", message);
      console.error(message);
      UIkit.tooltip(_insecureWarning);
      _insecureWarning.hidden = false;
    }
    document.body.hidden = false;
    _updateUnreadForumPosts();
  }

  async function _updateUnreadForumPosts() {
    let unread = 0;
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
    for (const post of topPosts) {
      if (!post.isRead) {
        unread++;
      }
      if (post.readCount > 0) {
        unread += post.replyCount - post.readCount;
      }
    }
    _unreadPostsCounter.textContent = unread;
    _unreadPosts.hidden = unread === 0;
  }

  return {
  };
})();
