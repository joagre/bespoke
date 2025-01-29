// -*- fill-column: 100 -*-

"use strict";

const Index = (function () {
  let _insecureWarning;
  let _titleUsername;
  let _unreadPostsCounter;
  let _unreadPosts;

  function init() {
    /*
    if (window.location.hostname !== "localhost" &&
        !window.location.hostname.endsWith(".b3s.zone") &&
        !_isIPv4Address(window.location.hostname)) {
      const redirect = async () => {
        // REST: Get host
        const response = await fetch("/api/get_host");
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          return;
        }
        const host = await response.text();
        const targetUrl = `https://${host}.b3s.zone${window.location.pathname}`;
        window.location.href = targetUrl;
        return;
      }
      redirect();
      return;
      }
    */
    Bespoke.onReady("index.html", () => _load());
  }

  function _isIPv4Address(string) {
    const IPv4Regex =
          /^(25[0-5]|(2[0-4]|1\d|[1-9])?\d)\.(25[0-5]|(2[0-4]|1\d|[1-9])?\d)\.(25[0-5]|(2[0-4]|1\d|[1-9])?\d)\.(25[0-5]|(2[0-4]|1\d|[1-9])?\d)$/;
    return IPv4Regex.test(string);
  }

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
      UIkit.tooltip(_insecureWarning);
      _insecureWarning.hidden = false;
    }
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
    _unreadPostsCounter.textContent = unread;
    _unreadPosts.hidden = unread === 0;
  }

  return {
      init
  };
})();

Index.init();
