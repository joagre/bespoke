// -*- fill-column: 100 -*-

"use strict";

const Index = (function () {
  const _username = Bespoke.getCookieValue("username");

  let _insecureWarningElement;
  let _titleUsernameElement;
  let _forumButtonElement;
  let _inboxButtonElement;
  let _filesButtonElement;
  let _unreadMessagesCounterElement;
  let _unreadMessagesElement;
  let _unreadPostsCounterElement;
  let _unreadPostsElement;

  Bespoke.onReady("index.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get header elements
    _insecureWarningElement = document.getElementById("insecure-warning");
    _titleUsernameElement = document.getElementById("title-username");

    // Intro
    const truthElement = document.getElementById('truth');
    window.addEventListener("load", () => {
      // Wait for the initial fade-in plus a short pause
      setTimeout(() => {
        // Clear old animation so we can restart it
        UIkit.util.removeClass(truthElement, "uk-animation-fade");
        // Force reflow, i.e. browser forgets the old animation state
        void truthElement.offsetWidth; // Magic reflow line
        // Set the new (long) duration *before* we start the next run
        truthElement.style.animationDuration = "2s";
        // Start a new fade with reverse direction
        UIkit.util.addClass(truthElement, "uk-animation-fade uk-animation-reverse");
        // Remove element when the fade-out ends
        truthElement.addEventListener("animationend", () => {
          truthElement.style.display = "none";
        }, { once: true });
      }, 600);   // 400+200ms pause
    });

    // Get button elements
    _forumButtonElement = document.getElementById("forum-button");
    _inboxButtonElement = document.getElementById("inbox-button");
    _filesButtonElement = document.getElementById("files-button");

    // Get footer elements
    _unreadMessagesCounterElement = document.getElementById("unread-messages-counter");
    _unreadMessagesElement = document.getElementById("unread-messages");
    _unreadPostsCounterElement = document.getElementById("unread-posts-counter");
    _unreadPostsElement = document.getElementById("unread-posts");

    _updatePage();
  }

  function _updatePage() {
    // Update header
    _titleUsernameElement.textContent = _username;
    const insecureWarningMessage = localStorage.getItem("insecureWarningMessage");
    if (insecureWarningMessage != null) {
      _insecureWarningElement.textContent = "⚠️";
      const message =
            `You are not protected against a malicious server admin (${insecureWarningMessage})`;
      _insecureWarningElement.setAttribute("uk-tooltip", message);
      UIkit.tooltip(_insecureWarningElement);
      _insecureWarningElement.hidden = false;
    }

    document.body.hidden = false;

    _updateUnreadMessages();
    _updateUnreadPosts();
  }

  async function _updateUnreadMessages() {
    // REST: Read top messages
    let unread = 0;
    const response = await fetch("/api/read_top_messages");
    if (!response.ok) {
      console.error(`Server error: ${response.status}`);
      if (response.status === 401) {
        Bespoke.navigateTo("loader.html");
      }
      return;
    }

    const topMessages = await response.json();
    for (const message of topMessages) {
      if (!message.isRead) {
        unread++;
      }
      if (message.readCount > 0) {
        unread += message.replyMessageIds.length - message.readCount;
      }
    }

    if (unread == 0) {
      _unreadMessagesElement.hidden = true;
    } else {
      _unreadMessagesCounterElement.textContent = unread;
      _unreadMessagesElement.hidden = false;
      _inboxButtonElement.classList.add("needs-action");
    }
  }

  async function _updateUnreadPosts() {
    // REST: Read top posts
    let unread = 0;
    let response = await fetch("/api/read_top_posts");
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

    if (unread == 0) {
      _unreadPostsElement.hidden = true;
    } else {
      _unreadPostsCounterElement.textContent = unread;
      _unreadPostsElement.hidden = false;
      _forumButtonElement.classList.add("needs-action");
    }
  }
})();
