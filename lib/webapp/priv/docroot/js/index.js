// -*- fill-column: 100 -*-

"use strict";

const Index = (function () {
  const {html, render} = uhtml;
  const _bbsName = Bespoke.getCookieValue("bbsName");
  const _username = Bespoke.getCookieValue("username");

  let _headerTitleElement;
  let _insecureWarningElement;
  let _titleUsernameElement;
  let _menuElement;
  let _logotypeElement;
  let _aboutElement;
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
    _headerTitleElement = document.getElementById("header-title");
    _insecureWarningElement = document.getElementById("insecure-warning");
    _titleUsernameElement = document.getElementById("title-username");

    // Get menu element
    _menuElement = document.getElementById("menu");

    // Get logotype element
    _logotypeElement = document.getElementById("logotype");

    // Get about element
    _aboutElement = document.getElementById("about");

    // Logotype
    window.addEventListener("load", () => {
      // Wait for the initial fade-in plus a short pause
      setTimeout(() => {
        // Clear old animation so we can restart it
        UIkit.util.removeClass(_logotypeElement, "uk-animation-fade");
        // Force reflow, i.e. browser forgets the old animation state
        void _logotypeElement.offsetWidth; // Magic reflow line
        // Set the new (long) duration *before* we start the next run
        _logotypeElement.style.animationDuration = "1s";
        // Start a new fade with reverse direction
        UIkit.util.addClass(_logotypeElement, "uk-animation-fade uk-animation-reverse");
        // Remove element when the fade-out ends
        _logotypeElement.addEventListener("animationend", () => {
          // Show about text
          fetch("/local/about.md")
            .then((response) => {
              if (!response.ok) {
                console.error(`Server error: ${response.status}`);
                if (response.status === 401) {
                  Bespoke.navigateTo("loader.html");
                }
                return;
              }
              return response.text();
            })
            .then((text) => {
              if (text != null) {
                _logotypeElement.parentNode.hidden = true;
                // NOTE: Disabled for now!!!
                //Bespoke.formatMarkdown(_aboutElement, text);
                _aboutElement.hidden = false;
              }
            })
            .catch((error) => {
              console.error("Error fetching about text:", error);
            });
        }, { once: true });
      }, 250);
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
    _headerTitleElement.textContent = _bbsName;
    _titleUsernameElement.textContent = _username;
    const insecureWarningMessage = localStorage.getItem("insecureWarningMessage");
    if (insecureWarningMessage != null) {
      _insecureWarningElement.textContent = "⚠️";
      const message =
            `You are not protected against a malicious server admin (${insecureWarningMessage})`;
      _insecureWarningElement.setAttribute("title", message);
      UIkit.tooltip(_insecureWarningElement);
      _insecureWarningElement.hidden = false;
    }

    // Update menu
    const baseMenu = html`
      <li><a href="javascript:Bespoke.gotoPage(event, 'switch_user.html')"><span class="uk-margin-small-right" uk-icon="icon: users"></span>Switch user</a></li>
      <li><a href="javascript:Bespoke.gotoPage(event, 'change_password.html')"><span class="uk-margin-small-right" uk-icon="icon: user"></span>Change password</a></li>
      <li><a href="javascript:Bespoke.gotoPage(event, 'about.html')"><span class="uk-margin-small-right" uk-icon="icon: info"></span>About</a></li>`;
    let adminMenu = [];
    if (_username == "admin") {
      // Add <li> last to the admin menu using uhtml
      adminMenu = html`
        <li class="uk-nav-divider"></li>
        <li><a href="javascript:Bespoke.gotoPage(event, 'settings.html')"><span class="uk-margin-small-right" uk-icon="icon: settings"></span>Admin settings</a></li>`;
    }
    render(_menuElement, html`${baseMenu}${adminMenu}`);

    document.body.hidden = false;

    // Uppdate footer
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
