// -*- fill-column: 100 -*-

"use strict";

const Files = (function () {
  let _stickyHeader;
  let _mainContent;

  function init() {
    Bespoke.onReady("files.html", () => _load());
  }

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }
    _stickyHeader = document.querySelector(".sticky-header");
    _mainContent = document.getElementById("main-content");
    // Update header
    const username = Bespoke.getCookieValue("username");
    document.getElementById("title-username").textContent = username;
    setTimeout(() => {
      // The timeout is needed to ensure the header has been rendered :-9
      const headerHeight = _stickyHeader.offsetHeight;
      _mainContent.style.paddingTop = headerHeight + "px";
    }, 100);
    document.body.hidden = false;
  }

  function copyToClipboard(link) {
    const tempInput = document.createElement("input");
    tempInput.value = link;
    document.body.appendChild(tempInput);
    tempInput.select();
    document.execCommand("copy");
    document.body.removeChild(tempInput);
    Bespoke.showNotification("Link copied to clipboard");
  }

  function toggleMembershipInContacts(username) {
    Bespoke.showNotification(`Toggled membership in contacts for ${username}`);
  }

  return {
    init,
    copyToClipboard,
    toggleMembershipInContacts
  };
})();

Files.init();
