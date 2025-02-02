// -*- fill-column: 100 -*-

"use strict";

const Files = (function () {
  let _stickyHeader;
  let _mainContent;
  let _fileUpload;

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
    _fileUpload = document.getElementById("file-upload");
    // Update header
    const username = Bespoke.getCookieValue("username");
    document.getElementById("title-username").textContent = username;
    setTimeout(() => {
      // The timeout is needed to ensure the header has been rendered :-9
      const headerHeight = _stickyHeader.offsetHeight;
      _mainContent.style.paddingTop = headerHeight + "px";
    }, 100);
    document.body.hidden = false;
    _initFileUpload();
  }

  function _initFileUpload() {
    // Note: XMLHTTPRequest is used instead of fetch(). This makes it possible to keep track of
    // upload progress, but it also ensures that the file is read/sent in chunks from disk.
    _fileUpload.addEventListener("change", e => {
      const file = e.target.files[0];
      if (!file) {
        return
      }
      const xhr = new XMLHttpRequest();
      xhr.open("PUT", "/upload-file");
      xhr.onload = function () {
        if (xhr.status >= 200 && xhr.status < 300) {
          console.log("Upload successful");
        } else {
          console.error("Upload failed:", xhr.statusText);
        }
      };
      xhr.upload.addEventListener("progress", function (e) {
        if (e.lengthComputable) {
          const percentComplete = (e.loaded / e.total) * 100;
          console.log(`Upload progress: ${percentComplete.toFixed(2)}%`);
        }
      });
      xhr.send(file);
    });
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
