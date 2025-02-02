// -*- fill-column: 100 -*-

"use strict";

const AddTopPost = (function () {
  let _titleUsername;
  let _attachmentCounter;
  let _formTitle;
  let _formBody;
  let _addButton;

  function init() {
    Bespoke.onReady("add_top_post.html", () => _load());
  }

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }
    _titleUsername = document.getElementById("title-username");
    _attachmentCounter = document.getElementById("attachment-counter");
    _formTitle = document.getElementById("form-title");
    _formTitle.addEventListener("input", () => _checkFormCompletion());
    _formBody = document.getElementById("form-body");
    _formBody.addEventListener("input", () => _checkFormCompletion());
    _addButton = document.getElementById("add-button");
    // Only keep uploaded files if we're coming back from the attachments page
    const previousPage = Bespoke.getPreviousPage();
    if (!previousPage.endsWith("add_attachments.html")) {
      AddAttachments.clearUploadedFiles();
      Bespoke.setRawLocalItem("title", "");
      Bespoke.setRawLocalItem("body", "");
    }
    _updatePage();
    Bespoke.initMobileKeyboardResizing("#form-body");
  }

  function _checkFormCompletion() {
    _addButton.disabled = (_formTitle.value.trim() === "" || _formBody.value.trim() === "");
  }

  function _updatePage() {
    const username = Bespoke.getCookieValue("username");
    _titleUsername.textContent = username;
    _attachmentCounter.textContent = AddAttachments.numberOfUploadedFiles();
    const title = Bespoke.getRawLocalItem("title", "");
    _formTitle.value = title;
    const body = Bespoke.getRawLocalItem("body", "");
    _formBody.value = body;
    _checkFormCompletion();
    document.body.hidden = false;
    _formTitle.focus();
  }

  function addNow(event) {
    Bespoke.ignoreEvent(event);
    _updateServer();
  }

  async function _updateServer() {
    const uploadedFiles = AddAttachments.getUploadedFiles();
    const attachments =
          uploadedFiles.map(file => ({
            filename: file.absPath.replace("/tmp/", ""),
            contentType: file.contentType,
          }));
    try {
      const payload = {
        title: _formTitle.value,
        body: _formBody.value,
        attachments
      };
      // REST: Add top post
      const response = await fetch("/api/insert_post", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(payload),
      });
      if (!response.ok) {
        if (response.status === 401) {
          Bespoke.navigateTo("loader.html");
        } else {
          console.error(`Server error: ${response.status}`);
        }
        return;
      }
      Bespoke.navigateTo("top_posts.html");
    } catch (error) {
      console.error("Addition of top post failed:", error);
    }
  }
  
  function gotoAddAttachments(event) {
    Bespoke.setRawLocalItem("title", _formTitle.value);
    Bespoke.setRawLocalItem("body", _formBody.value);
    Bespoke.gotoPage(event, "add_attachments.html");
  }
  
  return {
    init,
    addNow,
    gotoAddAttachments
  };
})();

AddTopPost.init();
