// -*- fill-column: 100 -*-

"use strict";

const AddTopPost = (function () {
  const _username = Bespoke.getCookieValue("username");

  let _uploadControllers = [];

  let _titleUsernameElement;
  let _attachmentCounterElement;
  let _formTitleElement;
  let _formBodyElement;
  let _addButtonElement;

  window.addEventListener("beforeunload", () => {
    // Abort still ongoing uploads
    for (const uploadController in _uploadControllers) {
      Bespoke.abortUpload(uploadController);
    }
  });

  Bespoke.onReady("add_top_post.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get header elements
    _titleUsernameElement = document.getElementById("title-username");
    _attachmentCounterElement = document.getElementById("attachment-counter");

    // Get form elements
    _formTitleElement = document.getElementById("form-title");
    _formTitleElement.addEventListener("input", () => _checkFormCompletion());
    _formBodyElement = document.getElementById("form-body");
    _formBodyElement.addEventListener("input", () => _checkFormCompletion());

    // Get add button element
    _addButtonElement = document.getElementById("add-button");

    // Only keep uploaded files if coming back from attachments page
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
    _addButtonElement.disabled = (_formTitleElement.value.trim() === "" ||
                                  _formBodyElement.value.trim() === "");
  }

  function _updatePage() {
    // Update header
    _titleUsernameElement.textContent = _username;
    _attachmentCounterElement.textContent = AddAttachments.numberOfUploadedFiles();

    // Update form
    const title = Bespoke.getRawLocalItem("title", "");
    _formTitleElement.value = title;
    const body = Bespoke.getRawLocalItem("body", "");
    _formBodyElement.value = body;

    _checkFormCompletion();
    document.body.hidden = false;
    _formTitleElement.focus();
  }

  function addNow(event) {
    Bespoke.ignoreEvent(event);
    _updateServer();
  }

  async function _updateServer() {
    // Upload attachments
    function onProgress(total, loaded, percentComplete) {
      Progress.update(total, loaded, percentComplete);
    }
    const files = await AddAttachments.getAllFiles();
    let attachments = [];
    for (const file of files) {
      Progress.show();

      // Upload file
      const uploadController = Bespoke.uploadFile(file.file, onProgress);
      _uploadControllers.push(uploadController);
      const uploadedFile = await Bespoke.waitForUpload(uploadController);
      const index = _uploadControllers.indexOf(uploadController);
      _uploadControllers.splice(index, 1);
      console.log("Attachment has been uploaded:", uploadedFile);

      // Add to attachments variable (used by /api/create_post REST call)
      const attachment = {
        filename: uploadedFile.absPath.replace("/tmp/", ""),
        contentType: uploadedFile.contentType
      }
      attachments.push(attachment);

      Progress.hide();
    }

    // Create top post
    try {
      const payload = {
        title: _formTitleElement.value,
        body: _formBodyElement.value,
        attachments
      };
      // REST: Create top post
      const response = await fetch("/api/create_post", {
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
    Bespoke.setRawLocalItem("title", _formTitleElement.value);
    Bespoke.setRawLocalItem("body", _formBodyElement.value);
    Bespoke.gotoPage(event, "add_attachments.html");
  }

  return {
    addNow,
    gotoAddAttachments
  };
})();
