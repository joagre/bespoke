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
      AddAttachments.clearFiles();
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

  async function _updatePage() {
    // Update header
    _titleUsernameElement.textContent = _username;
    _attachmentCounterElement.textContent = await AddAttachments.numberOfFiles();

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
    PostLib.createPostAndAttachments(_formTitleElement.value, _formBodyElement.value, null, null,
                                     _uploadControllers,
                                     () => Bespoke.navigateTo("top_posts.html"));
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
