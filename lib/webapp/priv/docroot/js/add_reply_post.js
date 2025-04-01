// -*- fill-column: 100 -*-

"use strict";

const AddReplyPost = (function () {
  const _username = Bespoke.getCookieValue("username");

  let _uploadControllers = [];
  let _parentPost;
  let _topPostTitle;

  let _titleUsernameElement;
  let _attachmentCounterElement;
  let _parentTitleElement;
  let _parentAuthorElement;
  let _parentBodyElement;
  let _formBodyElement;
  let _addButtonElement;

  window.addEventListener("beforeunload", () => {
    // Abort still ongoing uploads
    for (const uploadController in _uploadControllers) {
      Bespoke.abortUpload(uploadController);
    }
  });

  Bespoke.onReady("add_reply_post.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get header elements
    _titleUsernameElement = document.getElementById("title-username");
    _attachmentCounterElement = document.getElementById("attachment-counter");

    // Get parent post elements
    _parentTitleElement = document.getElementById("parent-title");
    _parentAuthorElement = document.getElementById("parent-author");
    _parentBodyElement = document.getElementById("parent-body");

    // Get form elements
    _formBodyElement = document.getElementById("form-body");
    _formBodyElement.addEventListener("input", () => _checkFormCompletion());

    // Get add button element
    _addButtonElement = document.getElementById("add-button");

    // Only keep uploaded files if coming back from attachments page
    const previousPage = Bespoke.getPreviousPage();
    if (!previousPage.endsWith("add_attachments.html")) {
      AddAttachments.clearUploadedFiles();
      Bespoke.setRawLocalItem("body", "");
    }

    _updatePage();
    Bespoke.initMobileKeyboardResizing("#form-body");
  }

  function _checkFormCompletion() {
    _addButtonElement.disabled = _formBodyElement.value.trim() === "";
  }

  async function _updatePage() {
    try {
      // Update header
      _titleUsernameElement.textContent = _username;
      _attachmentCounterElement.textContent = AddAttachments.numberOfUploadedFiles();

      // Update form
      const body = Bespoke.getRawLocalItem("body", "");
      _formBodyElement.value = body;

      _checkFormCompletion();
      document.body.hidden = false;
      _formBodyElement.focus();

      // REST: Read parent post
      const readParentPostResult = await PostLib.readPost(Bespoke.peekPostStack().postId);
      if (!readParentPostResult.ok) {
        return;
      }
      _parentPost = readParentPostResult.post;

      // REST: Read top post title (if necessary)
      _topPostTitle = _parentPost.title;
      if (_topPostTitle == null) {
        const readTopPostResult = await PostLib.readPost(_parentPost.topPostId);
        if (!readTopPostResult.ok) {
          return;
        }
        const topPost = readTopPostResult.post;
        _topPostTitle = topPost.title;
      }

      // Update parent post
      _parentTitleElement.innerText = _topPostTitle;
      _parentAuthorElement.textContent = _parentPost.authorUsername;
      Bespoke.formatMarkdown(_parentBodyElement, _parentPost.body);
    } catch (error) {
      console.error("Page update failed:", error);
    }
  }

  function addNow(event) {
    Bespoke.ignoreEvent(event);
    PostLib.createPostAndAttachments(null, _formBodyElement.value, _parentPost.id,
                                     _parentPost.topPostId, _uploadControllers,
                                     () => goBack(event, true));
  }

  function gotoAddAttachments(event) {
    Bespoke.setRawLocalItem("body", _formBodyElement.value);
    Bespoke.gotoPage(event, "add_attachments.html");
  }

  function toggleQuote(event) {
    Bespoke.ignoreEvent(event);
    const quoteCard = document.querySelector(".quote-card");
    const isHidden = quoteCard.hidden;
    const quoteAction = document.getElementById("quote-action");
    if (isHidden) {
      quoteCard.hidden = false;
      quoteAction.setAttribute("uk-icon", "chevron-up");
    } else {
      quoteCard.hidden = true;
      quoteAction.setAttribute("uk-icon", "chevron-down");
    }
  }

  function gotoAddReplyPage(event, postId, popPostStack) {
    Bespoke.setLocalItem("popPostStack", popPostStack);
    Bespoke.gotoPage(event, "add_reply_post.html", postId);
  }

  function goBack(event, ignorePopPostStack) {
    if (!ignorePopPostStack && Bespoke.getLocalItem("popPostStack")) {
      Bespoke.popPostStack();
    }
    Bespoke.setLocalItem("popPostStack", false);
    Bespoke.gotoPage(event, "post.html")
  }

  return {
    addNow,
    gotoAddAttachments,
    toggleQuote,
    gotoAddReplyPage,
    goBack
  };
})();
