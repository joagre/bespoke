// -*- fill-column: 100 -*-

"use strict";

const AddReplyPost = (function () {
  let _titleUsername;
  let _attachmentCounter;
  let _parentTitle;
  let _parentAuthor;
  let _parentBody;
  let _formBody;
  let _addButton;
  let _parentPost;
  let _topPostTitle;

  Bespoke.onReady("add_reply_post.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }
    _titleUsername = document.getElementById("title-username");
    _attachmentCounter = document.getElementById("attachment-counter");
    _parentTitle = document.getElementById("parent-title");
    _parentAuthor = document.getElementById("parent-author");
    _parentBody = document.getElementById("parent-body");
    _formBody = document.getElementById("form-body");
    _formBody.addEventListener("input", () => _checkFormCompletion());
    _addButton = document.getElementById("add-button");
    // Only keep uploaded files if we're coming back from the attachments page
    const previousPage = Bespoke.getPreviousPage();
    if (!previousPage.endsWith("add_attachments.html")) {
      AddAttachments.clearUploadedFiles();
      Bespoke.setRawLocalItem("body", "");
    }
    _updatePage();
    Bespoke.initMobileKeyboardResizing("#form-body");
  }

  function _checkFormCompletion() {
    _addButton.disabled = _formBody.value.trim() === "";
  }

  async function _updatePage() {
    try {
      const username = Bespoke.getCookieValue("username");
      _titleUsername.textContent = username;
      document.body.hidden = false;
      _attachmentCounter.textContent = AddAttachments.numberOfUploadedFiles();
      const body = Bespoke.getRawLocalItem("body", "");
      _formBody.value = body;
      _checkFormCompletion();
      // REST: Read parent post
      const response = await fetch("/api/read_posts", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify([Bespoke.peekPostStack().postId]),
      });
      if (!response.ok) {
        if (response.status === 401) {
          Bespoke.navigateTo("loader.html");
        }
        console.error(`Server error: ${response.status}`);
        return;
      }
      const result = await response.json();
      Bespoke.assert(result.length === 1, "Expected exactly one post");
      _parentPost = result[0];
      // REST: Read top post title (if necessary)
      _topPostTitle = _parentPost.title;
      if (_topPostTitle == null) {
        const lookupPostsResponse = await fetch("/api/read_posts", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify([_parentPost.topPostId]),
        });
        if (!lookupPostsResponse.ok) {
          if (lookupPostsResponse.status === 401) {
            Bespoke.navigateTo("loader.html");
          }
          console.error(`Server error: ${response.status}`);
          return;
        }
        const lookupPostsResult = await lookupPostsResponse.json();
        Bespoke.assert(lookupPostsResult.length === 1, "Expected exactly one post");
        const topPost = lookupPostsResult[0];
        _topPostTitle = topPost.title;
      }
      _populatePage();
      _formBody.focus();
    } catch (error) {
      console.error("Page update failed:", error);
    }
  }

  function _populatePage() {
    _parentTitle.innerText = _topPostTitle;
    _parentAuthor.textContent = _parentPost.authorUsername;
    _parentBody.innerHTML = Bespoke.formatMarkdown(_parentPost.body);
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
        body: _formBody.value,
        parentPostId: _parentPost.id,
        topPostId: (_parentPost.topPostId != null) ?
          _parentPost.topPostId : _parentPost.id,
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
      goBack(event, true);
    } catch (error) {
      console.error("Addition of reply post failed:", error);
    }
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

  function gotoAddAttachments(event) {
    Bespoke.setRawLocalItem("body", _formBody.value);
    Bespoke.gotoPage(event, "add_attachments.html");
  }

  return {
    addNow,
    toggleQuote,
    gotoAddReplyPage,
    goBack,
    gotoAddAttachments
  };
})();
