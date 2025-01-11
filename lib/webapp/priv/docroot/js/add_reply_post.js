// -*- fill-column: 100; -*-

import bespoke from "/js/bespoke.js";
import addAttachments from "/js/add_attachments.js";

class AddReplyPost {
  constructor() {
    bespoke.onReady("add_reply_post.html", () => this._load());
  }

  _load() {
    if (!bespoke.hasSessionId()) {
      bespoke.navigateTo("loader.html");
      return;
    }
    this._titleUsername = document.getElementById("title-username");
    this._attachmentCounter = document.getElementById("attachment-counter");
    this._parentTitle = document.getElementById("parent-title");
    this._parentAuthor = document.getElementById("parent-author");
    this._parentBody = document.getElementById("parent-body");
    this._formBody = document.getElementById("form-body");
    this._formBody.addEventListener("input", () => this._checkFormCompletion());
    this._addButton = document.getElementById("add-button");
    // Only keep uploaded files if we're coming back from the attachments page
    const previousPage = bespoke.getPreviousPage();
    if (!previousPage.endsWith("add_attachments.html")) {
      addAttachments.clearUploadedFiles();
      bespoke.setRawLocalItem("body", "");
    }
    this._updatePage();
    bespoke.initializeMobileKeyboardResizing("#form-body");
  }

  _checkFormCompletion() {
    this._addButton.disabled = this._formBody.value.trim() === "";
  }

  async _updatePage() {
    try {
      const username = bespoke.getCookieValue("username");
      this._titleUsername.textContent = username;
      document.body.hidden = false;
      this._attachmentCounter.textContent = addAttachments.numberOfUploadedFiles();
      const body = bespoke.getRawLocalItem("body", "");
      this._formBody.value = body;
      this._checkFormCompletion();
      // REST: Get parent post
      const response = await fetch("/api/lookup_posts", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify([bespoke.peekPostStack().postId]),
      });
      if (!response.ok) {
        if (response.status === 401) {
          bespoke.navigateTo("loader.html");
        }
        console.error(`Server error: ${response.status}`);
        return;
      }
      const result = await response.json();
      bespoke.assert(result.length === 1, "Expected exactly one post");
      this._parentPost = result[0];
      // REST: Get top post title (if necessary)
      this.topPostTitle = this._parentPost.title;
      if (this.topPostTitle == null) {
        const lookupPostsResponse = await fetch("/api/lookup_posts", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify([this._parentPost.topPostId]),
        });
        if (!lookupPostsResponse.ok) {
          if (lookupPostsResponse.status === 401) {
            bespoke.navigateTo("loader.html");
          }
          console.error(`Server error: ${response.status}`);
          return;
        }
        const lookupPostsResult = await lookupPostsResponse.json();
        bespoke.assert(lookupPostsResult.length === 1, "Expected exactly one post");
        const topPost = lookupPostsResult[0];
        this.topPostTitle = topPost.title;
      }
      this._populatePage();
      this._formBody.focus();
    } catch (error) {
      console.error("Page update failed:", error);
    }
  }

  _populatePage() {
    this._parentTitle.innerHTML = this.topPostTitle;
    this._parentAuthor.textContent = this._parentPost.author;
    this._parentBody.innerHTML = bespoke.formatMarkdown(this._parentPost.body);
  }

  addNow(event) {
    bespoke.ignoreEvent(event);
    const updateServer = async () => {
      const uploadedFiles = addAttachments.getUploadedFiles();
      const attachments =
            uploadedFiles.map(file => ({
              filename: file.absPath.replace("/tmp/", ""),
              contentType: file.contentType,
            }));
      try {
        const payload = {
          body: this._formBody.value,
          parentPostId: this._parentPost.id,
          topPostId: (this._parentPost.topPostId != null) ?
            this._parentPost.topPostId : this._parentPost.id,
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
            bespoke.navigateTo("loader.html");
          } else {
            console.error(`Server error: ${response.status}`);
          }
          return;
        }
        this.goBack(event, true);
      } catch (error) {
        console.error("Addition of reply post failed:", error);
      }
    };
    updateServer();
  }

  toggleQuote(event) {
    bespoke.ignoreEvent(event);
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

  gotoAddReplyPage(event, postId, popPostStack) {
    bespoke.setLocalItem("popPostStack", popPostStack);
    bespoke.gotoPage(event, "add_reply_post.html", postId);
  }

  goBack(event, ignorePopPostStack) {
    if (!ignorePopPostStack && bespoke.getLocalItem("popPostStack")) {
      bespoke.popPostStack();
    }
    bespoke.setLocalItem("popPostStack", false);
    bespoke.gotoPage(event, "post.html")
  }

  gotoAddAttachments(event) {
    bespoke.setRawLocalItem("body", this._formBody.value);
    bespoke.gotoPage(event, "add_attachments.html");
  }
}

const addReplyPost = new AddReplyPost();
export default addReplyPost
