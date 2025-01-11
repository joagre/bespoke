// -*- fill-column: 100; -*-

import bespoke from "/js/bespoke.js";
import addAttachments from "/js/add_attachments.js";

class AddTopPost {
  constructor() {
    bespoke.onReady("add_top_post.html", () => this._load());
  }

  _load() {
    if (!bespoke.hasSessionId()) {
      bespoke.navigateTo("loader.html");
      return;
    }
    this._titleUsername = document.getElementById("title-username");
    this._attachmentCounter = document.getElementById("attachment-counter");
    this._formTitle = document.getElementById("form-title");
    this._formTitle.addEventListener("input", () => this._checkFormCompletion());
    this._formBody = document.getElementById("form-body");
    this._formBody.addEventListener("input", () => this._checkFormCompletion());
    this._addButton = document.getElementById("add-button");
    // Only keep uploaded files if we're coming back from the attachments page
    const previousPage = bespoke.getPreviousPage();
    if (!previousPage.endsWith("add_attachments.html")) {
      addAttachments.clearUploadedFiles();
      bespoke.setRawLocalItem("title", "");
      bespoke.setRawLocalItem("body", "");
    }
    this._updatePage();
    bespoke.initializeMobileKeyboardResizing("#form-body");
  }

  _checkFormCompletion() {
    this._addButton.disabled =
      (this._formTitle.value.trim() === "" || this._formBody.value.trim() === "");
  }

  _updatePage() {
    const username = bespoke.getCookieValue("username");
    this._titleUsername.textContent = username;
    this._attachmentCounter.textContent = addAttachments.numberOfUploadedFiles();
    const title = bespoke.getRawLocalItem("title", "");
    this._formTitle.value = title;
    const body = bespoke.getRawLocalItem("body", "");
    this._formBody.value = body;
    this._checkFormCompletion();
    document.body.hidden = false;
    this._formTitle.focus();
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
          title: this._formTitle.value,
          body: this._formBody.value,
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
        bespoke.navigateTo("top_posts.html");
      } catch (error) {
        console.error("Addition of top post failed:", error);
      }
    };
    updateServer();
  }

  gotoAddAttachments(event) {
    bespoke.setRawLocalItem("title", this._formTitle.value);
    bespoke.setRawLocalItem("body", this._formBody.value);
    bespoke.gotoPage(event, "add_attachments.html");
  }
}

const addTopPost = new AddTopPost();
export default addTopPost
