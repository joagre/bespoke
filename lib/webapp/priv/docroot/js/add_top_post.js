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
    this._formTitle = document.getElementById("form-title");
    this._formAttachmentCounter =
      document.getElementById("form-attachment-counter");
    this._formTitle.addEventListener("input",
                                     () => this._checkFormCompletion());
    this._formBody = document.getElementById("form-body");
    this._formBody.addEventListener("input",
                                    () => this._checkFormCompletion());
    this._addButton = document.getElementById("add-button");
    // Only keep uploaded files if we're coming back from the attachments page
    const previousPage = bespoke.getPreviousPage();
    if (previousPage != "add_attachments.html") {
      addAttachments.clearUploadedFiles();
    }
    this._updatePage();
    this._initializeMobileKeyboardResizing();
  }

  _checkFormCompletion() {
    this._addButton.disabled =
      (this._formTitle.value.trim() === "" ||
       this._formBody.value.trim() === "");
  }

  _updatePage() {
    const username = bespoke.getCookieValue("username");
    this._titleUsername.textContent = username;
    this._formAttachmentCounter.textContent =
      addAttachments.numberOfUploadedFiles();
    document.body.hidden = false;
    this._formTitle.focus();
  }

  addNow(event) {
    event.preventDefault();
    const updateServer = async () => {
      try {
        const payload = {
          title: this._formTitle.value,
          body: this._formBody.value,
        };
        // REST: Add top post
        const response = await fetch("/insert_post", {
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

  _initializeMobileKeyboardResizing() {
    // Dynamically resize body when keyboard opens or closes
    if (window.visualViewport) {
      visualViewport.addEventListener("resize", this._resizeBody);
      visualViewport.addEventListener("scroll", this._resizeBody);
    } else {
      window.addEventListener("resize", this._updateHeight);
    }
    this._resizeBody();
    // Block scrolling outside of "form-body" textarea
    document.addEventListener("touchmove", (e) => {
      const el = e.target.closest("#form-body");
      if (el == null) {
        e.preventDefault();
        return;
      }
      const canScrollUp = el.scrollTop > 0;
      const canScrollDown = el.scrollTop + el.clientHeight < el.scrollHeight;
      if (!canScrollUp && !canScrollDown) {
        e.preventDefault();
      }
    }, {passive: false});
  }

  _resizeBody() {
    const newHeight =
          window.visualViewport ?
          window.visualViewport.height : window.innerHeight;
    document.body.style.height = `${newHeight}px`;
  }
}

const addTopPost = new AddTopPost();
export default addTopPost
