import bespoke from "/js/bespoke.js";

class AddTopPost {
  constructor() {
    bespoke.onReady("add_top_post.html", () => this._load());
  }

  _load() {
    if (!bespoke.isCookieSet()) {
      bespoke.navigateTo("loader.html");
      return;
    }
    this._formFields = Array.from(
      document.querySelectorAll("#form-title, #form-body")
    );
    this._formTitle = document.getElementById("form-title");
    this._formTitle.focus();
    this._addButton = document.getElementById("add-button");
    this._attachEventListeners();
    this._updatePage();
  }

  _attachEventListeners() {
    this._formFields.forEach((field) => {
      field.addEventListener("input", () => this._checkFormCompletion());
    });
  }

  _checkFormCompletion() {
    const allFilled = this._formFields.every(
      (field) => field.value.trim() !== ""
    );
    this._addButton.disabled = !allFilled;
  }

  _updatePage() {
    const username = bespoke.getCookieValue("username");
    document.getElementById("title-username").textContent = username;
  }

  addTopPost(event) {
    event.preventDefault();
    const updateServer = async () => {
      try {
        const payload = {
          title: document.getElementById("form-title").value,
          body: document.getElementById("form-body").value,
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
          console.error(`Server error: ${response.status}`);
          return;
        }
        bespoke.navigateTo("top_posts.html");
      } catch (error) {
        console.error("Addition of top post failed:", error);
      }
    };
    updateServer();
  }
}

const addTopPost = new AddTopPost();
export default addTopPost
