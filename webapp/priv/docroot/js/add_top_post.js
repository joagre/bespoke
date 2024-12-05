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
    // Insert username into title
    let username = bespoke.getCookieValue("username");
    document.getElementById("title-username").textContent = username;
  }

  addTopPost(event) {
    event.preventDefault();
    const post = {
      title: document.getElementById("form-title").value,
      body: document.getElementById("form-body").value,
    };

    const updateServer = async () => {
      try {
        // REST: Add top post
        const response = await fetch("/insert_post", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify(post),
        });
        if (!response.ok) {
          if (response.status === 403) {
            bespoke.navigateTo("loader.html");
            return;
          }
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
