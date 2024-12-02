import bespoke from "/js/bespoke.js";

class AddReplyPost {
  constructor() {
    bespoke.onReady("/add_reply_post.html", () => this._load());

  }

  _load() {
    this._formFields = Array.from(
      document.querySelectorAll("#form-author, #form-body")
    );
    this._addButton = document.getElementById("add-button");
    this._attachEventListeners();
    this._updatePage();
  }

  async _updatePage() {
    try {
      // Insert username into title
      let username = bespoke.getCookieValue("username");
      document.getElementById("title-username").textContent = username;
      // REST: Get parent post
      let response = await fetch("/lookup_posts", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify([bespoke.peekPostStack().postId]),
      });
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return;
      }
      let data = await response.json();
      bespoke.assert(data.length === 1, "Expected exactly one post");
      this.parentPost = data[0];

      // REST: Get top post title (if necessary)
      this.topPostTitle = this.parentPost["title"];
      if (this.topPostTitle == null) {
        response = await fetch("/lookup_posts", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify([this.parentPost["top-post-id"]]),
        });
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          return;
        }
        data = await response.json();
        bespoke.assert(data.length === 1, "Expected exactly one post");
        const topPost = data[0];
        this.topPostTitle = topPost["title"];
      }

      this._populatePage();
    } catch (error) {
      console.error("Fetching failed:", error);
    }
  }

  _populatePage() {
    document.getElementById("parent-title").innerHTML = this.topPostTitle;
    document.getElementById("parent-body").innerHTML = bespoke.formatMarkdown(
      this.parentPost["body"]
    );
    document.getElementById("parent-author").textContent =
      this.parentPost["author"];
    document.getElementById("parent-age").textContent =
      bespoke.formatSecondsSinceEpoch(this.parentPost["created"]);
    document.getElementById("parent-replies").textContent =
      this.parentPost["reply-count"];
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

  add(event) {
    event.preventDefault();

    const post = {
      author: document.getElementById("form-author").value,
      body: document.getElementById("form-body").value,
      "parent-post-id": this.parentPost["id"],
      "top-post-id": (this.parentPost["top-post-id"] != null) ?
        this.parentPost["top-post-id"] : this.parentPost["id"]
    };

    const updateServer = async () => {
      try {
        // REST: Add reply post
        const response = await fetch("/insert_post", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify(post),
        });
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          return;
        }
        this.goBack(event, true);
      } catch (error) {
        console.error("Fetching failed:", error);
      }
    };

    updateServer();
  }

  gotoAddReplyPage(event, postId, popPostStack) {
    bespoke.setCookieValue("popPostStack", popPostStack);
    bespoke.gotoPage(event, "add_reply_post.html", postId);
  }

  goBack(event, ignorePopPostStack) {
    if (!ignorePopPostStack &&
        bespoke.getCookieValue("popPostStack")) {
      bespoke.popPostStack();
    }
    bespoke.setCookieValue("popPostStack", false);
    bespoke.gotoPage(event, 'post.html')
  }
}

const addReplyPost = new AddReplyPost();
export default addReplyPost
