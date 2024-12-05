import bespoke from "/js/bespoke.js";

class AddReplyPost {
  constructor() {
    bespoke.onReady("add_reply_post.html", () => this._load());
  }

  _load() {
    if (!bespoke.isCookieSet()) {
      bespoke.navigateTo("loader.html");
      return;
    }
    this._formFields = Array.from(
      document.querySelectorAll("#form-body")
    );
    this._formBody = document.getElementById("form-body");
    this._formBody.focus();
    this._addButton = document.getElementById("add-button");
    this._attachEventListeners();
    this._updatePage();
  }

  async _updatePage() {
    try {
      const username = bespoke.getCookieValue("username");
      document.getElementById("title-username").textContent = username;
      // REST: Get parent post
      const response = await fetch("/lookup_posts", {
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
      const result = await response.json();
      bespoke.assert(result.length === 1, "Expected exactly one post");
      this.parentPost = result[0];
      // REST: Get top post title (if necessary)
      this.topPostTitle = this.parentPost["title"];
      if (this.topPostTitle == null) {
        const lookupPostsResponse = await fetch("/lookup_posts", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify([this.parentPost["top-post-id"]]),
        });
        if (!lookupPostsResponse.ok) {
          console.error(`Server error: ${response.status}`);
          return;
        }
        const lookupPostsResult = await lookupPostsResponse.json();
        bespoke.assert(lookupPostsResult.length === 1, "Expected exactly one post");
        const topPost = lookupPostsResult[0];
        this.topPostTitle = topPost["title"];
      }
      this._populatePage();
    } catch (error) {
      console.error("Page update failed:", error);
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

  addNow(event) {
    event.preventDefault();
    const updateServer = async () => {
      try {
        // REST: Add reply post
        const payload = {
          body: document.getElementById("form-body").value,
          "parent-post-id": this.parentPost["id"],
          "top-post-id": (this.parentPost["top-post-id"] != null) ?
            this.parentPost["top-post-id"] : this.parentPost["id"]
        };
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
        this.goBack(event, true);
      } catch (error) {
        console.error("Addition of reply failed:", error);
      }
    };
    updateServer();
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
}

const addReplyPost = new AddReplyPost();
export default addReplyPost
