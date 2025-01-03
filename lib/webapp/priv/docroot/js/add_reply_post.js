import bespoke from "/js/bespoke.js";

class AddReplyPost {
  constructor() {
    bespoke.onReady("add_reply_post.html", () => this._load());
  }

  _load() {
    if (!bespoke.hasSessionId()) {
      bespoke.navigateTo("loader.html");
      return;
    }
    this._formBody = document.getElementById("form-body");
    this._formBody.addEventListener("input",
                                    () => this._checkFormCompletion());
    this._addButton = document.getElementById("add-button");
    this._updatePage();
  }

  _checkFormCompletion() {
    this._addButton.disabled = this._formBody.value.trim() === "";
  }

  async _updatePage() {
    try {
      const username = bespoke.getCookieValue("username");
      document.getElementById("title-username").textContent = username;
      document.body.hidden = false;
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
      this._parentPost = result[0];
      // REST: Get top post title (if necessary)
      this.topPostTitle = this._parentPost.title;
      if (this.topPostTitle == null) {
        const lookupPostsResponse = await fetch("/lookup_posts", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify([this._parentPost.topPostId]),
        });
        if (!lookupPostsResponse.ok) {
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
    document.getElementById("parent-post")
      .setAttribute("data-post-id", this._parentPost.id);
    document.getElementById("parent-title").innerHTML = this.topPostTitle;
    document.getElementById("parent-body").innerHTML = bespoke.formatMarkdown(
      this._parentPost.body
    );
    document.getElementById("parent-author").textContent =
      this._parentPost.author;
    document.getElementById("parent-age").textContent =
      bespoke.formatSecondsSinceEpoch(this._parentPost.created);
    if (this._parentPost.likers.includes(bespoke.getCookieValue("userId"))) {
      document.getElementById("parent-like-icon").classList.add("bleeding-heart");
    }
    document.getElementById("parent-likes-count").textContent =
      this._parentPost.likers.length;
    document.getElementById("parent-replies").textContent =
      this._parentPost.replyCount;
  }

  addNow(event) {
    event.preventDefault();
    const updateServer = async () => {
      try {
        // REST: Add reply post
        const payload = {
          body: document.getElementById("form-body").value,
          "parentPostId": this._parentPost.id,
          "topPostId": (this._parentPost.topPostId != null) ?
            this._parentPost.topPostId : this._parentPost.id
        };
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
