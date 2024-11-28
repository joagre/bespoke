import bespoke from "/js/bespoke.js";

// uhtml.min.js must be imported in the HTML file before this script
const { html, render } = uhtml;

class Post {
  constructor() {
    this._dataLoaded = false;
    this._domReady = false;
    this._parentPost = null;
    this._topPostTitle = null;
    this._replyPosts = [];
    this._postIdToDelete = null;

    this.init();
  }

  init() {
    bespoke.initializeCookieState();
    document.addEventListener("DOMContentLoaded", () => {
      bespoke.init();
      this._domReady = true;
      if (this._dataLoaded) {
        this._populatePage();
      }
    });
    this.loadData();
  }

  async loadData() {
    if (!document.cookie.includes("bespoke")) {
      console.log("Cookie not found, retrying in 1 second");
      setTimeout(() => this.loadData(), 1000);
      return;
    }

    try {
      // REST: Get parent post
      let response = await fetch("/lookup_posts", {
        method: "POST",
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify([bespoke.peekPostStack()])
      });
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return;
      }
      const data = await response.json();
      bespoke.assert(data.length === 1, "Expected exactly one post");
      this._parentPost = data[0];

      // REST: Get top post title (maybe)
      this._topPostTitle = this._parentPost["title"];
      if (this._topPostTitle == null) {
        response = await fetch("/lookup_posts", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify([this._parentPost["top-post-id"]])
        });
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          return;
        }
        const topPost = await response.json();
        bespoke.assert(topPost.length === 1,
                        "Expected exactly one post");
        this._topPostTitle = topPost[0]["title"];
      }

      // REST: Get reply posts
      response = await fetch("/lookup_recursive_posts", {
        method: "POST",
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify(this._parentPost["replies"])
      });
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return;
      }
      this._replyPosts = await response.json();
      this._dataLoaded = true;

      if (this._domReady) {
        this._populatePage();
      }
    } catch (error) {
      console.error("Fetching failed:", error);
    }
  }

  _populatePage() {
    const postStackSize = bespoke.postStackSize();

    // Populate head title
    const headTitle = postStackSize > 1 ? "Reply" : "Post";
    document.getElementById("head-title").textContent = headTitle;

    // Populate header
    const headerTitle = postStackSize > 1 ? "Reply" : "Post";
    document.getElementById("header-title").textContent = headerTitle;

    if (postStackSize > 1) {
      document.getElementById(
        "header-reply-level").textContent = `[level: ${postStackSize - 1}]`;
    } else {
      document.getElementById("header-reply-level").style.display = "none";
    }

    // Populate parent post
    document.getElementById("parent-title").innerHTML = this._topPostTitle;
    document.getElementById("parent-body").innerHTML =
      bespoke.formatMarkdown(this._parentPost["body"]);
    document.getElementById("parent-author").textContent =
      this._parentPost["author"];
    document.getElementById("parent-age").textContent =
      bespoke.formatSecondsSinceEpoch(this._parentPost["created"]);
    document.getElementById("parent-replies").textContent =
      this._parentPost["reply-count"];
    document.getElementById("parent-delete")
      .setAttribute("data-post-id", this._parentPost["id"]);

    // Populate replies
    const repliesContainer = document.getElementById("replies");
    const replyTemplates = this._replyPosts.map((replyPost) =>
      this._createReplyTemplate(this._parentPost, replyPost,
                                this._replyPosts)
    );
    render(repliesContainer, html`${replyTemplates}`);
  }

  _createReplyTemplate(parentPost, post, replyPosts) {
    // A reply quote is only added if someone replies to a reply
    let replyQuote = "";
    if (post["parent-post-id"] != null &&
        post["parent-post-id"] != parentPost["id"]) {
      const replyQuoteButtonAttr = `reply-quote-button-${post["id"]}`;
      const replyQuoteAttr = `reply-quote-${post["id"]}`;
      const replyQuoteBodyAttr = `reply-quote-body-${post["id"]}`;
      const parentReplyPost = replyPosts.find(
        (replyPost) => replyPost["id"] == post["parent-post-id"]
      );
      const replyQuoteAuthor =
            parentReplyPost ? parentReplyPost["author"] : "Unknown";
      replyQuote = html`
        <!-- Reply quote -->
        <div
          class="uk-text-meta quote"
          onclick=${(event) => this.toggleQuote(event)}
          data-post-id="${post["id"]}"
          data-parent-post-id="${post["parent-post-id"]}">
          <span id="${replyQuoteButtonAttr}" class="uk-icon-link" uk-icon="chevron-down"></span>
          In reply to ${replyQuoteAuthor}...
          <div
            id="${replyQuoteAttr}"
            class="uk-card uk-card-body uk-card-default uk-card-small uk-text-meta uk-margin-small-bottom uk-margin-small-top custom-quote-padding"
            hidden
          >
            <div id="${replyQuoteBodyAttr}" class="uk-margin-remove-first-child uk-margin-remove-last-child">
              <p>Loading...</p>
            </div>
          </div>
        </div>
      `;
    }

    const age = bespoke.formatSecondsSinceEpoch(post["created"]);
    const replyBodyAttr = `reply-body-${post["id"]}`;
    const replyDividerAttr = `reply-divider-${post["id"]}`;

    let replies = "";
    if (post["reply-count"] > 0) {
      replies = html`•
        <button
          onclick=${(event) => bespoke.gotoPage(event, "post.html", post["id"])}
          class="uk-icon-button"
          uk-icon="comments"></button>
        ${post["reply-count"]}`;
    }

    return html`
      <article class="uk-article uk-margin-remove-top">
        ${replyQuote}
        <!-- Reply body -->
        <div id="${replyBodyAttr}" class="uk-margin-remove-first-child uk-margin-remove-last-child">
          ${bespoke.uhtmlFormatMarkdown(post["body"])}
        </div>
        <!-- Reply meta-data -->
        <div class="uk-article-meta uk-margin-top-remove">
          <div class="uk-flex uk-flex-between uk-flex-middle">
            <div>
              ${post["author"]} • ${age} ${replies}
            </div>
            <div>
              <button
                onclick=${(event) => this.openDeletePostModal(event)}
                data-post-id="${post["id"]}"
                class="uk-icon-button"
                uk-icon="trash"
              ></button>
              <button
                onclick=${(event) => addReplyPost.gotoAddReplyPostPage(event, post["id"], true)}
                class="uk-icon-button"
                uk-icon="reply"
              ></button>
            </div>
          </div>
        </div>
      </article>
      <hr id="${replyDividerAttr}" class="uk-margin-small uk-divider-icon">
    `;
  }

  openDeletePostModal(event) {
    event.stopPropagation();
    // Extract post to delete
    this._postIdToDelete =
      event.currentTarget.getAttribute("data-post-id");
    // Update the post modal body
    const replyPost = this._replyPosts.find(
      (replyPost) => replyPost["id"] === this._postIdToDelete
    );
    const postAuthor = replyPost ? replyPost["author"] : "this post";
    document.getElementById("delete-post-body").innerHTML =
      `Do you really want to delete this post written by ${postAuthor}?`;
    UIkit.modal("#delete-post-modal").show();
  }

  deletePost(event) {
    const postId =
          this._postIdToDelete === bespoke.peekPostStack() ? -1 : null;

    const updateServer = async () => {
      try {
        // REST API: Delete post
        const response = await fetch("/delete_post", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify(this._postIdToDelete)
        });
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          return;
        }
        bespoke.gotoPage(event, "post.html", postId);
        this._postIdToDelete = null;
      } catch (error) {
        console.error("Fetching failed:", error);
      }
    };

    updateServer();
  }

  toggleQuote(event) {
    event.preventDefault();
    const postId = event.currentTarget.getAttribute("data-post-id");
    const parentPostId =
          event.currentTarget.getAttribute("data-parent-post-id");
    const replyQuote = document.getElementById(`reply-quote-${postId}`);
    const isHidden = replyQuote.hidden;
    const replyQuoteButton =
          document.getElementById(`reply-quote-button-${postId}`);

    if (isHidden) {
      const replyQuoteBody =
            document.getElementById(`reply-quote-body-${postId}`);
      const replyBody =
            document.getElementById(`reply-body-${parentPostId}`);
      replyQuoteBody.innerHTML = replyBody.innerHTML;
      replyQuote.hidden = false;
      replyQuoteButton.setAttribute("uk-icon", "chevron-up");
    } else {
      replyQuote.hidden = true;
      replyQuoteButton.setAttribute("uk-icon", "chevron-down");
    }
  }
}

const post = new Post();
export default post;
