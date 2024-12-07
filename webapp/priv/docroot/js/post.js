import bespoke from "/js/bespoke.js";

// uhtml.min.js must be imported in the HTML file before this script
const { html, render } = uhtml;

class Post {
  constructor() {
    this._dataLoaded = false;
    this._domReady = false;
    // Note: bespoke.onReady() is not used by design
    this._load();
  }

  _load() {
    if (!bespoke.isCookieSet()) {
      bespoke.navigateTo("loader.html");
      return;
    }
    document.addEventListener("DOMContentLoaded", () => {
      this._domReady = true;
      if (this._dataLoaded) {
        this._populatePage();
      }
    });
    this._loadData();
  }

  async _loadData() {
    try {
      // REST: Get parent post
      const response = await fetch("/lookup_posts", {
        method: "POST",
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify([bespoke.peekPostStack().postId])
      });
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return;
      }
      const result = await response.json();
      bespoke.assert(result.length === 1, "Expected exactly one post");
      this._parentPost = result[0];
      // REST: Get top post title (maybe)
      this._topPostTitle = this._parentPost["title"];
      if (this._topPostTitle == null) {
        const lookupPostsResponse = await fetch("/lookup_posts", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify([this._parentPost["top-post-id"]])
        });
        if (!lookupPostsResponse.ok) {
          console.error(`Server error: ${lookupPostsResponse.status}`);
          return;
        }
        const lookupPostsResult = await lookupPostsResponse.json();
        bespoke.assert(lookupPostsResult.length === 1,
                       "Expected exactly one post");
        this._topPostTitle = lookupPostsResult[0]["title"];
      }
      // Possible remove delete button if not the author
      if (this._parentPost["author"] !== bespoke.getCookieValue("username")) {
        document.getElementById("parent-delete").style.display = "none";
      }
      // REST: Get reply posts
      const lookupRecursivePostsResponse =
            await fetch("/lookup_recursive_posts", {
              method: "POST",
              headers: {
                "Content-Type": "application/json"
              },
              body: JSON.stringify(this._parentPost["replies"])
            });
      if (!lookupRecursivePostsResponse.ok) {
        console.error(`Server error: ${lookupRecursivePostsResponse.status}`);
        return;
      }
      this._replyPosts = await lookupRecursivePostsResponse.json();
      this._dataLoaded = true;
      if (this._domReady) {
        this._populatePage();
        const postData = bespoke.peekPostStack();
        window.scrollTo(postData.scrollX, postData.scrollY);
      }
    } catch (error) {
      console.error("Loading of data failed:", error);
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
    // Insert username into title
    let username = bespoke.getCookieValue("username");
    document.getElementById("title-username").textContent = username;
    if (postStackSize > 1) {
      document.getElementById(
        "header-reply-level").textContent = `[level: ${postStackSize - 1}]`;
    } else {
      document.getElementById("header-reply-level").style.display = "none";
    }
    // Populate parent post
    document.getElementById("parent-post")
      .setAttribute("data-post-id", this._parentPost["id"]);
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
    // Add observers to post-dividers
    this._addHasBeenReadObservers();
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
        <div class="uk-text-meta quote"
             onclick=${(event) => this.toggleQuote(event)}>
          <span id="${replyQuoteButtonAttr}"
                class="uk-icon-link" uk-icon="chevron-down"></span>
          In reply to ${replyQuoteAuthor}...
          <div id="${replyQuoteAttr}"
               class="uk-card uk-card-body uk-card-default uk-card-small uk-text-meta uk-margin-small-bottom uk-margin-small-top custom-quote-padding"
               hidden>
            <div id="${replyQuoteBodyAttr}" class="quote-body">
              <p>Loading...</p>
            </div>
          </div>
        </div>`;
    }
    // Reply body
    const replyBodyAttr = `reply-body-${post["id"]}`;
    // Age
    const age = bespoke.formatSecondsSinceEpoch(post["created"]);
    // Replies
    let replies = "";
    if (post["reply-count"] > 0) {
      replies = html`•
        <span class="mini-action"><span onclick=${(event) => bespoke.gotoPage(event, "/post.html", post["id"])} uk-icon="comments"></span>
        ${post["reply-count"]}</span>`;
    }
    // Delete button
    let deleteButton = "";
    if (post["author"] === bespoke.getCookieValue("username")) {
      deleteButton = html`
        <span onclick=${(event) => this.openDeletePostModal(event)}
              class="mini-action"
              uk-icon="trash"></span>`;
    }
    return html`
      <div data-post-id="${post["id"]}"
           data-parent-post-id="${post["parent-post-id"]}">
        <!-- Quoted reply body -->
        ${replyQuote}
        <!-- Reply body -->
        <div id="${replyBodyAttr}" class="reply-body">
          ${bespoke.uhtmlFormatMarkdown(post["body"])}
        </div>
        <div class="uk-flex uk-flex-between uk-flex-middle">
          <!-- Reply meta-data -->
          <div class="uk-text-meta meta-data">
            <span uk-icon="icon: check" class="uk-text-success" hidden></span>
            ${post["author"]} •
            ${age} •
            <span onclick=${(event) => this.toggleLike(event)}
                  class="mini-action"><span uk-icon="icon: heart"></span>
              <span>${post["likes-count"]}</span></span>
            ${replies}
          </div>
          <!-- Reply actions -->
          <div>
            ${deleteButton}
            <span onclick=${(event) => addReplyPost.gotoAddReplyPage(event, post["id"], true)}
                  class="mini-action"
                  uk-icon="reply"></span>
          </div>
        </div>
        <hr class="uk-margin-small post-divider">
      </div>`;
  }

  _addHasBeenReadObservers() {
    // Add an observer to each node that has the class name "post-divider"
    const postDividers = document.getElementsByClassName("post-divider");
    for (const postDivider of postDividers) {
      const postElement = postDivider.closest("[data-post-id]");
      const postId = postElement.getAttribute("data-post-id");
      if (bespoke.getRawLocalItem(`post-${postId}`) != "") {
        // Add observer
        this._addHasBeenReadObserver(postDivider);
      } else {
        // Mark it as read in the UI
        const metaDataElement = postElement.querySelector('.meta-data');
        const hasBeenReadElement = metaDataElement.children[0];
        hasBeenReadElement.hidden = false;
      }
    }
  }

  _addHasBeenReadObserver(postDivider) {
    const options = {
      // Uses the viewport as the root
      root: null,
      // Shrinks the bottom by 50%, making the upper 50% the active area
      rootMargin: '0px 0px -50% 0px',
      // Trigger as soon as any part is visible within the adjusted root
      threshold: 0
    };
    const callback = (entries, observer) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          const postElement = postDivider.closest("[data-post-id]");
          const postId = postElement.getAttribute("data-post-id");
          // Mark it as read in the UI
          const metaDataElement = postElement.querySelector('.meta-data');
          const hasBeenReadElement = metaDataElement.children[0];
          hasBeenReadElement.hidden = false;
          // Mark it as read in the local storage
          bespoke.setRawLocalItem(`post-${postId}`, "");
          // Stop observing
          observer.unobserve(entry.target);
        }
      });
    };
    const observer = new IntersectionObserver(callback, options);
    observer.observe(postDivider);
  }

  openDeletePostModal(event) {
    event.stopPropagation();
    // Extract post to delete
    const postElement = event.currentTarget.closest("[data-post-id]");
    this._postIdToDelete = postElement.getAttribute("data-post-id");
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
    const updateServer = async () => {
      try {
        const postId =
              this._postIdToDelete === bespoke.peekPostStack().postId ? -1 :
              null;
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
        if (postId != -1) {
          bespoke.updatePostStackTopPosition();
        }
        bespoke.gotoPage(event, "post.html", postId);
        this._postIdToDelete = null;
      } catch (error) {
        console.error("Deletion of post failed:", error);
      }
    };
    updateServer();
  }

  toggleQuote(event) {
    event.preventDefault();
    const postElement = event.currentTarget.closest("[data-post-id]");
    const postId = postElement.getAttribute("data-post-id");
    const parentPostElement =
          event.currentTarget.closest("[data-parent-post-id]");
    const parentPostId = parentPostElement.getAttribute("data-parent-post-id");
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

  toggleLike(event) {
    event.preventDefault();
    const likeButton = event.currentTarget;
    const updateServer = async () => {
      try {
        const postElement = likeButton.closest("[data-post-id]");
        const postId = postElement.getAttribute("data-post-id");
        // REST API: Like post
        const response = await fetch("/toggle_like", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify(postId)
        });
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          return;
        }
        const result = await response.json();
        // Update the heart icon state
        if (result["liked"]) {
          likeButton.children[0].classList.add("bleeding-heart");
        } else {
          likeButton.children[0].classList.remove("bleeding-heart");
        }
        // Update the likes count
        likeButton.children[1].innerText = result["likes-count"];
      } catch (error) {
        console.error("Like post failed:", error);
      }
    };
    updateServer();
  }
}

const post = new Post();
export default post;
