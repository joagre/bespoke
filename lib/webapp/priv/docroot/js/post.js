import bespoke from "/js/bespoke.js";

// uhtml.min.js must be imported in the HTML file before this script
const { html, render } = uhtml;

class Post {
  constructor() {
    this._isLoadingData = false;
    this._firstLoad = true;
    this._dataLoaded = false;
    this._domReady = false;
    this._refreshTimer = null;
    this._REFRESH_INTERVAL = 30000;
    // Note: bespoke.onReady() is not used by design
    if (window.location.pathname == "/post.html") {
      this._load();
    }
  }

  destructor() {
    if (this._refreshTimer != null) {
      clearInterval(this._refreshTimer);
    }
  }

  _load() {
    if (!bespoke.hasSessionId() || bespoke.isPostStackEmpty()) {
      bespoke.navigateTo("loader.html");
      return;
    }
    document.addEventListener("DOMContentLoaded", () => {
      this._domReady = true;
      if (this._dataLoaded) {
        this._populatePage();
      }
    });







    /*
    this._refreshTimer =
    setInterval(() => this._loadData(), this._REFRESH_INTERVAL);
    */
    this._loadData();
  }

  async _loadData() {
    if (this._isLoadingData) {
      return;
    }
    this._isLoadingData = true;
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
      this._topPostTitle = this._parentPost.title;
      if (this._topPostTitle == null) {
        const lookupPostsResponse = await fetch("/lookup_posts", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify([this._parentPost.topPostId])
        });
        if (!lookupPostsResponse.ok) {
          console.error(`Server error: ${lookupPostsResponse.status}`);
          return;
        }
        const lookupPostsResult = await lookupPostsResponse.json();
        bespoke.assert(lookupPostsResult.length === 1,
                       "Expected exactly one post");
        this._topPostTitle = lookupPostsResult[0].title;
      }
      // Possible to remove delete button if not author
      if (this._parentPost.author !== bespoke.getCookieValue("username")) {
        document.getElementById("parent-delete").style.display = "none";
      }
      // REST: Get reply posts
      const lookupRecursivePostsResponse =
            await fetch("/lookup_recursive_posts", {
              method: "POST",
              headers: {
                "Content-Type": "application/json"
              },
              body: JSON.stringify(this._parentPost.replies)
            });
      if (!lookupRecursivePostsResponse.ok) {
        console.error(`Server error: ${lookupRecursivePostsResponse.status}`);
        return;
      }
      this._replyPosts = await lookupRecursivePostsResponse.json();
      this._dataLoaded = true;
      if (this._domReady) {
        this._populatePage();
      }
      // Subscribe on changes
      const postIds = this._replyPosts.map((post) => post.id);
      postIds.push(bespoke.peekPostStack().postId);
      bespoke.subscribeOnChanges(postIds, () => this._loadData());
    } catch (error) {
      console.error("Loading of data failed:", error);
    } finally {
      this._isLoadingData = false;
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
      .setAttribute("data-post-id", this._parentPost.id);
    document.getElementById("parent-title").innerHTML = this._topPostTitle;
    document.getElementById("parent-body").innerHTML =
      bespoke.formatMarkdown(this._parentPost.body);
    document.getElementById("parent-author").textContent =
      this._parentPost.author;
    document.getElementById("parent-age").textContent =
      bespoke.formatSecondsSinceEpoch(this._parentPost.created);
    if (this._parentPost.likers.includes(bespoke.getCookieValue("userId"))) {
      const button = document.getElementById("parent-like-icon");
      button.classList.add("liked");
      bespoke.refreshUIKitIcon(button);
    }
    document.getElementById("parent-likes-count").textContent =
      this._parentPost.likers.length;
    document.getElementById("parent-replies").textContent =
      this._parentPost.replyCount;
    document.getElementById("parent-delete")
      .setAttribute("data-post-id", this._parentPost.id);
    // Populate replies
    document.body.hidden = false;
    const repliesContainer = document.getElementById("replies");
    const replyTemplates = this._replyPosts.map((replyPost) =>
      this._createReplyTemplate(this._parentPost, replyPost,
                                this._replyPosts)
    );
    render(repliesContainer, html`${replyTemplates}`);
    bespoke.refreshAllUIKitIcons();
    // Add observers to post-dividers
    this._addHasBeenReadObservers();
    // Scroll to the correct position on first load
    if (this._firstLoad) {
      if (bespoke.getLocalItem("childPost")) {
        // Scroll to first unread post
        console.log("Scrolling to unread post");
        const unreadPost = document.getElementById("unread-post");
        if (unreadPost) {
          unreadPost.scrollIntoView({
            behavior: "smooth",
            block: "start",
            inline: "nearest"
          });
        }
      } else {
        // Scroll to saved position
        const postData = bespoke.peekPostStack();
        console.log("Scrolling to saved position");
        window.scrollTo(postData.scrollX, postData.scrollY);
      }
      this._firstLoad = false;
    }
  }

  _createReplyTemplate(parentPost, post, replyPosts) {
    // A reply quote is only added if someone replies to a reply
    let replyQuote = "";
    if (post.parentPostId != null &&
        post.parentPostId != parentPost.id) {
      const replyQuoteActionAttr = `reply-quote-action-${post.id}`;
      const replyQuoteAttr = `reply-quote-${post.id}`;
      const replyQuoteBodyAttr = `reply-quote-body-${post.id}`;
      const parentReplyPost = replyPosts.find(
        (replyPost) => replyPost.id == post.parentPostId
      );
      const replyQuoteAuthor =
            parentReplyPost ? parentReplyPost.author : "Unknown";
      replyQuote = html`
        <!-- Reply quote -->
        <div class="uk-text-meta quote"
             onclick=${(event) => this.toggleQuote(event)}>
          <span id="${replyQuoteActionAttr}"
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
    const replyBodyAttr = `reply-body-${post.id}`;
    // Age
    const age = bespoke.formatSecondsSinceEpoch(post.created);
    // Like
    let likeAttr;
    if (post.likers.includes(bespoke.getCookieValue("userId"))) {
      likeAttr = "liked toolbar-button";
    } else {
      likeAttr = "toolbar-button";
    }
    // Replies
    let replies = "";
    if (post.replyCount > 0) {
      replies = html`•
        <button onclick=${(event) => bespoke.gotoPage(event, "/post.html", post.id)}
                class="toolbar-button" uk-icon="comment"></button>
        ${post.replyCount}</span>`;
    }
    // Delete button
    let deleteButton = "";
    if (post.author === bespoke.getCookieValue("username")) {
      deleteButton = html`
        <button onclick=${(event) => this.openDeletePostDialog(event)}
                class="toolbar-button" uk-icon="trash"></button>`;
    }
    return html`
      <div data-post-id="${post.id}"
           data-parent-post-id="${post.parentPostId}">
        <!-- Quoted reply body -->
        ${replyQuote}
        <!-- Reply body -->
        <div id="${replyBodyAttr}" class="reply-body">
          ${bespoke.uhtmlFormatMarkdown(post.body)}
        </div>
        <div class="uk-flex uk-flex-between uk-flex-middle">
          <!-- Reply meta-data -->
          <div class="uk-text-meta meta-data">
            <span uk-icon="icon: check" hidden></span>
            ${post.author} •
            ${age} •
            <button onclick=${(event) => this.toggleLike(event)}
                    class="${likeAttr}" uk-icon="icon: heart"></button>
              <span>${post.likers.length}</span></span>
            ${replies}
          </div>
          <!-- Buttons -->
          <div>
            ${deleteButton}
            <button onclick=${(event) => addReplyPost.gotoAddReplyPage(event, post.id, true)}
                    class="toolbar-button" uk-icon="reply"></button>
          </div>
        </div>
        <hr class="uk-margin-small post-divider">
      </div>`;
  }

  _addHasBeenReadObservers() {
    // Add an observer to each node that has the class name "post-divider"
    const postDividers = document.getElementsByClassName("post-divider");
    let firstUnreadPostFound = false;
    let postElement = null;
    for (const postDivider of postDividers) {
      postElement = postDivider.closest("[data-post-id]");
      const postId = postElement.getAttribute("data-post-id");
      if (bespoke.getRawLocalItem(`post-${postId}`) != "") {
        // Add observer to unread post
        this._addHasBeenReadObserver(postDivider);
        // Mark the first unread post
        if (!firstUnreadPostFound) {
          console.log(`${postId} is the first unread post`);
          postElement.setAttribute("id", "unread-post");
          firstUnreadPostFound = true;
        }
      } else {
        // Mark it as read in the UI
        const metaDataElement = postElement.querySelector('.meta-data');
        const hasBeenReadElement = metaDataElement.children[0];
        hasBeenReadElement.hidden = false;
      }
    }
    // If all posts are read, mark the last post as unread to make
    // sure that last post is shown
    if (!firstUnreadPostFound && postElement != null) {
      const postId = postElement.getAttribute("data-post-id");
      console.log(`${postId} is the last post`);
      postElement.setAttribute("id", "unread-post");
    }
  }

  _addHasBeenReadObserver(postDivider) {
    const options = {
      root: null, // Uses the viewport as the root
      rootMargin: '0px', // No margin adjustments; full viewport
      threshold: 0 // Trigger when any part is visible
    };
    const callback = (entries, observer) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          const postElement = postDivider.closest("[data-post-id]");
          const postId = postElement.getAttribute("data-post-id");
          console.log(`${postId} has been read`);
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

  openDeletePostDialog(event) {
    event.stopPropagation();
    // Extract post to delete
    const postElement = event.currentTarget.closest("[data-post-id]");
    this._postIdToDelete = postElement.getAttribute("data-post-id");
    // Update the delete post dialog
    const replyPost = this._replyPosts.find(
      (replyPost) => replyPost.id === this._postIdToDelete
    );
    const postAuthor = replyPost ? replyPost.author : "this post";
    document.getElementById("delete-post-body").innerHTML =
      `Do you really want to delete this post written by ${postAuthor}?`;
    UIkit.modal("#delete-post-dialog").show();
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
          if (response.status === 401) {
            bespoke.navigateTo("loader.html");
          } else {
            console.error(`Server error: ${response.status}`);
          }
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
    bespoke.ignoreEvent(event);
    const postElement = event.currentTarget.closest("[data-post-id]");
    const postId = postElement.getAttribute("data-post-id");
    const parentPostElement =
          event.currentTarget.closest("[data-parent-post-id]");
    const parentPostId = parentPostElement.getAttribute("data-parent-post-id");
    const replyQuote = document.getElementById(`reply-quote-${postId}`);
    const isHidden = replyQuote.hidden;
    const replyQuoteAction =
          document.getElementById(`reply-quote-action-${postId}`);
    if (isHidden) {
      const replyQuoteBody =
            document.getElementById(`reply-quote-body-${postId}`);
      const replyBody =
            document.getElementById(`reply-body-${parentPostId}`);
      replyQuoteBody.innerHTML = replyBody.innerHTML;
      replyQuote.hidden = false;
      replyQuoteAction.setAttribute("uk-icon", "chevron-up");
    } else {
      replyQuote.hidden = true;
      replyQuoteAction.setAttribute("uk-icon", "chevron-down");
    }
  }

  toggleLike(event) {
    bespoke.ignoreEvent(event);
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
        if (result.liked) {
          likeButton.classList.add("liked");
        } else {
          likeButton.classList.remove("liked");
        }
        bespoke.refreshUIKitIcon(likeButton);
        // Update the likes count
        likeButton.nextSibling.innerText = result.likesCount;
      } catch (error) {
        console.error("Like post failed:", error);
      }
    };
    updateServer();
  }
}

const post = new Post();
export default post;
