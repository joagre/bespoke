import bespoke from "/js/bespoke.js";

// Ensure uhtml.min.js is imported in the HTML file before this script
const { html, render } = uhtml;

class TopPosts {
  constructor() {
    this._isUpdatingPage = false;
    this._refreshTimer = null;
    this._REFRESH_INTERVAL = 30000;
    bespoke.onReady("top_posts.html", () => this._load());
  }

  destructor() {
    if (this._refreshTimer != null) {
      clearInterval(this._refreshTimer);
    }
  }

  _load() {
    if (!bespoke.hasSessionId()) {
      bespoke.navigateTo("loader.html");
      return;
    }
    this._haveNoPosts = document.getElementById("have-no-posts");
    this._havePosts = document.getElementById("have-posts");
    bespoke.clearPostStack();
    this._refreshTimer =
      setInterval(() => this._updatePage(), this._REFRESH_INTERVAL);
    this._updatePage();
  }

  async _updatePage() {
    if (this._isUpdatingPage) {
      return;
    }
    this._isUpdatingPage = true;
    try {
      // REST: Get top posts
      const response = await fetch("/list_top_posts");
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return;
      }
      const topPosts = await response.json();
      // How many replies have been read
      let annotatedTopPosts = [];
      for (const post of topPosts) {
        const hasBeenRead = await this._howManyHasBeenRead(post.id);
        bespoke.assert(hasBeenRead !== null, "Failed to get read count");
        post.hasBeenRead = hasBeenRead;
        annotatedTopPosts.push(post);
      }
      this._populatePage(annotatedTopPosts);
      // Subscribe on changes
      const postIds = topPosts.map((post) => post.id);
      bespoke.subscribeOnChanges(postIds, () => this._updatePage());
    } catch (error) {
      console.error("Page update failed:", error);
    } finally {
      this._isUpdatingPage = false;
    }
  }

  async _howManyHasBeenRead(postId) {
    try {
      const response = await fetch("/lookup_recursive_post_ids", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify([postId])
      });
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return null;
      }
      const postIds = await response.json();
      let hasBeenRead = 0;
      for (const postId of postIds) {
        if (bespoke.getRawLocalItem(`post-${postId}`) == "") {
          hasBeenRead++;
        }
      }
      return hasBeenRead;
    } catch (error) {
      console.error("Recursive lookup of post-ids failed:", error);
      return null;
    }
  }

  _populatePage(topPosts) {
    // Header
    const username = bespoke.getCookieValue("username");
    document.getElementById("title-username").textContent = username;
    // Body
    document.body.hidden = false;
    const postsContainer = document.getElementById("posts");
    if (topPosts.length === 0) {
      this._haveNoPosts.hidden = false;
      this._havePosts.hidden = true;
      return;
    } else {
      this._haveNoPosts.hidden = true;
      this._havePosts.hidden = false;
    }
    const postTemplates =
          topPosts.map((post) => this._createPostTemplate(post));
    render(postsContainer, html`${postTemplates}`);
  }
  
  _createPostTemplate(post) {
    // Age
    const age = bespoke.formatSecondsSinceEpoch(post.created);
    // Like
    let likeAttr = "";
    if (post.likers.includes(bespoke.getCookieValue("userId"))) {
      likeAttr = "bleeding-heart";
    }
    // Replies
    let replies;
    if (post.replyCount == post.hasBeenRead) {
      replies = html`• <span uk-icon="comment"></span> ${post.replyCount}`;
    } else {
      replies =
        html`• <span uk-icon="commenting"></span> ${post.hasBeenRead} (${post.replyCount})`;
    }
    return html`
      <div onclick=${(event) => bespoke.gotoPage(event, "post.html", post.id)}
           class="top-post">
        ${post.title}
        <div class="post-meta-data uk-text-meta">
          ${post.author} •
          ${age} •
          <span class="${likeAttr}" uk-icon="icon: heart"></span> ${post.likers.length}
          ${replies}
        </div>
        <hr class="uk-margin-small post-divider">
      </div>`;
  }
}

const topPosts = new TopPosts();
export default topPosts
