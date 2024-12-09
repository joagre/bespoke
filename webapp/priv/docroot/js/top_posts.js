import bespoke from "/js/bespoke.js";

// Ensure uhtml.min.js is imported in the HTML file before this script
const { html, render } = uhtml;

class TopPosts {
  constructor() {
    bespoke.onReady("top_posts.html", () => this._load());
  }

  _load() {
    if (!bespoke.isCookieSet()) {
      bespoke.navigateTo("loader.html");
      return;
    }
    bespoke.clearPostStack();
    this._updatePage();
  }

  async _updatePage() {
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
        const hasBeenRead = await this._howManyHasBeenRead(post["id"]);
        bespoke.assert(hasBeenRead !== null, "Failed to get read count");
        post.hasBeenRead = hasBeenRead;
        annotatedTopPosts.push(post);
      }
      this._populatePage(annotatedTopPosts);
      const topPostIds = topPosts.map((post) => post["id"]);
      this._subscribeOnChanges(topPostIds);
    } catch (error) {
      console.error("Page update failed:", error);
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
    const username = bespoke.getCookieValue("username");
    document.getElementById("title-username").textContent = username;
    const postsContainer = document.getElementById("posts");
    if (topPosts.length === 0) {
      postsContainer.innerHTML = "<p>No posts available.</p>";
      return;
    }
    const postTemplates =
          topPosts.map((post) => this._createPostTemplate(post));
    render(postsContainer, html`${postTemplates}`);
  }

  _createPostTemplate(post) {
    // Age
    const age = bespoke.formatSecondsSinceEpoch(post["created"]);
    // Like
    let likeAttr = "";
    if (post["likers"].includes(bespoke.getCookieValue("userId"))) {
      likeAttr = "bleeding-heart";
    }
    // Replies
    let replies;
    if (post["reply-count"] == post.hasBeenRead       ) {
      replies = html`• <span uk-icon="comment"></span> ${post["reply-count"]}`;
    } else {
      replies =
        html`• <span uk-icon="commenting"></span> ${post.hasBeenRead} (${post["reply-count"]})`;
    }
    return html`
      <div onclick=${(event) => bespoke.gotoPage(event, "post.html", post["id"])}
           class="top-post">
        ${post["title"]}
        <div class="uk-text-meta uk-margin-small-top">
          ${post["author"]} •
          ${age} •
          <span class="${likeAttr}" uk-icon="icon: heart"></span> ${post["likers"].length}
          ${replies}
        </div>
        <hr class="uk-margin-small post-divider">
      </div>`;
  }

  async _subscribeOnChanges(postIds) {
    try {
      console.log('Subscribing on changes...');
      const response = await fetch('/subscribe_on_changes', {
        method: "POST",
        headers: {
          "Conncection": "close",
          "Content-Type": "application/json",
        },
        body: JSON.stringify(postIds)
      });
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        console.log('Retrying in 5 seconds');
        setTimeout(() => this._subscribeOnChanges(postIds), 5000);
        return;
      }
      const postId = await response.json();
      console.log(`${postId} has changed`);
      this._updatePage(); // Voila!
    } catch (error) {
      console.error('Subscribe on changes failed:', error);
    }
  }
}

const topPosts = new TopPosts();
export default topPosts
