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
    setInterval(() => this._updatePage(), 60000);
  }

  async _updatePage() {
    try {
      // REST: Get top posts
      const response = await fetch("/list_top_posts");
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return;
      }
      const result = await response.json();
      this._populatePage(result);
    } catch (error) {
      console.error("Page update failed:", error);
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
    const age = bespoke.formatSecondsSinceEpoch(post["created"]);
    return html`
      <div onclick=${(event) => {
             bespoke.gotoPage(event, "post.html", post["id"]);
           }} class="top-post">
        ${post["title"]}
        <div class="uk-text-meta uk-margin-small-top">
          ${post["author"]} •
          ${age} •
          <span uk-icon="icon: heart"></span> 12 •
          <span uk-icon="comment"></span> ${post["reply-count"]}
        </div>
      </div>
      <hr class="uk-margin-small post-divider">`;
  }
}

const topPosts = new TopPosts();
export default topPosts
