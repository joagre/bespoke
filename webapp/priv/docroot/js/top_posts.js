import bespoke from "/js/bespoke.js";

// Ensure uhtml.min.js is imported in the HTML file before this script
const { html, render } = uhtml;

class TopPosts {
  constructor() {
    document.addEventListener("DOMContentLoaded", () => {
      bespoke.init();
      bespoke.clearPostStack();
      this.updatePage();
      // Update page every minute
      setInterval(() => this.updatePage(), 60000);
    });
  }

  async updatePage() {
    try {
      // REST: Get top posts
      const response = await fetch("/list_top_posts");
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return;
      }
      const topPosts = await response.json();

      this.populatePage(topPosts);
    } catch (error) {
      console.error("Fetching failed:", error);
    }
  }

  populatePage(topPosts) {
    const postsContainer = document.getElementById("posts");
    if (topPosts.length === 0) {
      postsContainer.innerHTML = "<p>No posts available.</p>";
      return;
    }
    const postTemplates =
          topPosts.map((post) => this.createPostTemplate(post));
    render(postsContainer, html`${postTemplates}`);
  }

  createPostTemplate(post) {
    const age = bespoke.formatSecondsSinceEpoch(post["created"]);
    return html`
      <div onclick=${(event) => {
             bespoke.gotoPage(event, "post.html", post["id"]);
           }} class="uk-padding-small top-post">
        ${post["title"]}
        <div class="uk-text-meta">
          ${post["author"]} • ${age} •
          <span uk-icon="comment"></span> ${post["reply-count"]}
        </div>
      </div>
      <hr class="uk-margin-remove">`;
  }
}

const topPosts = new TopPosts();
export default topPosts;
