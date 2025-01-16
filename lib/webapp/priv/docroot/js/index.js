import bespoke from "/js/bespoke.js";

// Ensure uhtml.min.js is imported in the HTML file before this script
const { html, render } = uhtml;

class Index {
  constructor() {
    bespoke.onReady("index.html", () => this._load());
  }

  async _load() {
    if (!bespoke.hasSessionId()) {
      bespoke.navigateTo("loader.html");
      return;
    }

    // Switch over from mini-browser to system browser on Android
    if (bespoke.isAndroid() && !bespoke.isMiniBrowser()) {
      const intentUrl = `intent://${window.location.hostname}/index.html#Intent;scheme=http;end`;
      await bespoke.captivePortalAck();
      bespoke.navigateTo(intentUrl);
      return;
    }








    this._updatePage();
  }

  _updatePage() {
    const username = bespoke.getCookieValue("username");
    document.getElementById("title-username").textContent = username;
    document.body.hidden = false;
    this._updateUnreadForumPosts();
  }

  async _updateUnreadForumPosts() {
    let unread = 0;
    // REST: Get top posts
    const response = await fetch("/api/list_top_posts");
    if (!response.ok) {
      console.error(`Server error: ${response.status}`);
      if (response.status === 401) {
        bespoke.navigateTo("loader.html");
      }
      return;
    }
    const topPosts = await response.json();
    for (const post of topPosts) {
      if (!post.isRead) {
        unread++;
      }
      if (post.readCount > 0) {
        unread += post.replyCount - post.readCount;
      }
    }
    document.getElementById("unread-posts-counter").textContent = unread;
    document.getElementById("unread-posts").hidden = unread === 0;
  }
}

const index = new Index();
export default index
