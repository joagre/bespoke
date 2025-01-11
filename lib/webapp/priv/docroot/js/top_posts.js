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
    this._stickyHeader = document.querySelector(".sticky-header");
    this._mainContent = document.getElementById("main-content");
    this._haveNoPosts = document.getElementById("have-no-posts");
    this._havePosts = document.getElementById("have-posts");
    bespoke.clearPostStack();
    this._refreshTimer =
      setInterval(() => this._updatePage(), this._REFRESH_INTERVAL);
    // Update header
    const username = bespoke.getCookieValue("username");
    document.getElementById("title-username").textContent = username;
    const headerHeight = this._stickyHeader.offsetHeight;
    this._mainContent.style.paddingTop = headerHeight + "px";
    document.body.hidden = false;
    this._updatePage();
  }

  async _updatePage() {
    if (this._isUpdatingPage) {
      return;
    }
    this._isUpdatingPage = true;
    try {
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
      this._renderPosts(topPosts);
      const postIds = topPosts.map((post) => post.id);
      bespoke.subscribeOnChanges(postIds, () => this._updatePage());
    } catch (error) {
      console.error("Page update failed:", error);
    } finally {
      this._isUpdatingPage = false;
    }
  }

  _renderPosts(topPosts) {
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
    bespoke.initializeLongClick();
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
    let unread = "";
    if (!post.isRead) {
      unread = html`<span class="unread">✪</span>`;
      replies =
        html`• <span uk-icon="commenting"></span>
               ${post.readCount} (${post.replyCount})`;
      bespoke.markAsRead(post.id);
    } else if (post.replyCount == post.readCount) {
      replies = html`• <span uk-icon="comment"></span> ${post.replyCount}`;
    } else {
      if (post.readCount > 0) {
        replies =
          html`• <span uk-icon="commenting"></span>
                 ${post.readCount}
                 (<span class="unread">${post.replyCount}</span>)`;
        unread = html`<span class="unread">✪</span>`;
      } else {
        replies =
          html`• <span uk-icon="commenting"></span>
                 ${post.readCount} (${post.replyCount})`;
      }
    }
    const attachments = this.generateAttachments(post);
    return html`
      <div class="top-post"
           onclick=${(event) => bespoke.gotoPage(event, "post.html", post.id)}>
        <div>
          ${unread}
          <span class="uk-text-bold">${post.title}</span>
        </div>
        ${attachments}
        <div class="post-meta-data uk-text-meta">
          ${post.author} •
          ${age} •
          <span class="${likeAttr}" uk-icon="icon: heart"></span>
          ${post.likers.length}
          ${replies}
        </div>
        <hr class="uk-margin-small post-divider">
      </div>`;
  }

  generateAttachments(post) {
    let attachments = "";
    if (post.attachments.length > 0) {
      const items = [];
      for (const attachment of post.attachments) {
        const absPath = `/attachment/${post.id}/${attachment.filename}`;
        let item;
        if (attachment.contentType.startsWith("image/")) {
          item = html`<div class="attachment-item long-click" uk-lightbox>
            <a href="${absPath}" data-caption="${attachment.filename}">
              <img class="attachment-content" src="${absPath}"
                   alt="${attachment.filename}">
            </a>
          </div>`;
        } else if (attachment.contentType.startsWith("video/")) {
          item = html`<div class="attachment-item long-click" uk-lightbox>
            <a href="${absPath}" data-caption="${attachment.filename}">
              <img class="attachment-content" src="/images/1x1.png"
                   alt="${attachment.filename}">
              <div class="uk-position-center uk-text-break uk-text-muted attachment-misc">${attachment.filename}</div>
            </a>
          </div>`;
        } else {
          item = html`<div class="attachment-item long-click">
            <a href="${absPath}" data-caption="${attachment.filename}" download>
              <img class="attachment-content" src="/images/1x1.png"
                   alt="${attachment.filename}">
              <div class="uk-position-center uk-text-break uk-text-muted attachment-misc">${attachment.filename}</div>
            </a>
          </div>`;
        }
        items.push(item);
      }
      attachments = html`<div id="attachments" class="uk-position-relative"
                              uk-slider>
        <div class="uk-slider-items">
          ${items}
        </div>
        <a class="uk-slidenav uk-position-small uk-position-center-left uk-overlay uk-overlay-default" uk-slidenav-previous uk-slider-item="previous"></a>
        <a class="uk-slidenav uk-position-small uk-position-center-right uk-overlay uk-overlay-default" uk-slidenav-next uk-slider-item="next"></a>
      </div>`;
    }
    return attachments;
  }
}

const topPosts = new TopPosts();
export default topPosts
