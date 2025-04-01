// -*- fill-column: 100 -*-

"use strict";

const PostLib = (function () {
  const {html, render} = uhtml;

  async function readPost(postId) {
    // REST: Read post
    const response = await fetch("/api/read_posts", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify([postId]),
    });
    if (!response.ok) {
      if (response.status === 401) {
        Bespoke.navigateTo("loader.html");
      }
      console.error(`Server error: ${response.status}`);
      return {ok: false};
    }
    const result = await response.json();
    Bespoke.assert(result.length === 1, "Expected exactly one post");
    const post = result[0];
    return {ok: true, post};
  }

  function generateAttachments(post) {
    let attachments = "";
    if (post.attachments.length > 0) {
      const items = [];
      for (const attachment of post.attachments) {
        const absPath = `/post/${post.id}/${attachment.filename}`;
        let item;
        if (attachment.contentType.startsWith("image/")) {
          item = html`<div class="attachment-item long-click" uk-lightbox>
            <a href="${absPath}" data-caption="${attachment.filename}"
               uk-tooltip="${attachment.filename}">
              <img class="attachment-content" src="${absPath}"
                   alt="${attachment.filename}">
            </a>
          </div>`;
        } else if (attachment.contentType.startsWith("video/")) {
          item = html`<div class="attachment-item long-click" uk-lightbox>
            <a href="${absPath}" data-caption="${attachment.filename}"
               uk-tooltip="${attachment.filename}">
              <img class="attachment-content" src="/images/1x1.png"
                   alt="${attachment.filename}">
              <div class="uk-position-center uk-text-break uk-text-muted attachment-misc">${attachment.filename}</div>
            </a>
          </div>`;
        } else {
          item = html`<div class="attachment-item long-click">
            <a href="${absPath}" data-caption="${attachment.filename}" download>
              <img class="attachment-content" src="/images/1x1.png"
                   alt="${attachment.filename}" uk-tooltip="${attachment.filename}">
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

  return {
    readPost,
    generateAttachments
  };
})();
