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

  async function createPostAndAttachments(title, body, parentPostId, topPostId, uploadControllers,
                                          goBack) {
    // Upload attachments
    function onProgress(total, loaded, percentComplete) {
      Progress.update(total, loaded, percentComplete);
    }
    const files = await AddAttachments.getAllFiles();
    let attachments = [];
    for (const file of files) {
      Progress.show();

      // Upload file
      const uploadController = Bespoke.uploadFile(file.file, onProgress);
      uploadControllers.push(uploadController);
      const uploadedFile = await Bespoke.waitForUpload(uploadController);
      const index = uploadControllers.indexOf(uploadController);
      uploadControllers.splice(index, 1);
      console.log("Attachment has been uploaded:", uploadedFile);

      // Add to attachments variable (used by /api/create_post REST call)
      const attachment = {
        filename: uploadedFile.absPath.replace("/tmp/", ""),
        contentType: uploadedFile.contentType
      }
      attachments.push(attachment);

      Progress.hide();
    }

    // Create post
    try {
      const payload = {
        title: title == null ? undefined : title,
        parentPostId: parentPostId == null ? undefined : parentPostId,
        topPostId: parentPostId != null ?
          (topPostId != null ? topPostId : parentPostId) :
          undefined,
        body: body,
        attachments
      };

      // REST: Create post
      const response = await fetch("/api/create_post", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(payload),
      });
      if (!response.ok) {
        if (response.status === 401) {
          Bespoke.navigateTo("loader.html");
        } else {
          console.error(`Server error: ${response.status}`);
        }
        return;
      }

      goBack();
    } catch (error) {
      console.error("Addition of top post failed:", error);
    }
  }

  function generateAttachments(post) {
    let attachments = "";
    if (post.attachments.length > 0) {
      const items = [];
      for (const attachment of post.attachments) {
        const absPath = `/post/${post.id}/${attachment.filename}`;
        let item;
        if (attachment.contentType.startsWith("image/")) {
          item = html`
            <div class="attachment-item" uk-lightbox>
              <a tabindex="-1" href="${absPath}" data-caption="${attachment.filename}"
                 title="${attachment.filename}">
                <img class="attachment-content" src="${absPath}" alt="${attachment.filename}"
                     loading="lazy">
              </a>
            </div>`;
        } else if (attachment.contentType.startsWith("video/")) {
          item = html`
            <div class="attachment-item" uk-lightbox>
              <a tabindex="-1" href="${absPath}" data-caption="${attachment.filename}"
                 title="${attachment.filename}">
                <img class="attachment-content" src="/images/1x1.png" alt="${attachment.filename}">
                <div class="uk-position-center uk-text-break uk-text-muted attachment-misc">
                  ${attachment.filename}
                </div>
              </a>
            </div>`;
        } else {
          item = html`
            <div class="attachment-item">
              <a tabindex="-1" href="${absPath}" data-caption="${attachment.filename}" download>
                <img class="attachment-content" src="/images/1x1.png" alt="${attachment.filename}"
                     title="${attachment.filename}">
                <div class="uk-position-center uk-text-break uk-text-muted attachment-misc">
                  ${attachment.filename}
                </div>
              </a>
            </div>`;
        }
        items.push(item);
      }
      attachments = html`
        <div id="attachments" class="uk-position-relative" uk-slider>
          <div class="uk-slider-items">
            ${items}
          </div>
          <a class="uk-slidenav uk-position-small uk-position-center-left uk-overlay
                    uk-overlay-default" uk-slidenav-previous uk-slider-item="previous"></a>
          <a class="uk-slidenav uk-position-small uk-position-center-right uk-overlay
                    uk-overlay-default" uk-slidenav-next uk-slider-item="next"></a>
        </div>`;
    }
    return attachments;
  }

  return {
    readPost,
    createPostAndAttachments,
    generateAttachments
  };
})();
