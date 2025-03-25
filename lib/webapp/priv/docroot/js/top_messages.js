// -*- fill-column: 100 -*-

"use strict";

const TopMessages = (function () {
  const {html, render} = uhtml;
  const _REFRESH_INTERVAL = 30000;
  const _userId = Bespoke.getCookieValue("userId");

  let _refreshTimer = null;
  let _isUpdatingPage = false;
  let _stickyHeaderElement;
  let _titleUsernameElement;
  let _mainContentElement;
  let _haveNoMessagesElement;
  let _haveMessagesElement;
  let _messagesElement;

  window.addEventListener("beforeunload", () => {
    if (_refreshTimer != null) {
      clearInterval(_refreshTimer);
    }
  });

  Bespoke.onReady("top_messages.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get header element
    _stickyHeaderElement = document.querySelector(".sticky-header");
    _titleUsernameElement = document.getElementById("title-username");

    // Get main content elements and start refresh timer
    _mainContentElement = document.getElementById("main-content");
    _haveNoMessagesElement = document.getElementById("have-no-messages");
    _haveMessagesElement = document.getElementById("have-messages");
    _messagesElement = document.getElementById("messages");
    _refreshTimer = setInterval(() => _updatePage(), _REFRESH_INTERVAL);

    // Update header
    const username = Bespoke.getCookieValue("username");
    _titleUsernameElement.textContent = username;

    // Adjust padding of main content
    setTimeout(() => {
      // The timeout is needed to ensure the header has been rendered :-9
      const headerHeight = _stickyHeaderElement.offsetHeight;
      _mainContentElement.style.paddingTop = headerHeight + "px";
    }, 100);

    document.body.hidden = false;

    _updatePage();
  }

  async function _updatePage() {
    if (_isUpdatingPage) {
      return;
    }

    // Render top messages
    _isUpdatingPage = true;
    try {
      // REST: Read top messages
      const response = await fetch("/api/read_top_messages");
      if (!response.ok) {
        if (response.status === 401) {
          Bespoke.navigateTo("loader.html");
          return;
        } else {
          console.error(`Server error: ${response.status}`);
          return;
        }
      }
      const topMessages = await response.json();
      _renderMessages(topMessages);
    } catch (error) {
      console.error("Page update failed:", error);
    } finally {
      _isUpdatingPage = false;
    }
  }

  async function _renderMessages(topMessages) {
    const messagesContainer = document.getElementById("messages");
    if (topMessages.length === 0) {
      _haveNoMessagesElement.hidden = false;
      _haveMessagesElement.hidden = true;
      return;
    } else {
      _haveNoMessagesElement.hidden = true;
      _haveMessagesElement.hidden = false;
    }
    const messageTemplates = await Promise.all(
      topMessages.map(async (topMessage) => await _createTopMessageTemplate(topMessage))
    );
    render(messagesContainer, html`${messageTemplates}`);
    Bespoke.initLongClick();
    _scrollToLastKnownPosition();
  }

  async function _createTopMessageTemplate(topMessage) {
    // Age
    const age = Bespoke.formatSecondsSinceEpoch(topMessage.created);

    // Replies
    let replies;
    let unread = [];
    const replyCount = topMessage.replyMessageIds.length;
    if (!topMessage.isRead) {
      unread = html`<span class="unread" uk-icon="bolt"></span>`;
      if (replyCount == 0) {
        replies = html`• <span uk-icon="comment"></span> ${replyCount}`;
      } else {
        replies =
          html`• <span uk-icon="commenting"></span> ${topMessage.readCount} (${replyCount})`;
      }
    } else if (replyCount == topMessage.readCount) {
      replies = html`• <span uk-icon="comment"></span> ${replyCount}`;
    } else {
      if (topMessage.readCount > 0) {
        replies =
          html`• <span uk-icon="commenting"></span>
                 ${topMessage.readCount}
                 (<span class="unread">${replyCount}</span>)`;
        unread = html`<span class="unread" uk-icon="bolt"></span>`;
      } else {
        replies = html`• <span uk-icon="commenting"></span> ${topMessage.readCount} (${replyCount})`;
      }
    }

    // Get message title
    const response = await fetch(`/message/${topMessage.id}/${_userId}`);
    let noContent = false;
    if (!response.ok) {
      if (response.status === 404) {
        noContent = true;
      } else if (response.status === 401) {
        Bespoke.navigateTo("loader.html");
        return;
      } else {
        console.error(`Server error: ${response.status}`);
        return;
      }
    }

    // Extract top message title
    let title;
    if (!noContent) {
      const blob = await response.blob();
      const unblobifiedBlob = await Crypto.unblobifyData(_userId, blob);
      const buffer = await unblobifiedBlob.arrayBuffer();
      const view = new DataView(buffer);
      const titleSize = view.getUint16(0, false);
      const decoder = new TextDecoder();
      title = decoder.decode(new Uint8Array(buffer, 2, titleSize));
    } else {
      title = "<No content>";
    }

    // Generate attachments
    const attachments = await generateAttachments(_userId, topMessage);

    // Recipients
    const usernames = topMessage.recipients.map(recipient => recipient.username);
    const index = usernames.indexOf(topMessage.authorUsername);
    if (index > -1) {
      usernames.splice(index, 1);
    }
    const recipients = html`[${usernames.join(", ")}]`;

    return html`
      <div>
        <div>
          <span onclick=${(event) => {
                  _saveScrollPosition();
                  Bespoke.gotoPage(event, "message.html",
                                   () => Bespoke.setLocalItem("topMessageId", topMessage.id));
                }}
                class="message-link uk-text-bold">${title}</span>
          <div class="uk-text-meta">
            ${recipients}
          </div>
        </div>
        ${attachments}
        <div class="message-meta-data uk-text-meta">
          ${unread} ${topMessage.authorUsername} •
          ${age}
          ${replies}
        </div>
        <hr class="uk-margin-small message-divider">
      </div>`;
  }

  function _scrollToLastKnownPosition() {
    const scrollPosition = Bespoke.getLocalItem("topMessagesScrollPosition", null);
    if (scrollPosition != null) {
      window.scrollTo(scrollPosition.scrollX, scrollPosition.scrollY);
    }
  }

  function _saveScrollPosition() {
    Bespoke.setLocalItem("topMessagesScrollPosition",
                         {scrollX: window.scrollX, scrollY: window.scrollY});
  }

  function clearScrollPosition() {
    Bespoke.clearLocalItem("topMessagesScrollPosition");
  }

  async function generateAttachments(userId, message) {
    let attachments = "";
    if (message.attachmentIds.length > 0) {
      const items = [];
      for (const attachmentId of message.attachmentIds) {
        // Fetch attachment metadata
        const metadataResponse =
              await fetch(`/message/${message.id}/${userId}-${attachmentId}.dat`);
        if (!metadataResponse.ok) {
          if (metadataResponse.status === 401) {
            Bespoke.navigateTo("loader.html");
            return;
          } else {
            console.error(`Server error: ${metadataResponse.status}`);
            return;
          }
        }
        const metadataBlob = await metadataResponse.blob();
        const unblobifiedMetadata = await Crypto.unblobifyData(userId, metadataBlob);
        const metadata =
              JSON.parse(new TextDecoder().decode(await unblobifiedMetadata.arrayBuffer()));

        // Fetch attachment
        let objectUrl = null;
        const attachmentAbsPath = `/message/${message.id}/${userId}-${attachmentId}`;
        if (metadata.contentType.startsWith("image/")) {
          const attachmentResponse = await fetch(attachmentAbsPath);
          if (!attachmentResponse.ok) {
            if (attachmentResponse.status === 401) {
              Bespoke.navigateTo("loader.html");
              return;
            } else {
              console.error(`Server error: ${attachmentResponse.status}`);
              return;
            }
          }
          const attachmentBlob = await attachmentResponse.blob();
          const unblobifiedAttachment = await Crypto.unblobifyData(userId, attachmentBlob);
          objectUrl = URL.createObjectURL(unblobifiedAttachment);
        }

        // Layout attachment
        if (objectUrl != null) {
          const item = html`<div class="attachment-item long-click" uk-lightbox>
            <a href="${objectUrl}" data-caption="${metadata.filename}" data-type="image"
               uk-tooltip="${metadata.filename}">
              <img class="attachment-content" src="${objectUrl}" alt="${metadata.filename}">
            </a>
          </div>`;
          items.push(item);
        } else {
          const aHref = `javascript:TopMessages.downloadFile(event, "${attachmentAbsPath}",
                                                             "${metadata.filename}")`;
          const item = html`<div class="attachment-item long-click">
            <a href="${aHref}" data-caption="${metadata.filename}"
               uk-tooltip="${metadata.filename}">
              <img class="attachment-content" src="/images/1x1.png" alt="${metadata.filename}">
              <div class="uk-position-center uk-text-break uk-text-muted
                          attachment-misc">${Bespoke.truncateMiddle(metadata.filename, 12)}</div>
            </a>
          </div>`;
          items.push(item);
        }
      }

      attachments = html`<div id="attachments" class="uk-position-relative" uk-slider>
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

  async function downloadFile(event, absPath, filename) {
    Bespoke.ignoreEvent(event);

    // Prompt user to choose where to save file.
    const fileHandle = await window.showSaveFilePicker({suggestedName: filename});

    // Create a writable stream on chosen file.
    const writableStream = await fileHandle.createWritable();

    // Fetch encrypted data as a ReadableStream.
    const response = await fetch(absPath);
    if (!response.ok || !response.body) {
      throw new Error(`Request failed with status: ${response.status}`);
    }

    // Create a decryption stream
    const decryptStream = new TransformStream({
      transform(chunk, controller) {
        Crypto.unblobifyChunk(chunk)
          .then(result => {
            controller.enqueue(new Uint8Array(result));
          })
          .catch(err => controller.error(err));
      }
    });

    // Pipe encrypted stream -> decrypt stream -> writable file stream.
    await response.body.pipeThrough(decryptStream).pipeTo(writableStream);

    console.log(`${absPath} downloaded and unblobified`);
  }

  return {
    clearScrollPosition,
    generateAttachments,
    downloadFile
  };
})();
