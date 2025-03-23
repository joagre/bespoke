// -*- fill-column: 100 -*-

"use strict";

const Message = (function () {
  const {html, render} = uhtml;
  const _REFRESH_INTERVAL = 30000;
  let _userId = Bespoke.getCookieValue("userId");
  let _username = Bespoke.getCookieValue("username");
  let _isLoadingData = false;
  let _firstLoad = true;
  let _dataLoaded = false;
  let _domReady = false;
  let _refreshTimer = null;
  let _parentMessage;
  let _parentMessageTitle;
  let _parentMessageBody;
  let _replyMessages;
  let _messageIdToDelete;
  let _stickyHeaderElement;
  let _titleUsernameElement;
  let _mainContentElement;
  let _parentMessageElement;
  let _parentTitleElement;
  let _parentBodyElement;
  let _parentAttachmentsElement;
  let _parentAuthorElement;
  let _parentAgeElement;
  let _parentRepliesElement;
  let _parentDeleteElement;
  let _deleteMessageBodyElement;

  window.addEventListener("beforeunload", () => {
    if (_refreshTimer != null) {
      clearInterval(_refreshTimer);
    }
  });

  if (window.location.pathname == "/message.html") {
    _load();
  }

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get header elements
    _stickyHeaderElement = document.querySelector(".sticky-header");
    _titleUsernameElement = document.getElementById("title-username");

    // Get main content element
    _mainContentElement = document.getElementById("main-content");

    // Get parent message elements
    _parentMessageElement = document.getElementById("parent-message");
    _parentTitleElement = document.getElementById("parent-title");
    _parentBodyElement = document.getElementById("parent-body");
    _parentAttachmentsElement = document.getElementById("parent-attachments");
    _parentAuthorElement = document.getElementById("parent-author");
    _parentAgeElement = document.getElementById("parent-age");
    _parentRepliesElement = document.getElementById("parent-replies");
    _parentDeleteElement = document.getElementById("parent-delete");

    // Get dialog elements
    _deleteMessageBodyElement = document.getElementById("delete-message-body");

    document.addEventListener("DOMContentLoaded", () => {
      _domReady = true;
      if (_dataLoaded) {
        _populatePage();
      }
    });

    _refreshTimer = setInterval(() => _loadData(), _REFRESH_INTERVAL);
    _loadData();
  }

  async function _loadData() {
    if (_isLoadingData) {
      return;
    }

    _isLoadingData = true;
    try {
      // REST: Read message
      const response = await fetch("/api/read_messages", {
        method: "POST",
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify([Bespoke.getLocalItem("messageId")])
      });
      if (!response.ok) {
        if (response.status == 401) {
          Bespoke.navigateTo("loader.html");
        }
        console.error(`Server error: ${response.status}`);
        return;
      }
      const result = await response.json();
      Bespoke.assert(result.length === 1, "Expected exactly one message");
      _parentMessage = result[0];
      Bespoke.markMessageAsRead(_parentMessage.id);


      console.error("_parentMessage:", _parentMessage);


      // Fetch message body
      const bodyResponse = await fetch(`/message/${_parentMessage.id}/${_userId}`);
      if (!bodyResponse.ok) {
        console.error(`Server error: ${bodyResponse.status}`);
        if (bodyResponse.status === 401) {
          Bespoke.navigateTo("loader.html");
        }
        return;
      }

      // Parse message body
      const blob = await bodyResponse.blob();
      const unblobifiedBlob = await Crypto.unblobifyData(_userId, blob);
      const buffer = await unblobifiedBlob.arrayBuffer();
      const view = new DataView(buffer);
      const titleSize = view.getUint16(0, false);
      const decoder = new TextDecoder();
      _parentMessageTitle = decoder.decode(new Uint8Array(buffer, 2, titleSize));
      _parentMessageBody = decoder.decode(new Uint8Array(buffer, 2 + titleSize));

      // Remove delete button if not author
      if (_parentMessage.authorId !== _userId) {
        _parentDeleteElement.style.display = "none";
      }

      // REST: Read reply messages
      if (_parentMessage.replyMessageIds.length == 0) {
        _replyMessages = [];
      } else {
        const readReplyMessagesResponse = await fetch("/api/read_reply_messages", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify(_parentMessage.replyMessageIds)
        });

        if (!readReplyMessagesResponse.ok) {
          if (readReplyMessagesResponse.status == 401) {
            Bespoke.navigateTo("loader.html");
          }
          console.error(`Server error: ${readReplyMessagesResponse.status}`);
          return;
        }
        _replyMessages = await readReplyMessagesResponse.json();
      }

      _dataLoaded = true;

      if (_domReady) {
        _populatePage();
      }
    } catch (error) {
      console.error("Loading of data failed:", error);
    } finally {
      _isLoadingData = false;
    }
  }

  async function _populatePage() {
    // Adjust padding of main content
    setTimeout(() => {
      // The timeout is to ensure that the header has been rendered :-9
      const headerHeight = _stickyHeaderElement.offsetHeight;
      _mainContentElement.style.paddingTop = headerHeight + "px";
    }, 100);

    // Populate header
    _titleUsernameElement.textContent = _username;

    // Populate parent message
    _parentMessageElement.setAttribute("data-message-id", _parentMessage.id);
    _parentTitleElement.innerText = _parentMessageTitle;
    const attachments = await _generateAttachments();
    if (attachments != "") {
      render(_parentAttachmentsElement, html`${attachments}`);
      Bespoke.initLongClick();
    }
    _parentBodyElement.innerHTML = Bespoke.formatMarkdown(_parentMessageBody);
    _parentAuthorElement.textContent = _parentMessage.authorUsername;
    _parentAgeElement.textContent = Bespoke.formatSecondsSinceEpoch(_parentMessage.created);
    _parentRepliesElement.textContent = _parentMessage.replyCount;
    _parentDeleteElement.setAttribute("data-message-id", _parentMessage.id);

    document.body.hidden = false;


/*
    const repliesContainer = document.getElementById("replies");
    const replyTemplates = _replyPosts.map((replyPost) =>
      _createReplyTemplate(_parentPost, replyPost, _replyPosts)
    );
    render(repliesContainer, html`${replyTemplates}`);
    Bespoke.refreshAllUIKitIcons();
    Bespoke.disableLinks(".reply-body");
    // Add observers to post-dividers
    _addHasBeenReadObservers();
    // Scroll to the correct position on first load
    if (_firstLoad) {
      if (Bespoke.getLocalItem("childPost")) {
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
        const postData = Bespoke.peekPostStack();
        console.log("Scrolling to saved position");
        window.scrollTo(postData.scrollX, postData.scrollY);
      }
      _firstLoad = false;
    }
*/
  }

/*
  function _createReplyTemplate(parentPost, post, replyPosts) {
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
            parentReplyPost ? parentReplyPost.authorUsername : "Unknown";
      replyQuote = html`
        <!-- Reply quote -->
        <div class="uk-text-meta">
          <div class="quote" onclick=${(event) => toggleQuote(event)}>
            <span id="${replyQuoteActionAttr}"
                  class="uk-icon-link" uk-icon="chevron-down"></span>
            In reply to ${replyQuoteAuthor}...
          </div>
          <div id="${replyQuoteAttr}"
               class="uk-card uk-card-body uk-card-default uk-card-small uk-text-meta uk-margin-small-bottom quote-card" hidden>
            <div id="${replyQuoteBodyAttr}" class="quote-body">
              <p>Loading...</p>
            </div>
          </div>
        </div>`;
    }
    // Reply body
    const replyBodyAttr = `reply-body-${post.id}`;
    // Age
    const age = Bespoke.formatSecondsSinceEpoch(post.created);
    // Like
    let likeAttr;
    if (post.likers.includes(_userId)) {
      likeAttr = "liked toolbar-button";
    } else {
      likeAttr = "toolbar-button";
    }
    // Replies
    let replies = "";
    if (post.replyCount > 0) {
      replies = html`•
        <button onclick=${(event) => Bespoke.gotoPage(event, "/post.html", post.id)}
                class="toolbar-button" uk-icon="comment"></button>
        ${post.replyCount}</span>`;
    }
    // Delete button
    let deleteButton = "";
    if (post.authorId === Bespoke.getCookieValue("userId")) {
      deleteButton = html`
        <button onclick=${(event) => openDeletePostDialog(event)}
                class="toolbar-button" uk-icon="trash" uk-tooltip="Delete"></button>`;
    }
    const attachments = TopPosts.generateAttachments(post);
    return html`
      <div data-post-id="${post.id}"
           data-is-read="${post.isRead}"
           data-parent-post-id="${post.parentPostId}">
        <!-- Quoted reply body -->
        ${replyQuote}
        <!-- Reply body -->
        <div id="${replyBodyAttr}" class="reply-body uk-text-break">
          ${Bespoke.uhtmlFormatMarkdown(post.body)}
        </div>
        ${attachments}
        <div class="uk-flex uk-flex-between uk-flex-middle">
          <!-- Reply meta-data -->
          <div class="uk-text-meta meta-data">
            <span class="unread" uk-icon="bolt"></span>
            ${post.authorUsername} •
            ${age} •
            <button onclick=${(event) => toggleLike(event)}
                    class="${likeAttr}" uk-icon="icon: heart"></button>
              <span>${post.likers.length}</span></span>
            ${replies}
          </div>
          <!-- Buttons -->
          <div>
            ${deleteButton}
            <button onclick=${(event) => AddReplyPost.gotoAddReplyPage(event, post.id, true)}
                    class="toolbar-button" uk-icon="reply" uk-tooltip="Reply"></button>
          </div>
        </div>
        <hr class="uk-margin-small post-divider">
      </div>`;
  }

  function _addHasBeenReadObservers() {
    // Add an observer to each node that has the class name "post-divider"
    const postDividers = document.getElementsByClassName("post-divider");
    let firstUnreadPostFound = false;
    let postElement = null;
    for (const postDivider of postDividers) {
      postElement = postDivider.closest("[data-post-id]");
      const postId = postElement.getAttribute("data-post-id");
      const isRead = postElement.getAttribute("data-is-read");
      if (isRead == "false") {
        // Add observer to unread post
        _addHasBeenReadObserver(postDivider);
        // Mark the first unread post
        if (!firstUnreadPostFound) {
          console.log(`${postId} is the first unread post`);
          postElement.setAttribute("id", "unread-post");
          firstUnreadPostFound = true;
        }
      } else {
        // Mark it as read in the UI
        const metaDataElement = postElement.querySelector(".meta-data");
        const hasBeenReadElement = metaDataElement.children[0];
        hasBeenReadElement.hidden = true;
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

  function _addHasBeenReadObserver(postDivider) {
    const options = {
      root: null, // Uses the viewport as the root
      rootMargin: "0px", // No margin adjustments; full viewport
      threshold: 0 // Trigger when any part is visible
    };
    const callback = (entries, observer) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          const postElement = postDivider.closest("[data-post-id]");
          const postId = postElement.getAttribute("data-post-id");
          console.log(`${postId} has been read`);
          Bespoke.markMessageAsRead(postId);
          observer.unobserve(entry.target);
        }
      });
    };
    const observer = new IntersectionObserver(callback, options);
    observer.observe(postDivider);
  }
  */

  function openDeleteDialog(event) {
    Bespoke.ignoreEvent(event);

    // Extract message to delete
    const messageElement = event.currentTarget.closest("[data-message-id]");
    _messageIdToDelete = Number(messageElement.getAttribute("data-message-id"));

    // Update delete dialog
    const replyMessage = _replyMessages.find(
      replyMessage => replyMessage.id === _messageIdToDelete
    );
    const author = replyMessage != null ? replyMessage.authorUsername : "this message";
    _deleteMessageBodyElement.innerHTML =
      `Do you really want to delete this message written by ${author}?`;

    UIkit.modal("#delete-message-dialog").show();
  }

  function deleteMessage(event) {
    const updateServer = async () => {
      try {
        // REST API: Delete message
        const response = await fetch("/api/delete_message", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify(_messageIdToDelete)
        });
        if (!response.ok) {
          if (response.status === 401) {
            Bespoke.navigateTo("loader.html");
          } else {
            console.error(`Server error: ${response.status}`);
          }
          return;
        }

        // Is it parent message itself that has been deleted?
        if (_messageIdToDelete === _parentMessage.id) {
          Bespoke.gotoPage(event, "top_messages.html");
        } else {
          Bespoke.gotoPage(event, "message.html");
        }
        _messageIdToDelete = null;
      } catch (error) {
        console.error("Deletion of message failed:", error);
      }
    };

    updateServer();
  }

  async function _generateAttachments() {
    let attachments = "";
    if (_parentMessage.attachmentIds.length > 0) {
      const items = [];
      for (const attachmentId of _parentMessage.attachmentIds) {
        // Fetch attachment metadata
        const metadataResponse =
              await fetch(`/message/${_parentMessage.id}/${_userId}-${attachmentId}.dat`);
        if (!metadataResponse.ok) {
          console.error(`Server error: ${metadataResponse.status}`);
          if (metadataResponse.status === 401) {
            Bespoke.navigateTo("loader.html");
          }
          return;
        }
        const metadataBlob = await metadataResponse.blob();
        const unblobifiedMetadata = await Crypto.unblobifyData(_userId, metadataBlob);
        const metadata =
              JSON.parse(new TextDecoder().decode(await unblobifiedMetadata.arrayBuffer()));

        // Fetch attachment
        let objectUrl = null;
        const attachmentAbsPath = `/message/${_parentMessage.id}/${_userId}-${attachmentId}`;
        if (metadata.contentType.startsWith("image/")) {
          const attachmentResponse = await fetch(attachmentAbsPath);
          if (!attachmentResponse.ok) {
            console.error(`Server error: ${attachmentResponse.status}`);
            if (attachmentResponse.status === 401) {
              Bespoke.navigateTo("loader.html");
            }
            return;
          }
          const attachmentBlob = await attachmentResponse.blob();
          const unblobifiedAttachment = await Crypto.unblobifyData(_userId, attachmentBlob);
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
          const aHref = `javascript:Message.downloadFile(event, "${attachmentAbsPath}",
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
    downloadFile,
    deleteMessage,
    openDeleteDialog
  };
})();
