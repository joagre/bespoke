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
  let _repliesElement;

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

    // Get dialog element
    _deleteMessageBodyElement = document.getElementById("delete-message-body");

    // Get replies element
    _repliesElement = document.getElementById("replies");

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
      // Timeout is to ensure that header has been rendered :-9
      const headerHeight = _stickyHeaderElement.offsetHeight;
      _mainContentElement.style.paddingTop = headerHeight + "px";
    }, 100);

    // Populate header
    _titleUsernameElement.textContent = _username;

    // Populate parent message
    _parentMessageElement.setAttribute("data-message-id", _parentMessage.id);
    _parentTitleElement.innerText = _parentMessageTitle;
    const attachments = await TopMessages.generateAttachments(_userId, _parentMessage);
    if (attachments != "") {
      render(_parentAttachmentsElement, html`${attachments}`);
      Bespoke.initLongClick();
    }

    Bespoke.formatMarkdown(_parentBodyElement, _parentMessageBody);
    _parentAuthorElement.textContent = _parentMessage.authorUsername;
    _parentAgeElement.textContent = Bespoke.formatSecondsSinceEpoch(_parentMessage.created);
    _parentRepliesElement.textContent = _parentMessage.replyCount;
    _parentDeleteElement.setAttribute("data-message-id", _parentMessage.id);

    document.body.hidden = false;

    // Populate replies
    if (_replyMessages.length > 0) {
      const replyTemplates = _replyMessages.map(replyMessage =>
        _createReplyTemplate(_parentMessage, replyMessage, _replyMessages)
      );
      render(_repliesElement, html`${replyTemplates}`);
      Bespoke.refreshAllUIKitIcons();

      // Add observers to message-dividers
      _addHasBeenReadObservers();

      // Scroll to correct position on first load
      if (_firstLoad) {
        console.log("Scrolling to unread message");
        const unreadMessageElement = document.getElementById("unread-message");
        if (unreadMessageElement) {
          unreadMessageElement.scrollIntoView({
            behavior: "smooth",
            block: "start",
            inline: "nearest"
          });
        }
        _firstLoad = false;
      }
    }
  }

  function _createReplyTemplate(_parentPost, _post, _replyPosts) {
    return [];
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

  */

  function _addHasBeenReadObservers() {
    // Add an observer to each Element that has class "message-divider"
    const messageDividerElements = document.getElementsByClassName("message-divider");
    let firstUnreadMessageFound = false;
    let messageElement = null;
    for (const messageDividerElement of messageDividerElements) {
      messageElement = messageDividerElement.closest("[data-message-id]");
      const messageId = messageElement.getAttribute("data-message-id");
      const isRead = messageElement.getAttribute("data-is-read");
      if (isRead == "false") {
        // Add observer to unread message
        _addHasBeenReadObserver(messageDividerElement);
        // Mark first unread message
        if (!firstUnreadMessageFound) {
          console.log(`${messageId} is first unread message`);
          messageElement.setAttribute("id", "unread-message");
          firstUnreadMessageFound = true;
        }
      } else {
        // Mark it as read in UI
        const metaDataElement = messageElement.querySelector(".meta-data");
        const hasBeenReadElement = metaDataElement.children[0];
        hasBeenReadElement.hidden = true;
      }
    }
    // If all messages are read, mark last message as unread to make sure that last message is shown
    if (!firstUnreadMessageFound && messageElement != null) {
      const messageId = messageElement.getAttribute("data-message-id");
      console.log(`${messageId} is last message`);
      messageElement.setAttribute("id", "unread-message");
    }
  }

  function _addHasBeenReadObserver(messageDividerElement) {
    const options = {
      root: null, // Uses viewport as root
      rootMargin: "0px", // No margin adjustments; full viewport
      threshold: 0 // Trigger when any part is visible
    };
    const callback = (entries, observer) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          const messageElement = messageDividerElement.closest("[data-message-id]");
          const messageId = messageElement.getAttribute("data-message-id");
          console.log(`${messageId} has been read`);
          Bespoke.markMessageAsRead(messageId);
          observer.unobserve(entry.target);
        }
      });
    };
    const observer = new IntersectionObserver(callback, options);
    observer.observe(messageDividerElement);
  }

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
