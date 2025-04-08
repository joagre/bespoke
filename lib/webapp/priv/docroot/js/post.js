// -*- fill-column: 100 -*-

"use strict";

const Post = (function () {
  const {html, render} = uhtml;
  const _REFRESH_INTERVAL = 30000;
  const _userId = Bespoke.getCookieValue("userId");
  const _username = Bespoke.getCookieValue("username");
  const _currentPostId = Bespoke.peekPostStack().postId;

  let _isLoadingData = false;
  let _firstLoad = true;
  let _dataLoaded = false;
  let _domReady = false;
  let _refreshTimer = null;
  let _parentPost;
  let _topPostTitle;
  let _replyPosts;
  let _postIdToDelete;

  let _headTitleElement;
  let _headerTitleElement;
  let _headerReplyLevelElement;
  let _stickyHeaderElement;
  let _titleUsernameElement;
  let _mainContentElement;
  let _parentPostElement;
  let _parentTitleElement;
  let _parentBodyElement;
  let _parentAttachmentsElement;
  let _parentAuthorElement;
  let _parentAgeElement;
  let _parentLikeIconElement;
  let _parentLikesCountElement;
  let _parentHaveRepliesElement;
  let _parentRepliesElement;
  let _parentDeleteElement;
  let _deletePostBodyElement;
  let _repliesElement;

  window.addEventListener("beforeunload", () => {
    if (_refreshTimer != null) {
      clearInterval(_refreshTimer);
    }
  });

  if (window.location.pathname == "/post.html") {
    _load();
  }

  function _load() {
    if (!Bespoke.hasSessionId() || Bespoke.isPostStackEmpty()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get document head title element
    _headTitleElement = document.getElementById("head-title");

    // Get header elements
    _stickyHeaderElement = document.querySelector(".sticky-header");
    _headerTitleElement = document.getElementById("header-title");
    _headerReplyLevelElement = document.getElementById("header-reply-level");
    _titleUsernameElement = document.getElementById("title-username");

    // Get main content element
    _mainContentElement = document.getElementById("main-content");

    // Get parent post elements
    _parentPostElement = document.getElementById("parent-post");
    _parentTitleElement = document.getElementById("parent-title");
    _parentBodyElement = document.getElementById("parent-body");
    _parentAttachmentsElement = document.getElementById("parent-attachments");
    _parentAuthorElement = document.getElementById("parent-author");
    _parentAgeElement = document.getElementById("parent-age");
    _parentLikeIconElement = document.getElementById("parent-like-icon");
    _parentLikesCountElement = document.getElementById("parent-likes-count")
    _parentHaveRepliesElement = document.getElementById("parent-have-replies");
    _parentRepliesElement = document.getElementById("parent-replies");
    _parentDeleteElement = document.getElementById("parent-delete");

    // Get dialog element
    _deletePostBodyElement = document.getElementById("delete-post-body");

    // Get replies element
    _repliesElement = document.getElementById("replies");

    // Add DOMContentLoaded event listener to populate page
    // Note: _dataLoaded may be set by _loadData() before DOMContentLoaded triggers (by design)
    document.addEventListener("DOMContentLoaded", () => {
      _domReady = true;
      if (_dataLoaded) {
        _populatePage();
      }
    });

    // Start refresh timer
    _refreshTimer = setInterval(() => _loadData(), _REFRESH_INTERVAL);

    // Dynamically load data
    _loadData();
  }

  async function _loadData() {
    if (_isLoadingData) {
      return;
    }

    _isLoadingData = true;
    try {
      // REST: Read parent post
      const readParentPostResult = await PostLib.readPost(_currentPostId);
      if (!readParentPostResult.ok) {
        return;
      }
      _parentPost = readParentPostResult.post;

      // Mark parent post as read
      Bespoke.markPostAsRead(_currentPostId);

      // REST: Read top post title (maybe)
      _topPostTitle = _parentPost.title;
      if (_topPostTitle == null) {
        const readTopPostResult = await PostLib.readPost(_parentPost.topPostId)
        if (!readTopPostResult.ok) {
          return;
        }
        const topPost = readTopPostResult.post;
        _topPostTitle = topPost.title;
      }

      // Remove delete button if not author
      if (_parentPost.authorId !== _userId) {
        _parentDeleteElement.style.display = "none";
      }

      // REST: Read reply posts
      const lookupRecursivePostsResponse = await fetch("/api/read_recursive_posts", {
        method: "POST",
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify(_parentPost.replies)
      });
      if (!lookupRecursivePostsResponse.ok) {
        if (lookupRecursivePostsResponse.status == 401) {
          Bespoke.navigateTo("loader.html");
        }
        console.error(`Server error: ${lookupRecursivePostsResponse.status}`);
        return;
      }
      _replyPosts = await lookupRecursivePostsResponse.json();

      _dataLoaded = true;

      // Populate page if DOM is ready
      if (_domReady) {
        _populatePage();
      }

      // Subscribe on changes
      const postIds = _replyPosts.map(post => post.id);
      postIds.push(_currentPostId);
      Bespoke.subscribeOnChanges(postIds, () => _loadData());
    } catch (error) {
      console.error("Loading of data failed:", error);
    } finally {
      _isLoadingData = false;
    }
  }

  function _populatePage() {
    const postStackSize = Bespoke.postStackSize();
    // Populate head title
    const headTitle = postStackSize > 1 ? "Reply" : "Post";
    _headTitleElement.textContent = headTitle;

    // Adjust padding of main content
    setTimeout(() => {
      // Timeout is to ensure that header has been rendered :-9
      const headerHeight = _stickyHeaderElement.offsetHeight;
      _mainContentElement.style.paddingTop = headerHeight + "px";
    }, 10);

    document.body.hidden = false;

    // Populate header
    const headerTitle = postStackSize > 1 ? "Reply" : "Post";
    _headerTitleElement.textContent = headerTitle;
    _titleUsernameElement.textContent = _username;
    if (postStackSize > 1) {
      _headerReplyLevelElement.textContent = `[level: ${postStackSize - 1}]`;
    } else {
      _headerReplyLevelElement.style.display = "none";
    }

    // Populate parent post
    _parentPostElement.setAttribute("data-post-id", _parentPost.id);
    _parentTitleElement.innerText = _topPostTitle;
    const attachments = PostLib.generateAttachments(_parentPost);
    if (attachments != "") {
      render(_parentAttachmentsElement, html`${attachments}`);
    }
    Bespoke.formatMarkdown(_parentBodyElement, _parentPost.body);
    _parentAuthorElement.textContent = _parentPost.authorUsername;
    _parentAgeElement.textContent =
      Bespoke.formatSecondsSinceEpoch(_parentPost.created);
    if (_parentPost.likers.includes(_userId)) {
      _parentLikeIconElement.classList.add("liked");
      Bespoke.refreshUIKitIcon(_parentLikeIconElement);
    }
    _parentLikesCountElement.textContent = _parentPost.likers.length;
    if (_parentPost.replyCount == 0) {
      _parentHaveRepliesElement.hidden = true;
    } else {
      _parentHaveRepliesElement.hidden = false;
      _parentRepliesElement.textContent = _parentPost.replyCount;
    }
    _parentDeleteElement.setAttribute("data-post-id", _parentPost.id);

    // Populate replies
    const replyTemplates = _replyPosts.map(replyPost =>
      _createReplyTemplate(_parentPost, replyPost, _replyPosts)
    );
    render(_repliesElement, html`${replyTemplates}`);
    Bespoke.refreshAllUIKitIcons();
    Bespoke.disableAllLinks(".reply-body");

    // Add observers to post-dividers
    HasBeenReadDivider.createObservers("post");

    // Scroll to correct position on first load
    if (_firstLoad) {
      if (Bespoke.getLocalItem("childPost")) {
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
  }

  function _createReplyTemplate(parentPost, post, replyPosts) {
    // Reply quote (only added if someone replies to a reply)
    let replyQuote = "";
    if (post.parentPostId != null &&
        post.parentPostId != parentPost.id) {
      const replyQuoteActionAttr = `reply-quote-action-${post.id}`;
      const replyQuoteAttr = `reply-quote-${post.id}`;
      const replyQuoteBodyAttr = `reply-quote-body-${post.id}`;
      const parentReplyPost = replyPosts.find(
        replyPost => replyPost.id == post.parentPostId
      );
      const replyQuoteAuthor =
            parentReplyPost ? parentReplyPost.authorUsername : "Unknown";
      replyQuote = html`
        <div class="uk-text-meta">
          <div class="quote" onclick=${event => toggleQuote(event)} uk-tooltip="Quote">
            <span id="${replyQuoteActionAttr}" class="uk-icon-link" uk-icon="chevron-right"></span>
            In reply to ${replyQuoteAuthor}...
          </div>
          <div id="${replyQuoteAttr}"
               class="uk-card uk-card-body uk-card-default uk-card-small uk-text-meta
                      uk-margin-small-bottom quote-card" hidden>
            <div id="${replyQuoteBodyAttr}" class="quote-body">
              <p>Loading...</p>
            </div>
          </div>
        </div>`;
    }

    // Reply body
    const replyBodyAttr = `reply-body-${post.id}`;

    // Author
    const author = html`<span uk-tooltip="Author">${post.authorUsername}</span>`;

    // Age
    const age = html`
      <span uk-tooltip="Created">${Bespoke.formatSecondsSinceEpoch(post.created)}</span>`;

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
        <button onclick=${event => Bespoke.gotoPage(event, "/post.html", post.id)}
                class="toolbar-button" uk-icon="comment" uk-tooltip="Replies"></button>
        ${post.replyCount}`;
    }

    // Delete button
    let deleteButton = "";
    if (post.authorId === _userId) {
      deleteButton = html`
        <button onclick=${event => openDeletePostDialog(event)}
                class="toolbar-button" uk-icon="trash" uk-tooltip="Delete"></button>`;
    }

    // Generate attachments
    const attachments = PostLib.generateAttachments(post);

    return html`
      <div data-post-id="${post.id}"
           data-is-read="${post.isRead}"
           data-parent-post-id="${post.parentPostId}">
        ${replyQuote}
        <div id="${replyBodyAttr}" class="reply-body uk-text-break">
          ${Bespoke.uhtmlFormatMarkdown(post.body)}
        </div>
        ${attachments}
        <div class="uk-flex uk-flex-between uk-flex-middle">
          <div class="uk-text-meta post-meta-data meta-data">
            <span class="unread" uk-icon="bolt" uk-tooltip="Unread"></span>
            ${author} •
            ${age} •
            <button onclick=${event => toggleLike(event)}
                    class="${likeAttr}" uk-icon="icon: heart"></button>
              <span>${post.likers.length}</span></span>
            ${replies}
          </div>
          <div>
            ${deleteButton}
            <button onclick=${event => AddReplyPost.gotoAddReplyPage(event, post.id, true)}
                    class="toolbar-button" uk-icon="reply" uk-tooltip="Reply"></button>
          </div>
        </div>
        <hr class="uk-margin-small post-divider">
      </div>`;
  }

  function toggleQuote(event) {
    Bespoke.ignoreEvent(event);

    // Extract current post id
    const postElement = event.currentTarget.closest("[data-post-id]");
    const postId = postElement.getAttribute("data-post-id");

    // Extract current parent post id
    const parentPostElement = event.currentTarget.closest("[data-parent-post-id]");
    const parentPostId = parentPostElement.getAttribute("data-parent-post-id");

    // Toggle visibility of reply quote
    const replyQuoteElement = document.getElementById(`reply-quote-${postId}`);
    const replyQuoteActionElement = document.getElementById(`reply-quote-action-${postId}`);
    if (replyQuoteElement.hidden) {
      // Copy parent message body to reply quote body
      const replyQuoteBody = document.getElementById(`reply-quote-body-${postId}`);
      const replyBodyElement = document.getElementById(`reply-body-${parentPostId}`);
      replyQuoteBody.innerHTML = replyBodyElement.innerHTML;
      replyQuoteElement.hidden = false;
      replyQuoteActionElement.setAttribute("uk-icon", "chevron-down");
    } else {
      replyQuoteElement.hidden = true;
      replyQuoteActionElement.setAttribute("uk-icon", "chevron-right");
    }
  }

  function openDeletePostDialog(event) {
    event.stopPropagation();

    // Extract post to delete
    const postElement = event.currentTarget.closest("[data-post-id]");
    _postIdToDelete = postElement.getAttribute("data-post-id");

    // Update delete dialog
    const replyPost = _replyPosts.find(
      replyPost => replyPost.id === _postIdToDelete
    );
    const postAuthor = replyPost ? replyPost.authorUsername : "you";
    _deletePostBodyElement.innerHTML =
      `Do you really want to delete this post written by ${postAuthor}?`;

    UIkit.modal("#delete-post-dialog").show();
  }

  function deletePost(event) {
    const updateServer = async () => {
      try {
        const postId = _postIdToDelete === _currentPostId ? -1 : null;
        // REST API: Delete post
        const response = await fetch("/api/delete_post", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify(_postIdToDelete)
        });
        if (!response.ok) {
          if (response.status === 401) {
            Bespoke.navigateTo("loader.html");
          } else {
            console.error(`Server error: ${response.status}`);
          }
          return;
        }
        if (postId != -1) {
          Bespoke.updatePostStackTopPosition();
        }
        Bespoke.gotoPage(event, "post.html", postId);
        _postIdToDelete = null;
      } catch (error) {
        console.error("Deletion of post failed:", error);
      }
    };

    updateServer();
  }

  function toggleLike(event) {
    Bespoke.ignoreEvent(event);
    const likeButton = event.currentTarget;
    const updateServer = async () => {
      try {
        const postElement = likeButton.closest("[data-post-id]");
        const postId = postElement.getAttribute("data-post-id");
        // REST API: Like post
        const response = await fetch("/api/toggle_post_like", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify(postId)
        });
        if (!response.ok) {
          if (response.status === 401) {
            Bespoke.navigateTo("loader.html");
          }
          console.error(`Server error: ${response.status}`);
          return;
        }
        const result = await response.json();

        // Update heart icon state
        if (result.liked) {
          likeButton.classList.add("liked");
        } else {
          likeButton.classList.remove("liked");
        }
        Bespoke.refreshUIKitIcon(likeButton);

        // Update likes count
        likeButton.nextSibling.innerText = result.likesCount;
      } catch (error) {
        console.error("Like post failed:", error);
      }
    };

    updateServer();
  }

  return {
    deletePost,
    openDeletePostDialog,
    toggleQuote,
    toggleLike
  };
})();
