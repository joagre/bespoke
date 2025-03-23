// -*- fill-column: 100 -*-

"use strict";

const Post = (function () {
  const {html, render} = uhtml;
  const _REFRESH_INTERVAL = 30000;
  let _isLoadingData = false;
  let _firstLoad = true;
  let _dataLoaded = false;
  let _domReady = false;
  let _refreshTimer = null;
  let _stickyHeader;
  let _mainContent;
  let _userId;
  let _parentPost;
  let _topPostTitle;
  let _replyPosts;
  let _postIdToDelete;

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
    _stickyHeader = document.querySelector(".sticky-header");
    _mainContent = document.getElementById("main-content");
    _userId = Bespoke.getCookieValue("userId");
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
      // REST: Read parent post
      const response = await fetch("/api/read_posts", {
        method: "POST",
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify([Bespoke.peekPostStack().postId])
      });
      if (!response.ok) {
        if (response.status == 401) {
          Bespoke.navigateTo("loader.html");
        }
        console.error(`Server error: ${response.status}`);
        return;
      }
      const result = await response.json();
      Bespoke.assert(result.length === 1, "Expected exactly one post");
      _parentPost = result[0];
      Bespoke.markPostAsRead(_parentPost.id);
      // REST: Read top post title (maybe)
      _topPostTitle = _parentPost.title;
      if (_topPostTitle == null) {
        const lookupPostsResponse = await fetch("/api/read_posts", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify([_parentPost.topPostId])
        });
        if (!lookupPostsResponse.ok) {
          if (lookupPostsResponse.status == 401) {
            Bespoke.navigateTo("loader.html");
          }
          console.error(`Server error: ${lookupPostsResponse.status}`);
          return;
        }
        const lookupPostsResult = await lookupPostsResponse.json();
        Bespoke.assert(lookupPostsResult.length === 1, "Expected exactly one post");
        _topPostTitle = lookupPostsResult[0].title;
      }
      // Possible to remove delete button if not author
      if (_parentPost.authorId !== Bespoke.getCookieValue("userId")) {
        document.getElementById("parent-delete").style.display = "none";
      }
      // REST: Read reply posts
      const lookupRecursivePostsResponse =
            await fetch("/api/read_recursive_posts", {
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
      if (_domReady) {
        _populatePage();
      }
      // Subscribe on changes
      const postIds = _replyPosts.map((post) => post.id);
      postIds.push(Bespoke.peekPostStack().postId);
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
    document.getElementById("head-title").textContent = headTitle;
    setTimeout(() => {
      // The timeout is needed to ensure the header has been rendered :-9
      const headerHeight = _stickyHeader.offsetHeight;
      _mainContent.style.paddingTop = headerHeight + "px";
    }, 100);
    // Populate header
    const headerTitle = postStackSize > 1 ? "Reply" : "Post";
    document.getElementById("header-title").textContent = headerTitle;
    // Insert username into title
    let username = Bespoke.getCookieValue("username");
    document.getElementById("title-username").textContent = username;
    if (postStackSize > 1) {
      document.getElementById("header-reply-level").textContent = `[level: ${postStackSize - 1}]`;
    } else {
      document.getElementById("header-reply-level").style.display = "none";
    }
    // Populate parent post
    document.getElementById("parent-post").setAttribute("data-post-id", _parentPost.id);
    document.getElementById("parent-title").innerText = _topPostTitle;
    const attachments = TopPosts.generateAttachments(_parentPost);
    if (attachments != "") {
      render(document.getElementById("parent-attachments"), html`${attachments}`);
      Bespoke.initLongClick();
    }
    Bespoke.formatMarkdown(document.getElementById("parent-body"), _parentPost.body);
    document.getElementById("parent-author").textContent = _parentPost.author;
    document.getElementById("parent-age").textContent =
      Bespoke.formatSecondsSinceEpoch(_parentPost.created);
    if (_parentPost.likers.includes(_userId)) {
      const button = document.getElementById("parent-like-icon");
      button.classList.add("liked");
      Bespoke.refreshUIKitIcon(button);
    }
    document.getElementById("parent-likes-count").textContent = _parentPost.likers.length;
    document.getElementById("parent-replies").textContent = _parentPost.replyCount;
    document.getElementById("parent-delete").setAttribute("data-post-id", _parentPost.id);
    // Populate replies
    document.body.hidden = false;
    const repliesContainer = document.getElementById("replies");
    const replyTemplates = _replyPosts.map((replyPost) =>
      _createReplyTemplate(_parentPost, replyPost, _replyPosts)
    );
    render(repliesContainer, html`${replyTemplates}`);
    Bespoke.refreshAllUIKitIcons();
    Bespoke.disableAllLinks(".reply-body");
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
  }

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
        const metaDataElement = postElement.querySelector('.meta-data');
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
      rootMargin: '0px', // No margin adjustments; full viewport
      threshold: 0 // Trigger when any part is visible
    };
    const callback = (entries, observer) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          const postElement = postDivider.closest("[data-post-id]");
          const postId = postElement.getAttribute("data-post-id");
          console.log(`${postId} has been read`);
          Bespoke.markPostAsRead(postId);
          observer.unobserve(entry.target);
        }
      });
    };
    const observer = new IntersectionObserver(callback, options);
    observer.observe(postDivider);
  }

  function openDeletePostDialog(event) {
    event.stopPropagation();
    // Extract post to delete
    const postElement = event.currentTarget.closest("[data-post-id]");
    _postIdToDelete = postElement.getAttribute("data-post-id");
    // Update the delete post dialog
    const replyPost = _replyPosts.find(
      (replyPost) => replyPost.id === _postIdToDelete
    );
    const postAuthor = replyPost ? replyPost.authorUsername : "this post";
    document.getElementById("delete-post-body").innerHTML =
      `Do you really want to delete this post written by ${postAuthor}?`;
    UIkit.modal("#delete-post-dialog").show();
  }

  function deletePost(event) {
    const updateServer = async () => {
      try {
        const postId =
              _postIdToDelete === Bespoke.peekPostStack().postId ? -1 :
              null;
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

  function toggleQuote(event) {
    Bespoke.ignoreEvent(event);
    const postElement = event.currentTarget.closest("[data-post-id]");
    const postId = postElement.getAttribute("data-post-id");
    const parentPostElement =
          event.currentTarget.closest("[data-parent-post-id]");
    const parentPostId = parentPostElement.getAttribute("data-parent-post-id");
    const replyQuote = document.getElementById(`reply-quote-${postId}`);
    const isHidden = replyQuote.hidden;
    const replyQuoteAction =
          document.getElementById(`reply-quote-action-${postId}`);
    if (isHidden) {
      const replyQuoteBody =
            document.getElementById(`reply-quote-body-${postId}`);
      const replyBody =
            document.getElementById(`reply-body-${parentPostId}`);
      replyQuoteBody.innerHTML = replyBody.innerHTML;
      replyQuote.hidden = false;
      replyQuoteAction.setAttribute("uk-icon", "chevron-up");
    } else {
      replyQuote.hidden = true;
      replyQuoteAction.setAttribute("uk-icon", "chevron-down");
    }
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
        // Update the heart icon state
        if (result.liked) {
          likeButton.classList.add("liked");
        } else {
          likeButton.classList.remove("liked");
        }
        Bespoke.refreshUIKitIcon(likeButton);
        // Update the likes count
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
