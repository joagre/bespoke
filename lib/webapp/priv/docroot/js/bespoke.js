// -*- fill-column: 100 -*-

"use strict";

const Bespoke = (function () {
  const {html, render} = uhtml;
  const _LONG_CLICK_DURATION = 500;

  let _cookieState = null;
  let _touch = {
    startX: 0,
    endX: 0,
    startY: 0,
    endY: 0
  };
  let _SWIPE_THRESHOLD;
  let _VERTICAL_THRESHOLD;
  let _subscriptionController = null;
  let _initializedElements = new WeakSet();

  _initSwipeListeners();
  _initCookieState();
  _initSubscription();
  _markMessagesAsRead();
  _markPostsAsRead();
  _addCustomIcons();

  function _initSwipeListeners() {
    _SWIPE_THRESHOLD = window.innerWidth * 0.75;
    _VERTICAL_THRESHOLD = window.innerHeight * 0.75;
    document.addEventListener("touchstart", event => _onTouchStart(event), {passive: false});
    document.addEventListener("touchend", event => _onTouchEnd(event), {passive: false});
  }

  function _onTouchStart(event) {
    _touch.startX = event.changedTouches[0].screenX;
    _touch.startY = event.changedTouches[0].screenY;
  }

  function _onTouchEnd(event) {
    _touch.endX = event.changedTouches[0].screenX;
    _touch.endY = event.changedTouches[0].screenY;
    _handleSwipeGesture();
  }

  function _handleSwipeGesture() {
    const horizontalSwipe = _touch.endX - _touch.startX;
    const verticalSwipe = Math.abs(_touch.endY - _touch.startY);
    if (horizontalSwipe > _SWIPE_THRESHOLD && verticalSwipe < _VERTICAL_THRESHOLD) {
      _triggerSwipeNavigation();
    }
  }

  function _triggerSwipeNavigation() {
    const swipeTarget = document.querySelector("[data-back-destination]");
    if (swipeTarget) {
      const destination = swipeTarget.getAttribute("data-back-destination");
      if (destination === "") {
        gotoPreviousPage();
      } else if (destination === "post.html") {
        gotoPage(null, destination, -1);
      } else {
        gotoPage(null, destination);
      }
    }
  }

  function _initCookieState() {
    _cookieState = _getCookie("bespoke");
    if (_cookieState == null) {
      _cookieState = {};
    }
  }

  function _getCookie(name) {
    const value = `; ${document.cookie}`;
    const parts = value.split(`; ${name}=`);
    if (parts.length === 2) {
      try {
        return JSON.parse(decodeURIComponent(parts.pop().split(";").shift()));
      } catch (e) {
        console.error("Error parsing Bespoke cookie:", e);
      }
    }
    return null;
  }

  function _updateCookieState() {
    _setCookie("bespoke", _cookieState, 7);
  }

  function _setCookie(name, value, days = 7) {
    let expires = "";
    if (days) {
      const date = new Date();
      date.setTime(date.getTime() + days * 24 * 60 * 60 * 1000);
      expires = `; expires=${date.toUTCString()}`;
    }
    const cookieValue = typeof value === "object" ? JSON.stringify(value) : value;
    document.cookie =
      `${name}=${encodeURIComponent(cookieValue)}${expires}; path=/; SameSite=Strict`;
  }

  function _initSubscription() {
    _cancelSubscription();
    window.addEventListener("beforeunload", () => {
      _cancelSubscription();
    });
  }

  function _cancelSubscription() {
    if (_subscriptionController != null) {
      try {
        console.log("Cancelling subscription");
        _subscriptionController.abort();
      } catch (error) {
        console.error("Subscription cancellation failed:", error);
      } finally {
        _subscriptionController = null;
      }
    }
  }

  function onReady(relevantPage, callback) {
    if (relevantPage === true ||
        window.location.pathname.endsWith(relevantPage)) {
      if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", () => callback());
      } else {
        callback();
      }
    }
  }

  function gotoPage(event, destination, postId) {
    ignoreEvent(event);

    // Prevent navigation in uk-lightboxes
    if (event != null && (event.target.tagName === "IMG" || event.target.tagName === "DIV")) {
      const lightboxDiv = event.target.closest("div.long-click");
      if (lightboxDiv) {
        return;
      }
    }

    // Prevent navigation if text is selected
    //if (_isTextSelected()) {
    //  return;
    //}

    // -1 is a special value to indicate that the post stack should be popped
    if (postId === -1) {
      popPostStack();
    } else if (typeof postId === "string") {
      _pushPostStack(postId);
    } else if (typeof postId === "function") {
      postId();
    }

    // If destination is not set, go back to the previous page
    if (destination == null) {
      gotoPreviousPage();
      return;
    }

    // If the post stack is empty, go to the top posts page
    if (destination === "post.html" && isPostStackEmpty()) {
      navigateTo("top_posts.html");
    } else {
      // If the destination is the post page, set the child post flag
      if (destination === "post.html") {
        if (typeof postId === "string") {
          setLocalItem("childPost", true);
        } else {
          setLocalItem("childPost", false);
        }
      }
      navigateTo(destination);
    }
  }

  function ignoreEvent(event) {
    if (event != null) {
      if (event.target.tagName === "BUTTON" || event.target.closest("button")) {
        event.stopPropagation();
      }
      event.preventDefault();
    }
  }

  function _isTextSelected() {
    const selection = window.getSelection();
    return selection && selection.toString().length > 0;
  }

  function isPostStackEmpty() {
    return postStackSize() === 0;
  }

  function setCookieValue(name, value) {
    _cookieState[name] = value;
    _updateCookieState();
  }

  function hasSessionId() {
    if (_cookieState != null) {
      const sessionId = getCookieValue("sessionId");
      return sessionId != null;
    }
    return false;
  }

  function getCookieValue(name) {
    return _cookieState[name];
  }

  function setLocalItem(key, value) {
    localStorage.setItem(key, JSON.stringify(value));
  }

  function getLocalItem(key, defaultValue) {
    const value = getRawLocalItem(key);
    if (value == null) {
      if (defaultValue !== undefined) {
        return defaultValue;
      }
      assert(false, `A local item is missing (${key})`);
    } else {
      return JSON.parse(value);
    }
  }

  function setRawLocalItem(key, value) {
    localStorage.setItem(key, value);
  }

  function clearLocalItem(key) {
    localStorage.removeItem(key);
  }

  function getRawLocalItem(key, defaultValue) {
    const value = localStorage.getItem(key);
    if (value == null && defaultValue !== undefined) {
      return defaultValue;
    }
    return value;
  }

  function clearPostStack() {
    setLocalItem("postStack", []);
  }

  function _pushPostStack(postId) {
    assert(postId != null, "postId is not set");
    let postStack = getLocalItem("postStack");
    assert(postStack != null, "Post stack not found");
    if (postStack.length > 0) {
      postStack.slice(-1)[0].scrollX = window.scrollX;
      postStack.slice(-1)[0].scrollY = window.scrollY;
    }
    const postData = {postId: postId, scrollX: 0, scrollY: 0};
    postStack.push(postData);
    setLocalItem("postStack", postStack);
  }

  function updatePostStackTopPosition() {
    let postStack = getLocalItem("postStack");
    assert(postStack != null, "Post stack not found");
    postStack.slice(-1)[0].scrollX = window.scrollX;
    postStack.slice(-1)[0].scrollY = window.scrollY;
    setLocalItem("postStack", postStack);
  }

  function popPostStack() {
    let postStack = getLocalItem("postStack");
    assert(postStack != null, "Post stack not found");
    assert(postStack.length > 0, "Post stack is empty");
    postStack.pop();
    setLocalItem("postStack", postStack);
  }

  function peekPostStack() {
    let postStack = getLocalItem("postStack");
    assert(postStack != null, "Post stack not found");
    assert(postStack.length > 0, "Post stack is empty");
    return postStack.slice(-1)[0];
  }

  function postStackSize() {
    const postStack = getLocalItem("postStack");
    assert(postStack != null, "Post stack not found");
    return postStack.length;
  }

  function truncatePostStack(length) {
    const postStack = getLocalItem("postStack");
    assert(postStack != null, "Post stack not found");
    setLocalItem("postStack", postStack.slice(0, length));
  }

  function formatBytes(bytes) {
    if (bytes === 0) {
      return "0B";
    }
    const k = 1024;
    const sizes = ["B", "KB", "MB", "GB", "TB"];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return parseFloat((bytes / Math.pow(k, i)).toFixed(1)) + sizes[i];
  }

  function formatSecondsSinceEpoch(secondsSinceEpoch) {
    const now = Math.floor(Date.now() / 1000);
    const ageInSeconds = now - secondsSinceEpoch;
    if (ageInSeconds < 60) {
      return `${ageInSeconds}s`;
       }
    if (ageInSeconds < 3600) {
      return `${Math.floor(ageInSeconds / 60)}m`;
    }
    if (ageInSeconds < 86400) {
      return `${Math.floor(ageInSeconds / 3600)}h`;
    }
    if (ageInSeconds < 604800) {
      return `${Math.floor(ageInSeconds / 86400)}d`;
    }
    if (ageInSeconds < 2419200) {
      return `${Math.floor(ageInSeconds / 604800)}w`;
    }
    if (ageInSeconds < 29030400) {
      return `${Math.floor(ageInSeconds / 2592000)}mo`;
    }
    return `${Math.floor(ageInSeconds / 31536000)}y`;
  }

  function _disableLinks(element) {
    const links = element.querySelectorAll("a");
    links.forEach(link => {
      if (!link.hasAttribute("data-disabled")) {
        link.removeAttribute("href");
        link.setAttribute("title", "This link is disabled");
        link.style.cursor = "not-allowed";
        link.addEventListener("click", event => event.preventDefault());
        link.setAttribute("data-disabled", "true");
      }
    });
  }

  function disableAllLinks(selector) {
    const elements = document.querySelectorAll(selector);
    elements.forEach(element => _disableLinks(element));
  }

  function formatMarkdown(element, content) {
    const purifiedContent = DOMPurify.sanitize(content);
    element.innerHTML = marked.parse(purifiedContent);
    _disableLinks(element);
  }

  function uhtmlFormatMarkdown(content) {
    const purifiedContent = DOMPurify.sanitize(content);
    return html`${html([marked.parse(purifiedContent)])}`;
  }

  function assert(condition, post) {
    if (!condition) {
      throw new Error(post || "Assertion failed");
    }
  }

  function navigateTo(destination) {
    setRawLocalItem("previousPage", window.location.pathname);
    window.location.href = destination;
  }

  function getPreviousPage() {
    const previousPage = getRawLocalItem("previousPage", "");
    return previousPage;
  }

  function gotoPreviousPage() {
    const previousPage = getPreviousPage();
    if (previousPage === "") {
      console.error("No previous page found");
      return;
    }
    navigateTo(previousPage);
  }

  function generateStrongPassword() {
    const length = 12;
    const charset =
          "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()_+~`|}{[]:;?><,./-=";
    let password = "";
    for (let i = 0; i < length; i++) {
      const at = Math.floor(Math.random() * charset.length);
      password += charset.charAt(at);
    }
    return password;
  }

  async function subscribeOnChanges(postIds, callback) {
    try {
      console.log("Subscribing on changes");
      _cancelSubscription();
      _subscriptionController = new AbortController();
      const signal = _subscriptionController.signal;
      const response = await fetch("/api/subscribe_on_changes", {
        method: "POST",
        headers: {
          "Connection": "close",
          "Content-Type": "application/json",
        },
        body: JSON.stringify(postIds),
        signal
      });
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        console.log("Retrying in 5 seconds");
        setTimeout(() => subscribeOnChanges(postIds, callback), 5000);
        return;
      }
      const postId = await response.json();
      console.log(`${postId} has changed`);
      callback();
    } catch (error) {
      console.log("Subscribe on changes failed:", error);
    } finally {
      _cancelSubscription();
    }
  }

  function decodeBase64(base64) {
    const binaryString = atob(base64);
    return new Uint8Array([...binaryString].map(char => char.charCodeAt(0)));
  }

  function encodeBase64(bytes) {
    return btoa(String.fromCharCode(...bytes));
  }

  function showLoadingSpinner() {
    document.getElementById("loading-overlay").removeAttribute("hidden");
  }

  function hideLoadingSpinner() {
    document.getElementById("loading-overlay").setAttribute("hidden", true);
  }

  function refreshAllUIKitIcons() {
    document.querySelectorAll("[uk-icon]").forEach(el => {
      refreshUIKitIcon(el);
    });
  }

  function refreshUIKitIcon(target) {
    UIkit.icon(target).$destroy();
    UIkit.icon(target);
  }

  function initMobileKeyboardResizing(id) {
    // Dynamically resize body when keyboard opens or closes
    const resizeBody = () => {
      const newHeight = window.visualViewport ? window.visualViewport.height : window.innerHeight;
      document.body.style.height = `${newHeight}px`;
    }
    if (window.visualViewport) {
      visualViewport.addEventListener("resize", resizeBody);
      visualViewport.addEventListener("scroll", resizeBody);
    } else {
      window.addEventListener("resize", resizeBody);
    }
    resizeBody();

    // Block body scrolling if "overscoll-behaivor: contain;" is unsupported
    document.addEventListener("touchmove", e => {
      const el = e.target.closest(id);
      if (el == null) {
        e.preventDefault();
        return;
      }
      const canScrollUp = el.scrollTop > 0;
      const canScrollDown = el.scrollTop + el.clientHeight < el.scrollHeight;
      if (!canScrollUp && !canScrollDown) {
        e.preventDefault();
      }
    }, {passive: false});
  }

  function isTouchDevice() {
    const isTouchDevice =
          "ontouchstart" in window ||
          navigator.maxTouchPoints > 0 ||
          window.matchMedia("(pointer: coarse)").matches;
    return isTouchDevice;
  }

  function basename(path) {
    return path.slice(path.lastIndexOf("/") + 1);
  }

  function initLongClick() {
    if (isTouchDevice()) {
      return;
    }
    let clickTimer;
    let isLongClick = false;
    const longClickImages =
          document.querySelectorAll(".long-click img, .long-click video, .long-click div");
    longClickImages.forEach(img => {
      img.addEventListener("contextmenu", e => {
        e.preventDefault();
      });
      if (_initializedElements.has(img)) {
        return;
      }
      _initializedElements.add(img);
      const parentAnchor = img.closest("a");
      const downloadUrl = parentAnchor.getAttribute("href");
      const basename = Bespoke.basename(downloadUrl);
      img.addEventListener("mousedown", e => {
        isLongClick = false;
        clickTimer = setTimeout(() => {
          console.log("Long-click detected");
          showNotification(`Downloading ${basename}`);
          isLongClick = true;
          const anchor = document.createElement("a");
          anchor.href = downloadUrl;
          anchor.download = "";
          anchor.click();
        }, _LONG_CLICK_DURATION);
      });
      img.addEventListener("mouseup", () => clearTimeout(clickTimer));
      img.addEventListener("mouseleave", () => clearTimeout(clickTimer));
      parentAnchor.addEventListener("click", e => {
        if (isLongClick) {
          e.preventDefault();
        }
      });
    });
  }

  function showNotification(content, status = 'primary') {
   const notificationContent = `
      <div class="uk-card uk-card-small uk-card-default uk-card-body uk-border-rounded uk-box-shadow-small">
        ${content}
      </div>`;
    UIkit.notification(notificationContent, {
      pos: "bottom-center",
      status,
    });
  }

  function markMessageAsRead(messageId) {
    const readMessages = getLocalItem("readMessages");
    readMessages.push(messageId);
    console.log("Marking message as read:", messageId);
    setLocalItem("readMessages", readMessages);
  }

  function _markMessagesAsRead() {
    const readMessages = getLocalItem("readMessages", []);
    if (readMessages.length === 0) {
      setLocalItem("readMessages", readMessages);
      return;
    }

    // REST: Mark messages as read
    console.log("Mark messages as read:", readMessages);
    fetch("/api/mark_messages_as_read", {
      method: "POST",
      body: JSON.stringify(readMessages),
      headers: {"Content-Type": "application/json"}
    }).catch(err => console.error("Fetch failed:", err));
    setLocalItem("readMessages", []);
  }

  function markPostAsRead(postId) {
    const readPosts = getLocalItem("readPosts");
    readPosts.push(postId);
    console.log("Marking post as read:", postId);
    setLocalItem("readPosts", readPosts);
  }

  function _markPostsAsRead() {
    const readPosts = getLocalItem("readPosts", []);
    if (readPosts.length === 0) {
      setLocalItem("readPosts", readPosts);
      return;
    }

    // REST: Mark posts as read
    console.log("Mark posts as read:", readPosts);
    fetch("/api/mark_posts_as_read", {
      method: "POST",
      body: JSON.stringify(readPosts),
      headers: {"Content-Type": "application/json"}
    }).catch(err => console.error("Fetch failed:", err));
    setLocalItem("readPosts", []);
  }

  function _addCustomIcons() {
    const iconOk = `<svg version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" height="20px" width="20px" x="0px" y="0px" viewBox="0 0 256 256" enable-background="new 0 0 256 256" xml:space="preserve">
<metadata> Svg Vector Icons : http://www.onlinewebfonts.com/icon </metadata>
<g><g><path fill="#000000" d="M104.5,176.3c-8.8,9.1-21.6,13.7-38.4,13.7c-16.7,0-29.5-4.6-38.4-13.7C15.9,165.1,10,149,10,128c0-21.5,5.9-37.5,17.8-48.3C36.6,70.6,49.4,66,66.2,66c16.7,0,29.5,4.6,38.4,13.7c11.8,10.8,17.7,26.9,17.7,48.3C122.2,149,116.3,165.1,104.5,176.3z M89.3,158.5c5.7-7.2,8.5-17.3,8.5-30.5c0-13.1-2.8-23.3-8.5-30.5c-5.7-7.2-13.4-10.8-23.1-10.8c-9.7,0-17.4,3.6-23.2,10.7c-5.8,7.2-8.7,17.3-8.7,30.5c0,13.2,2.9,23.4,8.7,30.5s13.5,10.7,23.2,10.7C75.9,169.2,83.5,165.7,89.3,158.5z"/><path fill="#000000" d="M142.6,69.6h24.1v48.2l45.3-48.2h31.6l-48,48.2l50.5,68.9h-31.5l-36.1-51.1l-11.8,11.9v39.2h-24.1V69.6z"/></g></g>
</svg>`;
    UIkit.icon.add({
      "bespoke-ok": iconOk
    });
  }

  function isFileInputSupported() {
    const input = document.createElement("input");
    input.type = "file";
    document.body.appendChild(input);
    input.style.display = "none";
    let isFunctional = false;
    input.addEventListener("click", () => {
      isFunctional = true;
    });
    input.click();
    input.remove();
    return isFunctional;
  }

  function uploadData(data, filename, onProgress, extraHeaders) {
    const file = new File([data], filename, {type: "application/octet-stream"});
    return uploadFile(file, onProgress, extraHeaders);
  }

  function uploadFile(file, onProgress, extraHeaders) {
    // Note: XMLHTTPRequest is used instead of fetch(). This makes it possible to keep track of
    // upload progress, but it also ensures that the file is read/sent in chunks from disk.
    const formData = new FormData();
    formData.append("file", file, file.name);
    const xhr = new XMLHttpRequest();
    const xhrPromise = new Promise((resolve, reject) => {
      xhr.open("POST", "/api/upload_file");
      if (extraHeaders != null) {
        for (let i = 0; i < extraHeaders.length; i++) {
          const header = extraHeaders[i];
          xhr.setRequestHeader(header.key, header.value);
        }
      }
      xhr.onload = function () {
        if (xhr.status >= 200 && xhr.status < 300) {
          console.log("Upload successful");
          resolve(xhr);
        } else {
          console.error("Upload failed:", xhr.statusText);
          reject(new Error(xhr.statusText));
        }
      };
      xhr.onerror = function () {
        reject(new Error("Network error"));
      };
      xhr.upload.addEventListener("progress", function (e) {
        if (e.lengthComputable) {
          if (onProgress != null) {
            const percentComplete = (e.loaded / e.total) * 100;
            onProgress(e.total, e.loaded, percentComplete);
          }
        }
      });
      xhr.send(formData);
    });
    return {xhr, promise: xhrPromise};
  }

  function abortUpload(uploadController) {
    try {
      uploadController.xhr.abort();
    } catch (error) {
      console.error("Abort error:", error);
    }
  }

  async function waitForUpload(uploadController) {
    try {
      const xhr = await uploadController.promise;
      if (xhr.status === 200) {
        return JSON.parse(xhr.responseText);
      } else {
        throw new Error(`Upload failed: ${xhr.status} ${xhr.statusText}`);
      }
    } catch (error) {
      console.error("Upload error:", error);
      throw error;
    }
  }

  function pause(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }

  function truncateMiddle(str, maxLength = 20) {
    if (str.length <= maxLength) return str;

    const half = Math.floor((maxLength - 3) / 2); // Leave space for "..."
    return str.slice(0, half) + "..." + str.slice(-half);
  }

  return {
    onReady,
    gotoPage,
    ignoreEvent,
    isPostStackEmpty,
    setCookieValue,
    hasSessionId,
    getCookieValue,
    setLocalItem,
    getLocalItem,
    setRawLocalItem,
    clearLocalItem,
    getRawLocalItem,
    clearPostStack,
    updatePostStackTopPosition,
    popPostStack,
    peekPostStack,
    postStackSize,
    truncatePostStack,
    formatBytes,
    formatSecondsSinceEpoch,
    disableAllLinks,
    formatMarkdown,
    uhtmlFormatMarkdown,
    assert,
    navigateTo,
    getPreviousPage,
    gotoPreviousPage,
    generateStrongPassword,
    subscribeOnChanges,
    decodeBase64,
    encodeBase64,
    showLoadingSpinner,
    hideLoadingSpinner,
    refreshAllUIKitIcons,
    refreshUIKitIcon,
    initMobileKeyboardResizing,
    isTouchDevice,
    basename,
    initLongClick,
    markMessageAsRead,
    markPostAsRead,
    showNotification,
    isFileInputSupported,
    uploadData,
    uploadFile,
    abortUpload,
    waitForUpload,
    pause,
    truncateMiddle
  };
})();
