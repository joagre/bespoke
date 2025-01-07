import { marked } from "/js/marked.esm.js";

// Ensure uhtml.min.js is imported in the HTML file before this script
const { html, render } = uhtml;

class Bespoke {
  constructor() {
    this._cookieState = null;
    this._touch = {
      startX: 0,
      endX: 0,
      startY: 0,
      endY: 0,
    };
    this._LONG_CLICK_DURATION = 2000;
    this._initializeSwipeListeners();
    this._initializeCookieState();
    this._initializeSubscription();
    this._initializedElements = new WeakSet();
  }

  _initializeSwipeListeners() {
    this.SWIPE_THRESHOLD = window.innerWidth * 0.5;
    this.VERTICAL_THRESHOLD = window.innerHeight * 0.5;
    document.addEventListener(
      "touchstart",
      event => this._onTouchStart(event),
      {passive: false});
    document.addEventListener(
      "touchend",
      event => this._onTouchEnd(event),
      {passive: false}
    );
  }

  _onTouchStart(event) {
    this._touch.startX = event.changedTouches[0].screenX;
    this._touch.startY = event.changedTouches[0].screenY;
  }

  _onTouchEnd(event) {
    this._touch.endX = event.changedTouches[0].screenX;
    this._touch.endY = event.changedTouches[0].screenY;
    this._handleSwipeGesture();
  }

  _handleSwipeGesture() {
    const horizontalSwipe = this._touch.endX - this._touch.startX;
    const verticalSwipe = Math.abs(this._touch.endY - this._touch.startY);
    if (horizontalSwipe > this.SWIPE_THRESHOLD &&
        verticalSwipe < this.VERTICAL_THRESHOLD) {
      this._triggerSwipeNavigation();
    }
  }

  _triggerSwipeNavigation() {
    const swipeTarget = document.querySelector("[data-back-destination]");
    if (swipeTarget) {
      const destination = swipeTarget.getAttribute("data-back-destination");
      if (destination === "") {
        this.gotoPreviousPage();
      } else if (destination === "post.html") {
        this.gotoPage(null, destination, -1);
      } else {
        this.gotoPage(null, destination);
      }
    }
  }

  _initializeCookieState() {
    this._cookieState = this._getCookie("bespoke");
    if (this._cookieState == null) {
      this._cookieState = {};
    }
  }

  _getCookie(name) {
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

  _updateCookieState() {
    this._setCookie("bespoke", this._cookieState, 7);
  }

  _setCookie(name, value, days = 7) {
    let expires = "";
    if (days) {
      const date = new Date();
      date.setTime(date.getTime() + days * 24 * 60 * 60 * 1000);
      expires = `; expires=${date.toUTCString()}`;
    }
    const cookieValue =
          typeof value === "object" ? JSON.stringify(value) : value;
    document.cookie =
      `${name}=${encodeURIComponent(cookieValue)}${expires}; path=/; SameSite=Strict`;
  }

  _initializeSubscription() {
    this._subscriptionController = null;
    window.addEventListener("beforeunload", () => {
      this._cancelSubscription();
    });
  }

  _cancelSubscription() {
    if (this._subscriptionController != null) {
      this._subscriptionController.abort();
      this._subscriptionController = null;
    }
  }

  onReady(relevantPage, callback) {
    if (window.location.pathname.endsWith(relevantPage)) {
      if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", () => callback());
      } else {
        callback();
      }
    }
  }

  gotoPage(event, destination, postId) {
    this.ignoreEvent(event);
    // Prevent navigation if text is selected
    if (this._isTextSelected()) {
      return;
    }
    // -1 is a special value to indicate that the post stack should be popped
    if (postId === -1) {
      this.popPostStack();
    } else if (typeof postId === "string") {
      this._pushPostStack(postId);
    }
    // If destination is not set, go back to the previous page
    if (destination == null) {
      this.gotoPreviousPage();
      return;
    }
    // If the post stack is empty, go to the top posts page
    if (destination === "post.html" && this.isPostStackEmpty()) {
      this.navigateTo("top_posts.html");
    } else {
      // If the destination is the post page, set the child post flag
      if (destination === "post.html") {
        if (typeof postId === "string") {
          bespoke.setLocalItem("childPost", true);
        } else {
          bespoke.setLocalItem("childPost", false);
        }
      }
      this.navigateTo(destination);
    }
  }

  ignoreEvent(event) {
    if (event != null) {
      if (event.target.tagName === "BUTTON" || event.target.closest("button")) {
        event.stopPropagation();
      }
      event.preventDefault();
    }
  }

  _isTextSelected() {
    const selection = window.getSelection();
    return selection && selection.toString().length > 0;
  }

  isPostStackEmpty() {
    return this.postStackSize() === 0;
  }

  setCookieValue(name, value) {
    this._cookieState[name] = value;
    this._updateCookieState();
  }

  hasSessionId() {
    if (this._cookieState != null) {
      const sessionId = this.getCookieValue("sessionId");
      return sessionId != null;
    }
    return false;
  }

  getCookieValue(name) {
    return this._cookieState[name];
  }

  setLocalItem(key, value) {
    console.log("Setting local item:", key, value);
    localStorage.setItem(key, JSON.stringify(value));
  }

  getLocalItem(key, defaultValue) {
    const value = this.getRawLocalItem(key);
    if (value == null) {
      if (defaultValue !== undefined) {
        return defaultValue;
      }
      this.assert(false, "A local item is missing");
    } else {
      return JSON.parse(value);
    }
  }s

  setRawLocalItem(key, value) {
    console.log("Setting local item:", key, value);
    localStorage.setItem(key, value);
  }

  clearLocalItem(key) {
    localStorage.removeItem(key);
  }

  getRawLocalItem(key, defaultValue) {
    const value = localStorage.getItem(key);
    if (value == null && defaultValue !== undefined) {
      return defaultValue;
    }
    return value;
  }

  clearPostStack() {
    this.setLocalItem("postStack", []);
  }

  _pushPostStack(postId) {
    this.assert(postId != null, "postId is not set");
    let postStack = this.getLocalItem("postStack");
    this.assert(postStack != null, "Post stack not found");
    if (postStack.length > 0) {
      postStack.slice(-1)[0].scrollX = window.scrollX;
      postStack.slice(-1)[0].scrollY = window.scrollY;
    }
    const postData = {postId: postId, scrollX: 0, scrollY: 0};
    postStack.push(postData);
    this.setLocalItem("postStack", postStack);
  }

  updatePostStackTopPosition() {
    let postStack = this.getLocalItem("postStack");
    this.assert(postStack != null, "Post stack not found");
    postStack.slice(-1)[0].scrollX = window.scrollX;
    postStack.slice(-1)[0].scrollY = window.scrollY;
    this.setLocalItem("postStack", postStack);
  }

  popPostStack() {
    let postStack = this.getLocalItem("postStack");
    this.assert(postStack != null, "Post stack not found");
    this.assert(postStack.length > 0, "Post stack is empty");
    postStack.pop();
    this.setLocalItem("postStack", postStack);
  }

  peekPostStack() {
    let postStack = this.getLocalItem("postStack");
    this.assert(postStack != null, "Post stack not found");
    this.assert(postStack.length > 0, "Post stack is empty");
    return postStack.slice(-1)[0];
  }

  postStackSize() {
    const postStack = this.getLocalItem("postStack");
    this.assert(postStack != null, "Post stack not found");
    return postStack.length;
  }

  truncatePostStack(length) {
    const postStack = this.getLocalItem("postStack");
    this.assert(postStack != null, "Post stack not found");
    this.setLocalItem("postStack", postStack.slice(0, length));
  }

  formatSecondsSinceEpoch(secondsSinceEpoch) {
    const now = Math.floor(Date.now() / 1000);
    const ageInSeconds = now - secondsSinceEpoch;
    if (ageInSeconds < 60) return `${ageInSeconds}s`;
    if (ageInSeconds < 3600) return `${Math.floor(ageInSeconds / 60)}m`;
    if (ageInSeconds < 86400) return `${Math.floor(ageInSeconds / 3600)}h`;
    if (ageInSeconds < 604800) return `${Math.floor(ageInSeconds / 86400)}d`;
    if (ageInSeconds < 2419200) return `${Math.floor(ageInSeconds / 604800)}w`;
    if (ageInSeconds < 29030400)
      return `${Math.floor(ageInSeconds / 2592000)}mo`;
    return `${Math.floor(ageInSeconds / 31536000)}y`;
  }

  uhtmlFormatMarkdown(content) {
    // FIXME: Sanitize HTML
    return html`${html([marked.parse(content)])}`;
  }

  formatMarkdown(content) {
    // FIXME: Sanitize HTML
    return marked.parse(content);
  }

  assert(condition, post) {
    if (!condition) {
      throw new Error(post || "Assertion failed");
    }
  }

  navigateTo(destination) {
    this.setRawLocalItem("previousPage", window.location.pathname);
    window.location.href = destination;
  }

  getPreviousPage() {
    const previousPage = this.getRawLocalItem("previousPage", "");
    return previousPage;
  }

  gotoPreviousPage() {
    const previousPage = this.getPreviousPage();
    if (previousPage === "") {
      console.error("No previous page found");
      return;
    }
    this.navigateTo(previousPage);
  }

  generateStrongPassword() {
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

  async subscribeOnChanges(postIds, callback) {
    try {
      console.log("Subscribing on changes...");
      this._subscriptionController = new AbortController();
      const signal = this._subscriptionController.signal;
      const response = await fetch("/subscribe_on_changes", {
        method: "POST",
        headers: {
          "Conncection": "close",
          "Content-Type": "application/json",
        },
        body: JSON.stringify(postIds),
        signal
      });
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        console.log("Retrying in 5 seconds");
        setTimeout(() => this.subscribeOnChanges(postIds, callback), 5000);
        return;
      }
      const postId = await response.json();
      console.log(`${postId} has changed`);
      callback(); // Voila!
    } catch (error) {
      console.error("Subscribe on changes failed:", error);
    } finally {
      this._cancelSubscription();
    }
  }

  async captivePortalAck() {
    // REST: Acknowledge captive portal
    const response = await fetch("/captive_portal_ack",
                                 { method: "GET", mode: "no-cors" });
    if (!response.ok) {
      console.error(`Captive portal failed: ${response.status}`);
      return false;
    }
    console.log("Captive portal acknowledgment sent to server");
    return true
  }

  decodeBase64(base64) {
    const binaryString = atob(base64);
    return new Uint8Array([...binaryString].map(char => char.charCodeAt(0)));
  }

  encodeBase64(bytes) {
    return btoa(String.fromCharCode(...bytes));
  }

  showLoadingSpinner() {
    document.getElementById("loading-overlay").removeAttribute("hidden");
  }

  hideLoadingSpinner() {
    document.getElementById("loading-overlay").setAttribute("hidden", true);
  }

  refreshAllUIKitIcons() {
    document.querySelectorAll("[uk-icon]").forEach(el => {
      this.refreshUIKitIcon(el);
    });
  }

  refreshUIKitIcon(target) {
    UIkit.icon(target).$destroy();
    UIkit.icon(target);
  }

  initializeMobileKeyboardResizing(id) {
    // Dynamically resize body when keyboard opens or closes
    const resizeBody = () => {
      const newHeight =
            window.visualViewport ?
            window.visualViewport.height : window.innerHeight;
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

  initializeLongClick() {
    let clickTimer;
    let isLongClick = false;
    const lightboxImages = document.querySelectorAll(".long-click img");
    lightboxImages.forEach(img => {
      if (this._initializedElements.has(img)) {
        return;
      }
      this._initializedElements.add(img);
      // Get the <a> tag wrapping the image
      const parentAnchor = img.closest("a");
      // Get the image URL
      const downloadUrl = parentAnchor.getAttribute("href");
      // Suppress default context menu on long-press
      img.addEventListener("contextmenu", e => {
        e.preventDefault();
      });
      // Long-click logic
      img.addEventListener("mousedown", e => {
        if (e.button === 2) {
          // Ignore right-clicks
          return;
        }
        isLongClick = false;
        clickTimer = setTimeout(() => {
          isLongClick = true;
          const anchor = document.createElement("a");
          anchor.href = downloadUrl;
          anchor.download = "";
          anchor.click();
        }, this._LONG_CLICK_DURATION);
      });
      img.addEventListener("mouseup", () => clearTimeout(clickTimer));
      img.addEventListener("mouseleave", () => clearTimeout(clickTimer));
      // Long-press on touch devices
      img.addEventListener("touchstart", e => {
        isLongClick = false;
        clickTimer = setTimeout(() => {
          isLongClick = true;
          const anchor = document.createElement("a");
          anchor.href = downloadUrl;
          anchor.download = "";
          anchor.click();
        }, this._LONG_CLICK_DURATION);
        // Suppress default long-press behavior
        e.preventDefault();
      });
      img.addEventListener("touchend", () => clearTimeout(clickTimer));
      // Suppress lightbox opening on long-click
      parentAnchor.addEventListener("click", e => {
        if (isLongClick) {
          e.preventDefault();
        }
      });
    });
  }
}

const bespoke = new Bespoke();
export default bespoke;
