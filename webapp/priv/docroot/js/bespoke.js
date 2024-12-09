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
    this.SWIPE_THRESHOLD = 30;
    this.VERTICAL_THRESHOLD = 75;
    this._initializeSwipeListeners();
    this._initializeCookieState();
  }

  _initializeSwipeListeners() {
    document.addEventListener(
      "touchstart",
      (event) => this._onTouchStart(event),
      {passive: false});
    document.addEventListener(
      "touchend",
      (event) => this._onTouchEnd(event),
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
        window.history.back();
      } else if (destination === "post.html") {
        this.gotoPage(null, destination, -1);
      } else {
        this.gotoPage(null, destination);
      }
    }
  }

  _initializeCookieState() {
    this._cookieState = this._getCookie("bespoke");
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
    // Prevent form submission
    if (event != null) {
      this._handleButtonEvent(event);
    }
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
    // If destination is not set, go back to the referrer
    if (destination == null) {
      if (document.referrer) {
        window.history.back();
        return;
      } else {
        console.error("No referrer found");
      }
    }
    // If the post stack is empty, go to the top posts page
    if (destination === "post.html" && this._isPostStackEmpty()) {
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

  _handleButtonEvent(event) {
    // Prevent form submission
    if (event.target.tagName === "BUTTON" || event.target.closest("button")) {
      event.stopPropagation();
    }
    event.preventDefault();
  }

  _isTextSelected() {
    const selection = window.getSelection();
    return selection && selection.toString().length > 0;
  }

  _isPostStackEmpty() {
    return this.postStackSize() === 0;
  }

  setCookieValue(name, value) {
    this._cookieState[name] = value;
    this._updateCookieState();
  }

  isCookieSet() {
    return this._cookieState != null;
  }

  getCookieValue(name) {
    return this._cookieState[name];
  }

  setLocalItem(key, value) {
    console.log("Setting local item:", key, value);
    localStorage.setItem(key, JSON.stringify(value));
  }

  getLocalItem(key) {
    const value = localStorage.getItem(key);
    this.assert(value != null, "Local item is maybe missing");
    return JSON.parse(value);
  }

  setRawLocalItem(key, value) {
    console.log("Setting local item:", key, value);
    localStorage.setItem(key, value);
  }

  getRawLocalItem(key) {
    const value = localStorage.getItem(key);
    return value;
  }

  clearPostStack() {
    this.setLocalItem("postStack", []);
  }

  _pushPostStack(postId) {
    this.assert(postId != null, "post-id is not set");
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
    window.location.href = destination;
  }

  generateStrongPassword() {
    const length = 16;
    const charset =
          "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()_+~`|}{[]:;?><,./-=";
    let password = "";
    for (let i = 0; i < length; i++) {
      const at = Math.floor(Math.random() * charset.length);
      password += charset.charAt(at);
    }
    return password;
  }
}

const bespoke = new Bespoke();
export default bespoke;
