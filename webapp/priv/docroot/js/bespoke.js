// Import dependencies
import { marked } from '/js/marked.esm.js';

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

    // Constants
    this.SWIPE_THRESHOLD = 30;
    this.VERTICAL_THRESHOLD = 75;

    // Initialize the class
    this.init();
  }

  init() {
    this._initializeSwipeListeners();
    this.initializeCookieState();
  }

  gotoPage(event, destination, postId) {
    if (postId === -1) {
      this.popPostStack();
    } else if (typeof postId === 'string') {
      this.pushPostStack(postId);
    }

    if (event != null) {
      this._handleButtonEvent(event);
    }

    if (this._isTextSelected()) {
      return;
    }

    if (destination === 'post.html' && this._isPostStackEmpty()) {
      this.navigateTo('top_posts.html');
    } else {
      this.navigateTo(destination);
    }
  }

  setCookieValue(name, value) {
    this._cookieState[name] = value;
    this._updateCookieState();
  }

  getCookieValue(name) {
    return this._cookieState[name];
  }

  clearPostStack() {
    this._cookieState.postStack = [];
    this._updateCookieState();
  }

  pushPostStack(postId) {
    this._cookieState.postStack.push(postId);
    this._updateCookieState();
  }

  popPostStack() {
    if (this._cookieState.postStack.pop() != null) {
      this._updateCookieState();
    }
  }

  peekPostStack() {
    return this._cookieState.postStack.slice(-1)[0];
  }

  postStackSize() {
    return this._cookieState.postStack.length;
  }

  truncatePostStack(length) {
    this._cookieState.postStack =
      this._cookieState.postStack.slice(0, length);
    this._updateCookieState();
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
      throw new Error(post || 'Assertion failed');
    }
  }

  navigateTo(destination) {
    window.location.href = destination;
  }

  initializeCookieState() {
    this._cookieState = this._getCookie('bespoke');

    if (!this._cookieState) {
      this._cookieState = { postStack: [] };
      this._updateCookieState();
    }
  }

  _handleButtonEvent(event) {
    if (event.target.tagName === 'BUTTON' || event.target.closest('button')) {
      event.stopPropagation();
    }
    event.preventDefault();
  }

  _isTextSelected() {
    const selection = window.getSelection();
    return selection && selection.toString().length > 0;
  }

  _isPostStackEmpty() {
    return this._cookieState.postStack.length === 0;
  }

  _updateCookieState() {
    this._setCookie('bespoke', this._cookieState, 7);
  }

  _getCookie(name) {
    const value = `; ${document.cookie}`;
    const parts = value.split(`; ${name}=`);
    if (parts.length === 2) {
      try {
        return JSON.parse(decodeURIComponent(parts.pop().split(';').shift()));
      } catch (e) {
        console.error('Error parsing Bespoke cookie', e);
      }
    }
    return null;
  }

  _setCookie(name, value, days = 7) {
    let expires = '';
    if (days) {
      const date = new Date();
      date.setTime(date.getTime() + days * 24 * 60 * 60 * 1000);
      expires = `; expires=${date.toUTCString()}`;
    }
    const cookieValue =
          typeof value === 'object' ? JSON.stringify(value) : value;
    document.cookie =
      `${name}=${encodeURIComponent(cookieValue)}${expires}; path=/; SameSite=Strict`;
  }

  _initializeSwipeListeners() {
    document.addEventListener(
      'touchstart',
      (event) => this._onTouchStart(event),
      { passive: false }
    );
    document.addEventListener(
      'touchend',
      (event) => this._onTouchEnd(event),
      { passive: false }
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
    if (
      horizontalSwipe > this.SWIPE_THRESHOLD &&
      verticalSwipe < this.VERTICAL_THRESHOLD
    ) {
      this._triggerSwipeNavigation();
    }
  }

  _triggerSwipeNavigation() {
    const swipeTarget = document.querySelector('[data-back-destination]');
    if (swipeTarget) {
      const destination = swipeTarget.getAttribute('data-back-destination');
      if (destination === 'post.html') {
        this.gotoPage(null, destination, -1);
      } else {
        this.gotoPage(null, destination);
      }
    }
  }
}

// Export the class instance
const bespoke = new Bespoke();
export default bespoke;
