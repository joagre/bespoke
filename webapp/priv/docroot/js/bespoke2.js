// Import dependencies
import { marked } from '/js/marked.esm.js';

// Ensure uhtml.min.js is imported in the HTML file before this script
const { html, render } = uhtml;

class Bespoke2 {
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

  gotoPage(event, destination, messageId) {
    if (messageId === -1) {
      this.popMessageStack();
    } else if (typeof messageId === 'string') {
      this.pushMessageStack(messageId);
    }

    this._handleButtonEvent(event);

    if (this._isTextSelected()) {
      return;
    }

    if (destination === 'message2.html' && this._isMessageStackEmpty()) {
      this.navigateTo('posts2.html');
    } else {
      this.navigateTo(destination);
    }
  }

  clearMessageStack() {
    this._cookieState.messageStack = [];
    this._updateCookieState();
  }

  pushMessageStack(messageId) {
    this._cookieState.messageStack.push(messageId);
    this._updateCookieState();
  }

  popMessageStack() {
    if (this._cookieState.messageStack.pop() != null) {
      this._updateCookieState();
    }
  }

  peekMessageStack() {
    return this._cookieState.messageStack.slice(-1)[0];
  }

  messageStackSize() {
    return this._cookieState.messageStack.length;
  }

  truncateMessageStack(length) {
    this._cookieState.messageStack = this._cookieState.messageStack.slice(0, length);
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
    if (ageInSeconds < 29030400) return `${Math.floor(ageInSeconds / 2592000)}mo`;
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

  assert(condition, message) {
    if (!condition) {
      throw new Error(message || 'Assertion failed');
    }
  }

  navigateTo(destination) {
    window.location.href = destination;
  }

  initializeCookieState() {
    this._cookieState = this._getCookie('bespoke');

    if (!this._cookieState) {
      this._cookieState = { messageStack: [] };
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

  _isMessageStackEmpty() {
    return this._cookieState.messageStack.length === 0;
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
    const cookieValue = typeof value === 'object' ? JSON.stringify(value) : value;
    document.cookie = `${name}=${encodeURIComponent(cookieValue)}${expires}; path=/; SameSite=Strict`;
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
      if (destination === 'message2.html') {
        this.gotoPage(event, destination, -1);
      } else {
        this.gotoPage(event, destination);
      }
    }
  }
}

// Export the class instance
const bespoke2 = new Bespoke2();
export default bespoke2;
