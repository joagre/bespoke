import { marked } from '/js/marked.esm.js';

// uhtml.min.js must be imported in the HTML file before this script
const { html, render } = uhtml;

const SWIPE_THRESHOLD = 30;
const VERTICAL_THRESHOLD = 75;

const bespoke2 = {
  _cookieState: null,
  _touch: {
    startX: 0,
    endX: 0,
    startY: 0,
    endY: 0,
  },

  init() {
    bespoke2._initializeSwipeListeners();
    bespoke2._initializeCookieState();
  },

  gotoPage(event, destination, messageId) {
    if (messageId === -1) {
      bespoke2.popMessageStack();
    } else if (typeof messageId === "string") {
      bespoke2.pushMessageStack(messageId);
    }

    bespoke2._handleButtonEvent(event);

    if (bespoke2._isTextSelected()) {
      return;
    }

    if (destination === "message2.html" && bespoke2._isMessageStackEmpty()) {
      bespoke2.navigateTo("posts2.html");
    } else {
      bespoke2.navigateTo(destination);
    }
  },

  clearMessageStack() {
    bespoke2._cookieState.messageStack = [];
    bespoke2._updateCookieState();
  },

  pushMessageStack(messageId) {
    bespoke2._cookieState.messageStack.push(messageId);
    bespoke2._updateCookieState();
  },

  popMessageStack() {
    if (bespoke2._cookieState.messageStack.pop() != null) {
      bespoke2._updateCookieState();
    }
  },

  peekMessageStack() {
    return bespoke2._cookieState.messageStack.slice(-1)[0];
  },

  messageStackSize() {
    return bespoke2._cookieState.messageStack.length;
  },

  truncateMessageStack(length) {
    bespoke2._cookieState.messageStack =
      bespoke2._cookieState.messageStack.slice(0, length);
    bespoke2._updateCookieState();
  },

  formatSecondsSinceEpoch(secondsSinceEpoch) {
    const now = Math.floor(Date.now() / 1000);
    const ageInSeconds = now - secondsSinceEpoch;

    if (ageInSeconds < 60) return `${ageInSeconds}s`;
    if (ageInSeconds < 3600) return `${Math.floor(ageInSeconds / 60)}m`;
    if (ageInSeconds < 86400) return `${Math.floor(ageInSeconds / 3600)}h`;
    if (ageInSeconds < 31536000) return `${Math.floor(ageInSeconds / 86400)}d`;
    if (ageInSeconds < 31536000 * 2) return `${Math.floor(ageInSeconds / 604800)}w`;
    if (ageInSeconds < 31536000 * 12) return `${Math.floor(ageInSeconds / 2592000)}mo`;
    return `${Math.floor(ageInSeconds / 31536000)}y`;
  },

  uhtmlFormatMarkdown(content) {
    // FIXME: Sanitize HTML
    return uhtml.html`${uhtml.html([marked.parse(content)])}`;
  },

  formatMarkdown(content) {
    // FIXME: Sanitize HTML
    return marked.parse(content);
  },

  assert(condition, message) {
    if (!condition) {
      throw new Error(message || "Assertion failed");
    }
  },

  navigateTo(destination) {
    window.location.href = destination;
  },

  _initializeCookieState() {
    bespoke2._cookieState = bespoke2._getCookie("bespoke");

    if (!bespoke2._cookieState) {
      bespoke2._cookieState = { messageStack: [] };
    }
  },

  _handleButtonEvent(event) {
    if (event.target.tagName === "BUTTON" || event.target.closest("button")) {
      event.stopPropagation();
    }
    event.preventDefault();
  },

  _isTextSelected() {
    const selection = window.getSelection();
    return selection && selection.toString().length > 0;
  },

  _isMessageStackEmpty() {
    return bespoke2._cookieState.messageStack.length === 0;
  },

  _updateCookieState() {
    bespoke2._setCookie("bespoke", bespoke2._cookieState, 7);
  },

  _getCookie(name) {
    const value = `; ${document.cookie}`;
    const parts = value.split(`; ${name}=`);
    if (parts.length === 2) {
      try {
        return JSON.parse(decodeURIComponent(parts.pop().split(";").shift()));
      } catch (e) {
        console.error("Error parsing Bespoke cookie", e);
      }
    }
    return null;
  },

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
  },

  _initializeSwipeListeners() {
    document.addEventListener("touchstart",
                              (event) => bespoke2._onTouchStart(event),
                              {passive: false});
    document.addEventListener("touchend",
                              (event) => bespoke2._onTouchEnd(event),
                              {passive: false});
  },

  _onTouchStart(event) {
    bespoke2._touch.startX = event.changedTouches[0].screenX;
    bespoke2._touch.startY = event.changedTouches[0].screenY;
  },

  _onTouchEnd(event) {
    bespoke2._touch.endX = event.changedTouches[0].screenX;
    bespoke2._touch.endY = event.changedTouches[0].screenY;
    bespoke2._handleSwipeGesture();
  },

  _handleSwipeGesture() {
    const horizontalSwipe = bespoke2._touch.endX - bespoke2._touch.startX;
    const verticalSwipe = Math.abs(bespoke2._touch.endY - bespoke2._touch.startY);
    if (horizontalSwipe > SWIPE_THRESHOLD &&
        verticalSwipe < VERTICAL_THRESHOLD) {
      bespoke2._triggerSwipeNavigation();
    }
  },

  _triggerSwipeNavigation() {
    const swipeTarget = document.querySelector("[data-back-destination]");
    if (swipeTarget) {
      const destination = swipeTarget.getAttribute("data-back-destination");
      if (destination === "message2.html") {
        bespoke2.gotoPage(event, destination, -1);
      } else {
        bespoke2.gotoPage(event, destination);
      }
    }
  },
};

export default bespoke2;
