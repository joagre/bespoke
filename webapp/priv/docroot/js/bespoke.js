const SWIPE_THRESHOLD = 30;
const VERTICAL_THRESHOLD = 75;

const bespoke = {
  _cookieState: null,
  _touch: {
    startX: 0,
    endX: 0,
    startY: 0,
    endY: 0,
  },

  init() {
    bespoke._initializeSwipeListeners();
    bespoke._initializeCookieState();
  },

  gotoPage(event, destination, messageId) {
    if (messageId === -1) {
      bespoke.popMessageStack();
    } else if (typeof messageId === "string") {
      bespoke.pushMessageStack(messageId);
    }

    bespoke._handleButtonEvent(event);

    if (bespoke._isTextSelected()) {
      return;
    }

    if (destination === "message.html" && bespoke._isMessageStackEmpty()) {
      bespoke.navigateTo("topics.html");
    } else {
      bespoke.navigateTo(destination);
    }
  },

  clearMessageStack() {
    bespoke._cookieState.messageStack = [];
    bespoke._updateCookieState();
  },

  pushMessageStack(messageId) {
    bespoke._cookieState.messageStack.push(messageId);
    bespoke._updateCookieState();
  },

  popMessageStack() {
    if (bespoke._cookieState.messageStack.pop() != null) {
      bespoke._updateCookieState();
    }
  },

  peekMessageStack() {
    return bespoke._cookieState.messageStack.slice(-1)[0];
  },

  messageStackSize() {
    return bespoke._cookieState.messageStack.length;
  },

  truncateMessageStack(length) {
    bespoke._cookieState.messageStack =
      bespoke._cookieState.messageStack.slice(0, length);
    bespoke._updateCookieState();
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

  escapeHTML(unsafeString) {
    return unsafeString
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#039;");
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
    bespoke._cookieState = bespoke._getCookie("bespoke");

    if (!bespoke._cookieState) {
      bespoke._cookieState = { messageStack: [] };
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
    return bespoke._cookieState.messageStack.length === 0;
  },

  _updateCookieState() {
    bespoke._setCookie("bespoke", bespoke._cookieState, 7);
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
                              (event) => bespoke._onTouchStart(event),
                              {passive: false});
    document.addEventListener("touchend",
                              (event) => bespoke._onTouchEnd(event),
                              {passive: false});
  },

  _onTouchStart(event) {
    bespoke._touch.startX = event.changedTouches[0].screenX;
    bespoke._touch.startY = event.changedTouches[0].screenY;
  },

  _onTouchEnd(event) {
    bespoke._touch.endX = event.changedTouches[0].screenX;
    bespoke._touch.endY = event.changedTouches[0].screenY;
    bespoke._handleSwipeGesture();
  },

  _handleSwipeGesture() {
    const horizontalSwipe = bespoke._touch.endX - bespoke._touch.startX;
    const verticalSwipe = Math.abs(bespoke._touch.endY - bespoke._touch.startY);
    if (horizontalSwipe > SWIPE_THRESHOLD &&
        verticalSwipe < VERTICAL_THRESHOLD) {
      bespoke._triggerSwipeNavigation();
    }
  },

  _triggerSwipeNavigation() {
    const swipeTarget = document.querySelector("[data-back-destination]");
    if (swipeTarget) {
      const destination = swipeTarget.getAttribute("data-back-destination");
      if (destination === "message.html") {
        bespoke.gotoPage(event, destination, -1);
      } else {
        bespoke.gotoPage(event, destination);
      }
    }
  },
};

export default bespoke;
