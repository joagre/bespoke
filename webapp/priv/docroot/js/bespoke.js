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
    this._initializeSwipeListeners();
    this._initializeCookieState();
  },

  gotoPage(event, destination) {
    if (!event) {
      console.error("No event object provided");
      return;
    }

    this._handleButtonEvent(event);

    if (this._isTextSelected()) {
      return; // Do not navigate if text is selected
    }

    if (destination === "message.html" && this._isMessageStackEmpty()) {
      this._navigateTo("topics.html");
    } else {
      this._navigateTo(destination);
    }
  },

  clearMessageStack() {
    this._cookieState.messageStack = [];
    this._updateCookieState();
  },

  pushMessageStack(messageId) {
    this._cookieState.messageStack.push(messageId);
    this._updateCookieState();
  },

  popMessageStack() {
    if (this._cookieState.messageStack.pop() != null) {
      this._updateCookieState();
    }
  },

  formatSecondsSinceEpoch(secondsSinceEpoch) {
    const now = Math.floor(Date.now() / 1000);
    const ageInSeconds = now - secondsSinceEpoch;

    if (ageInSeconds < 60) return `${ageInSeconds}s`;
    if (ageInSeconds < 3600) return `${Math.floor(ageInSeconds / 60)}m`;
    if (ageInSeconds < 86400) return `${Math.floor(ageInSeconds / 3600)}h`;
    if (ageInSeconds < 31536000) return `${Math.floor(ageInSeconds / 86400)}d`;
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

  _initializeCookieState() {
    this._cookieState = this._getCookie("bespoke");

    if (!this._cookieState) {
      console.warn("No Bespoke cookie found; initializing new state");
      this._cookieState = { messageStack: [] };
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
    return this._cookieState.messageStack.length === 0;
  },

  _navigateTo(destination) {
    window.location.href = destination;
  },

  _updateCookieState() {
    this._setCookie("bespoke", this._cookieState, 7); // 7 days expiry by default
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
    const cookieValue = typeof value === "object" ? JSON.stringify(value) : value;
    document.cookie = `${name}=${encodeURIComponent(cookieValue)}${expires}; path=/; SameSite=Strict`;
  },

  _initializeSwipeListeners() {
    document.addEventListener("touchstart", (event) => this._onTouchStart(event));
    document.addEventListener("touchend", (event) => this._onTouchEnd(event));
  },

  _onTouchStart(event) {
    this._touch.startX = event.changedTouches[0].screenX;
    this._touch.startY = event.changedTouches[0].screenY;
  },

  _onTouchEnd(event) {
    this._touch.endX = event.changedTouches[0].screenX;
    this._touch.endY = event.changedTouches[0].screenY;
    this._handleSwipeGesture();
  },

  _handleSwipeGesture() {
    const horizontalSwipe = this._touch.endX - this._touch.startX;
    const verticalSwipe = Math.abs(this._touch.endY - this._touch.startY);

    if (horizontalSwipe > SWIPE_THRESHOLD && verticalSwipe < VERTICAL_THRESHOLD) {
      this._triggerSwipeNavigation();
    }
  },

  _triggerSwipeNavigation() {
    const swipeTarget = document.querySelector("[data-back-destination]");
    if (swipeTarget) {
      this._navigateTo(swipeTarget.getAttribute("data-back-destination"));
    }
  },
};

export default bespoke;
