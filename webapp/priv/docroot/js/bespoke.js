const Bespoke = (function() {
    let bespokeState = null;
    let touchStartX = 0;
    let touchEndX = 0;
    let touchStartY = 0;
    let touchEndY = 0;
    const SWIPE_THRESHOLD = 30;  // Reduced threshold for sensitivity
    const VERTICAL_THRESHOLD = 75; // Prevent accidental vertical swipes

    function init() {
        initSwipeListeners();
        bespokeState = getCookie("bespoke");

        if (bespokeState == null) {
            console.warn("No Bespoke cookie found; initializing new state");
            bespokeState = {messageStack: []};
        }
    }

    function gotoPage(event) {
        if (event == null) {
            console.error("No event object provided");
            return;
        }

        if (event.target.tagName === "BUTTON" || event.target.closest("button")) {
            event.stopPropagation();
        }

        event.preventDefault();

        const selection = window.getSelection();
        if (selection && selection.toString().length > 0) {
            return;
        }

        const destination = event.currentTarget.getAttribute("data-destination");

        if (destination == null) {
            console.error("No destination URL specified in data-destination attribute");
            return;
        }

        if (destination === "message.html" && bespokeState.messageStack.length === 0) {
            window.location.href = "topics.html";
        } else {
            window.location.href = destination;
        }
    }

    function clearMessageStack() {
        bespokeState.messageStack = [];
        setCookie("bespoke", bespokeState);
    }

    function pushMessageStack(messageId) {
        bespokeState.messageStack.push(messageId);
        setCookie("bespoke", bespokeState);
    }

    function popMessageStack() {
        if (bespokeState.messageStack.pop() != null) {
            setCookie("bespoke", bespokeState);
        }
    }

    function getCookie(name) {
        const value = `; ${document.cookie}`;
        const parts = value.split(`; ${name}=`);

        if (parts.length === 2) {
            try {
                return JSON.parse(decodeURIComponent(parts.pop().split(";").shift()));
            } catch (e) {
                console.error("Error parsing Bespoke cookie", e);
                return null;
            }
        }

        return null;
    }

    function setCookie(name, value, days) {
        let expires = "";

        if (days != null) {
            const date = new Date();
            date.setTime(date.getTime() + days * 24 * 60 * 60 * 1000);
            expires = "; expires=" + date.toUTCString();
        }

        const cookieValue = typeof value === "object" ? JSON.stringify(value) : value;
        document.cookie = name + "=" + encodeURIComponent(cookieValue) + expires + "; path=/; SameSite=Strict";
    }

    function handleSwipeGesture() {
        const horizontalSwipe = touchEndX - touchStartX;
        const verticalSwipe = Math.abs(touchEndY - touchStartY);

        if (horizontalSwipe > SWIPE_THRESHOLD && verticalSwipe < VERTICAL_THRESHOLD) {
            const swipeTarget = document.querySelector("[data-destination]");

            if (swipeTarget) {
                navigateToPage({ currentTarget: swipeTarget });
            }
        }
    }

    function initSwipeListeners() {
        document.addEventListener("touchstart", (event) => {
            touchStartX = event.changedTouches[0].screenX;
            touchStartY = event.changedTouches[0].screenY;
        });
        document.addEventListener("touchend", (event) => {
            touchEndX = event.changedTouches[0].screenX;
            touchEndY = event.changedTouches[0].screenY;
            handleSwipeGesture();
        });
    }

    return {
        init: init,
        gotoPage: gotoPage,
        clearMessageStack: clearMessageStack,
        pushMessageStack: pushMessageStack,
        popMessageStack: popMessageStack
    };
})();
