const Bespoke = (function() {
    let touchStartX = 0;
    let touchEndX = 0;
    let touchStartY = 0;
    let touchEndY = 0;
    const SWIPE_THRESHOLD = 30;  // Reduced threshold for sensitivity
    const VERTICAL_THRESHOLD = 75; // Prevent accidental vertical swipes

    function navigateToPage(event) {
        if (event) event.preventDefault();

        const destination = event.currentTarget.getAttribute('data-destination');
        if (!destination) {
            console.warn("No destination URL specified in data-destination attribute.");
            return;
        }

        window.location.href = destination;
    }

    function handleSwipeGesture() {
        const horizontalSwipe = touchEndX - touchStartX;
        const verticalSwipe = Math.abs(touchEndY - touchStartY);

        if (horizontalSwipe > SWIPE_THRESHOLD && verticalSwipe < VERTICAL_THRESHOLD) {
            const swipeTarget = document.querySelector('[data-destination]');
            if (swipeTarget) {
                navigateToPage({ currentTarget: swipeTarget });
            }
        }
    }

    function initSwipeListeners() {
        document.addEventListener('touchstart', (event) => {
            touchStartX = event.changedTouches[0].screenX;
            touchStartY = event.changedTouches[0].screenY;
        });

        document.addEventListener('touchend', (event) => {
            touchEndX = event.changedTouches[0].screenX;
            touchEndY = event.changedTouches[0].screenY;
            handleSwipeGesture();
        });
    }

    function doNothing() {
        alert("Doing nothing...");
        console.log("Doing nothing...");
    }

    return {
        init: function() {
            initSwipeListeners();
        },
        navigateToPage: navigateToPage,
        doNothing: doNothing
    };
})();

// Initialize the Bespoke singleton
document.addEventListener('DOMContentLoaded', () => {
    Bespoke.init();
});
