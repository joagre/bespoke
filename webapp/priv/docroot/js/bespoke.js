const Bespoke = (function() {
    let touchStartX = 0;
    let touchEndX = 0;

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
        if (touchEndX - touchStartX > 50) {
            const swipeTarget = document.querySelector('[data-destination]');
            if (swipeTarget) {
                navigateToPage({ currentTarget: swipeTarget });
            }
        }
    }

    function initSwipeListeners() {
        document.addEventListener('touchstart', (event) => {
            touchStartX = event.changedTouches[0].screenX;
        });

        document.addEventListener('touchend', (event) => {
            touchEndX = event.changedTouches[0].screenX;
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
