const Bespoke = (function() {
    // Private variables or functions can be declared here if needed

    // Public method
    function doNothing() {
        console.log("Doing nothing...");
    }

    // Return the Singleton instance
    return {
        doNothing: doNothing
    };
})();
