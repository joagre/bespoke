// A placeholder
const Topics = (function() {
    return {
    };
})();

document.addEventListener("DOMContentLoaded", function() {
    Bespoke.init();
    Bespoke.clearMessageStack();
    // Fetch root messages
    fetch("/list_root_messages")
        .then(response => {
            if (!response.ok) {
                throw new Error(`Server error: ${response.status}`);
            }
            return response.json();
        })
        .then(data => console.log(data))
        .catch(error => console.error("Error:", error));
});
