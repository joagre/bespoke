const Message = (function() {
    let messageId = null;

    function openDeleteMessageModal(event, button) {
        event.stopPropagation();
        messageId = button.getAttribute("data-message-id");
        const messageTitle = button.getAttribute("data-message-title");
        document.getElementById("delete-message-title").textContent = messageTitle;
        UIkit.modal("#delete-message-modal").show();
    }

    function deleteMessage() {
        if (messageId == null) {
            console.error("No message ID specified for deletion.");
            return;
        }

        console.log("Deleting message with ID: " + messageId);
        UIkit.modal("#delete-message-modal").hide();
        messageId = null;
    }

    return {
        openDeleteMessageModal: openDeleteMessageModal,
        deleteMessage: deleteMessage
    };
})();

document.addEventListener("DOMContentLoaded", function() {
    Bespoke.init();
    console.log("Populating message.html with dynamic content");
});
