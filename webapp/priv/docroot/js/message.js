const Message = (function() {
    let messageId = null;

    function openDeleteMessageModal(event, button, messageId, messageTitle) {
        event.stopPropagation();
        this.messageId = messageId;
        document.getElementById("delete-message-title").textContent = messageTitle;
        UIkit.modal("#delete-message-modal").show();
    }

    function deleteMessage() {
        if (this.messageId == null) {
            console.error("No message ID specified for deletion.");
            return;
        }

        console.log("Deleting message with ID: " + this.messageId);
        UIkit.modal("#delete-message-modal").hide();
        this.messageId = null;
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
