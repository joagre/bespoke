const Topics = (function() {
    let currentTopicId = null;

    function openDeleteModal(button) {
        event.stopPropagation()

        // Get the topic ID and title from the button's data attributes
        currentTopicId = button.getAttribute('data-topic-id');
        const topicTitle = button.getAttribute('data-topic-title');

        // Set the topic title in the modal
        document.getElementById('modal-topic-title').textContent = topicTitle;

        // Open the modal
        UIkit.modal('#delete-modal').show();
    }

    function deleteTopic() {
        if (!currentTopicId) {
            console.warn("No topic ID specified for deletion.");
            return;
        }

        // Example: Here you would add the code to delete the topic using currentTopicId
        console.log(`Deleting topic with ID: ${currentTopicId}`);

        // Close the modal after deletion
        UIkit.modal('#delete-modal').hide();

        // Reset the current topic ID after action
        currentTopicId = null;
    }

    return {
        openDeleteModal: openDeleteModal,
        deleteTopic: deleteTopic
    };
})();
