const Topics = (function() {
    let currentTopicId = null;

    function openDeleteModal(button) {
        // Get the topic ID from the button's data attribute
        currentTopicId = button.getAttribute('data-topic-id');

        // Find the title element using the topic ID and set it in the modal
        const topicTitleElement = document.getElementById(`topic-title-${currentTopicId}`);
        const topicTitle = topicTitleElement ? topicTitleElement.textContent : 'Unknown Topic';

        // Update the modal content
        document.getElementById('modal-topic-title').textContent = topicTitle;

        // Open the modal
        UIkit.modal('#delete-modal').show();
    }

    function deleteTopic() {
        if (!currentTopicId) {
            console.warn("No topic ID specified for deletion.");
            return;
        }

        console.log(`Deleting topic with ID: ${currentTopicId}`);

        // Close the modal after deletion
        UIkit.modal('#delete-modal').hide();

        // Reset the current topic ID
        currentTopicId = null;
    }

    return {
        openDeleteModal: openDeleteModal,
        deleteTopic: deleteTopic
    };
})();
