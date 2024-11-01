const AddTopic = (function() {
    let formFields = [];
    let createButton = null;

    function init() {
        formFields = Array.from(document.querySelectorAll('#form-title, #form-author, #form-body'));
        createButton = document.getElementById('create-button');
    }

    function checkFormCompletion() {
        const allFilled = formFields.every(field => field.value.trim() !== '');
        createButton.disabled = !allFilled;
    }

    return {
        init: init,
        checkFormCompletion: checkFormCompletion
    };
})();

document.addEventListener("DOMContentLoaded", function() {
    Bespoke.init();
    AddTopic.init();
    formFields.forEach(field => field.addEventListener('input', AddTopic.checkFormCompletion));
});
