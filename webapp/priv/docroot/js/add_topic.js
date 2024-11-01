const AddTopic = (function() {
    let _formFields = [];
    let createButton = null;

    function init() {
        _formFields = Array.from(document.querySelectorAll('#form-title, #form-author, #form-body'));
        createButton = document.getElementById('create-button');
    }

    function checkFormCompletion() {
        const allFilled = _formFields.every(field => field.value.trim() !== '');
        createButton.disabled = !allFilled;
    }

    function addTopic(event) {
        event.preventDefault();
        const title = document.getElementById('form-title').value;
        const author = document.getElementById('form-author').value;
        const body = document.getElementById('form-body').value;
        console.log("Adding topic with title: " + title);
        console.log("Adding topic with author: " + author);
        console.log("Adding topic with body: " +
                    body.substring(0, 50) + (body.length > 50 ? "..." : ""));
    }

    return {
        formFields: function() {
            return _formFields;
        },
        init: init,
        checkFormCompletion: checkFormCompletion,
        addTopic: addTopic
    };
})();

document.addEventListener("DOMContentLoaded", function() {
    Bespoke.init();
    AddTopic.init();
    AddTopic.formFields().forEach(
        field => field.addEventListener('input', AddTopic.checkFormCompletion));
});
