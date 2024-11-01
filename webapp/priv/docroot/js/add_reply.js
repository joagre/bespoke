const AddReply = (function() {
    let _formFields = [];
    let createButton = null;

    function init() {
        _formFields = Array.from(document.querySelectorAll('#form-author, #form-body'));
        createButton = document.getElementById('create-button');
    }

    function checkFormCompletion() {
        const allFilled = _formFields.every(field => field.value.trim() !== '');
        createButton.disabled = !allFilled;
    }

    function addReply(event) {
        event.preventDefault();
        const author = document.getElementById('form-author').value;
        const body = document.getElementById('form-body').value;
        console.log("Adding reply with author: " + author);
        console.log("Adding reply with body: " +
                    body.substring(0, 50) + (body.length > 50 ? "..." : ""));
    }

    return {
        formFields: function() {
            return _formFields;
        },
        init: init,
        checkFormCompletion: checkFormCompletion,
        addReply: addReply
    };
})();

document.addEventListener("DOMContentLoaded", function() {
    Bespoke.init();
    AddReply.init();
    AddReply.formFields().forEach(
        field => field.addEventListener('input', AddReply.checkFormCompletion));
});
