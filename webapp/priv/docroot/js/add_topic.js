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

    return {
        formFields: function() {
            return _formFields;
        },
        init: init,
        checkFormCompletion: checkFormCompletion
    };
})();

document.addEventListener("DOMContentLoaded", function() {
    Bespoke.init();
    AddTopic.init();
    AddTopic.formFields().forEach(
        field => field.addEventListener('input', AddTopic.checkFormCompletion));
});
