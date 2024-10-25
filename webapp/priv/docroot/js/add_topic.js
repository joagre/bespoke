document.addEventListener('DOMContentLoaded', () => {
    const formFields = Array.from(document.querySelectorAll('#form-title, #form-author, #form-body'));
    const createButton = document.getElementById('create-button');

    function checkFormCompletion() {
        const allFilled = formFields.every(field => field.value.trim() !== '');
        createButton.disabled = !allFilled;
    }

    formFields.forEach(field => field.addEventListener('input', checkFormCompletion));
});
