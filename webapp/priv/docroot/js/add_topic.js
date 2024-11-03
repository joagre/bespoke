import bespoke from "/js/bespoke.js";

const addTopic = {
  _formFields: [],
  _addButton: null,

  init() {
    addTopic._formFields =
      Array.from(document.querySelectorAll(
        '#form-title, #form-author, #form-body'));
    addTopic._addButton = document.getElementById('add-button');
    addTopic._attachEventListeners();
  },

  addTopic(event) {
    event.preventDefault();
    const title = document.getElementById('form-title').value;
    const author = document.getElementById('form-author').value;
    const body = document.getElementById('form-body').value;
    console.log("Adding topic with title: " + title);
    console.log("Adding topic with author: " + author);
    console.log("Adding topic with body: " + body.substring(0, 50) +
                (body.length > 50 ? "..." : ""));
  },

  _attachEventListeners() {
    addTopic._formFields.forEach(field => {
      field.addEventListener('input', () => addTopic._checkFormCompletion());
    });
  },

  _checkFormCompletion() {
    const allFilled =
          addTopic._formFields.every(field => field.value.trim() !== '');
    addTopic._addButton.disabled = !allFilled;
  }
};

document.addEventListener("DOMContentLoaded", () => {
  bespoke.init();
  addTopic.init();
});

export default addTopic;
