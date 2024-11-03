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
    const message = {
      title: document.getElementById('form-title').value,
      author: document.getElementById('form-author').value,
      body: document.getElementById('form-body').value
    };

    async function updateServer() {
      try {
        // REST: Get root messages
        const response = await fetch("/insert_message", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify(message)
        });
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          return;
        }
        bespoke.navigateTo("topics.html");
      } catch (error) {
        console.error("Fetching failed:", error);
      }
    };

    updateServer();
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
