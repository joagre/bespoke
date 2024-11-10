import bespoke2 from "/js/bespoke2.js";

const addPost2 = {
  _formFields: [],
  _addButton: null,

  init() {
    addPost2._formFields =
      Array.from(document.querySelectorAll(
        "#form-title, #form-author, #form-body"));
    addPost2._addButton = document.getElementById('add-button');
    addPost2._attachEventListeners();
  },

  addPost(event) {
    event.preventDefault();
    const message = {
      title: document.getElementById('form-title').value,
      author: document.getElementById('form-author').value,
      body: document.getElementById('form-body').value
    };

    async function updateServer() {
      try {
        // REST: Add root message
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
        bespoke2.navigateTo("posts2.html");
      } catch (error) {
        console.error("Fetching failed:", error);
      }
    };

    updateServer();
  },

  _attachEventListeners() {
    addPost2._formFields.forEach(field => {
      field.addEventListener('input', () => addPost2._checkFormCompletion());
    });
  },

  _checkFormCompletion() {
    const allFilled =
          addPost2._formFields.every(field => field.value.trim() !== '');
    addPost2._addButton.disabled = !allFilled;
  }
};

document.addEventListener("DOMContentLoaded", () => {
  bespoke2.init();
  addPost2.init();
});

export default addPost2;
