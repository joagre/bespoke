// Import dependencies
import bespoke from "/js/bespoke.js";

class AddPost {
  constructor() {
    this._formFields = [];
    this._addButton = null;
  }

  init() {
    bespoke.initializeCookieState();
    this._formFields = Array.from(
      document.querySelectorAll("#form-title, #form-author, #form-body")
    );
    this._addButton = document.getElementById("add-button");
    this._attachEventListeners();
  }

  addPost(event) {
    event.preventDefault();
    const message = {
      title: document.getElementById("form-title").value,
      author: document.getElementById("form-author").value,
      body: document.getElementById("form-body").value,
    };

    const updateServer = async () => {
      try {
        // REST: Add root message
        const response = await fetch("/insert_message", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify(message),
        });
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          // Optional: Display an error message to the user
          return;
        }
        bespoke.navigateTo("posts.html");
      } catch (error) {
        console.error("Fetching failed:", error);
        // Optional: Display an error message to the user
      }
    };

    updateServer();
  }

  _attachEventListeners() {
    this._formFields.forEach((field) => {
      field.addEventListener("input", () => this._checkFormCompletion());
    });
  }

  _checkFormCompletion() {
    const allFilled = this._formFields.every(
      (field) => field.value.trim() !== ""
    );
    this._addButton.disabled = !allFilled;
  }
}

// Initialize the class on DOMContentLoaded
document.addEventListener("DOMContentLoaded", () => {
  bespoke.init();
  addPost.init();
});

// Export the class instance
const addPost = new AddPost();
export default addPost
