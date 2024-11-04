import bespoke from "/js/bespoke.js";

const addReply = {
  parentMessage: null,
  rootMessageTitle: null,
  _formFields: [],
  _addButton: null,

  init() {
    addReply._formFields =
      Array.from(document.querySelectorAll("#form-author, #form-body"));
    addReply._addButton = document.getElementById('add-button');
    addReply._attachEventListeners();
  },

  addReply(event) {
    event.preventDefault();

    async function updateServer() {
      // Message to be added
      const message = {
        author: document.getElementById('form-author').value,
        body: document.getElementById('form-body').value
      };
      message["parent-message-id"] = addReply.parentMessage["id"];
      if (addReply.parentMessage["root-message-id"] == null) {
        message["root-message-id"] = addReply.parentMessage["id"];
      } else {
        message["root-message-id"] = addReply.parentMessage["root-message-id"];
      }

      try {
        // REST: Add reply message
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
        bespoke.gotoPage(event, "message.html");
      } catch (error) {
        console.error("Fetching failed:", error);
      }
    };

    updateServer();
  },

  _attachEventListeners() {
    addReply._formFields.forEach(field => {
      field.addEventListener('input', () => addReply._checkFormCompletion());
    });
  },

  _checkFormCompletion() {
    const allFilled =
          addReply._formFields.every(field => field.value.trim() !== '');
    addReply._addButton.disabled = !allFilled;
  }
};

export default addReply;

document.addEventListener("DOMContentLoaded", function() {
  bespoke.init();
  addReply.init();

  function populatePage(parentMessage, rootMessageTitle) {
    // Populate page title
    document.getElementById("page-title").textContent =
      bespoke.escapeHTML(rootMessageTitle);

    // Populate header
    document.getElementById("message-title").textContent =
      document.getElementById("page-title").textContent;
    document.getElementById("reply-depth").textContent =
      `[${bespoke.messageStackSize()}]`;

    // Populate parent message
    document.getElementById("message-body").textContent =
      bespoke.escapeHTML(parentMessage["body"]);
    document.getElementById("message-author").textContent =
      bespoke.escapeHTML(parentMessage["author"]);
    document.getElementById("message-created").textContent =
      bespoke.formatSecondsSinceEpoch(parentMessage["created"]);
    document.getElementById("message-reply-count").textContent =
      parentMessage["reply-count"];
  };

  async function updatePage() {
    try {
      // REST: Get parent message
      let response = await fetch("/lookup_messages", {
        method: "POST",
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify([bespoke.peekMessageStack()])
      });
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return;
      }
      let data = await response.json();
      bespoke.assert(data.length === 1, "Expected exactly one message");
      addReply.parentMessage = data[0];

      // REST: Get root message title (maybe)
      addReply.rootMessageTitle = addReply.parentMessage["title"];
      if (addReply.rootMessageTitle == null) {
        response = await fetch("/lookup_messages", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify([addReply.parentMessage["root-message-id"]])
        });
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          return;
        }
        data = await response.json();
        bespoke.assert(data.length === 1, "Expected exactly one message");
        const rootMessage = data[0];
        addReply.rootMessageTitle = rootMessage["title"];
      }

      // Populate page
      populatePage(addReply.parentMessage, addReply.rootMessageTitle);
    } catch (error) {
      console.error("Fetching failed:", error);
    }
  };

  updatePage();
});
