import bespoke from "/js/bespoke.js";

const addReply = {
  replyMessage: null,
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
      message["reply-message-id"] = addReply.replyMessage["id"];
      if (addReply.replyMessage["root-message-id"] == null) {
        message["root-message-id"] = addReply.replyMessage["id"];
      } else {
        message["root-message-id"] = addReply.replyMessage["root-message-id"];
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

  function populatePage(replyMessage, rootMessageTitle) {
    // Populate page title
    document.getElementById("page-title").textContent =
      bespoke.escapeHTML(rootMessageTitle);

    // Populate header
    document.getElementById("message-title").textContent =
      document.getElementById("page-title").textContent;
    document.getElementById("reply-depth").textContent =
      `[${bespoke.messageStackSize()}]`;

    // Populate reply message
    document.getElementById("message-body").textContent =
      bespoke.escapeHTML(replyMessage["body"]);
    document.getElementById("message-author").textContent =
      bespoke.escapeHTML(replyMessage["author"]);
    document.getElementById("message-created").textContent =
      bespoke.formatSecondsSinceEpoch(replyMessage["created"]);
    document.getElementById("message-reply-count").textContent =
      replyMessage["reply-count"];
  };

  async function updatePage() {
    try {
      // REST: Get reply message
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
      const data = await response.json();
      bespoke.assert(data.length === 1, "Expected exactly one message");
      addReply.replyMessage = data[0];

      // REST: Get root message title (maybe)
      addReply.rootMessageTitle = addReply.replyMessage["title"];
      if (addReply.rootMessageTitle == null) {
        response = await fetch("/lookup_messages", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify([addReply.replyMessage["root-message-id"]])
        });
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          return;
        }
        data = await response.json();
        bespoke.assert(data.length === 1, "Expected exactly one message");
        const rootMessage = data[0];
        addReply.rootMessageTitle = addReply.replyMessage["title"];
      }

      // Populate page
      populatePage(addReply.replyMessage, addReply.rootMessageTitle);
    } catch (error) {
      console.error("Fetching failed:", error);
    }
  };

  updatePage();
});
