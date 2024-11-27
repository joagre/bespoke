// Import dependencies
import bespoke from "/js/bespoke.js";

class AddReply {
  constructor() {
    this.parentMessage = null;
    this.rootMessageTitle = null;
    this._formFields = [];
    this._addButton = null;
  }

  init() {
    bespoke.initializeCookieState();
    this._formFields = Array.from(
      document.querySelectorAll("#form-author, #form-body")
    );
    this._addButton = document.getElementById("add-button");
    this._attachEventListeners();
    this.updatePage();
  }

  addReply(event) {
    event.preventDefault();

    const message = {
      author: document.getElementById("form-author").value,
      body: document.getElementById("form-body").value,
      "parent-message-id": this.parentMessage["id"],
      "root-message-id": (this.parentMessage["root-message-id"] != null) ? this.parentMessage["root-message-id"] : this.parentMessage["id"]
    };

    const updateServer = async () => {
      try {
        // REST: Add reply message
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
        this.goBack(event, true);
      } catch (error) {
        console.error("Fetching failed:", error);
        // Optional: Display an error message to the user
      }
    };

    updateServer();
  }

  gotoAddReplyPage(event, messageId, popMessageStack) {
    bespoke.setCookieValue("pop-message-stack", popMessageStack);
    bespoke.gotoPage(event, "add_reply.html", messageId);
  }

  goBack(event, ignorePopMessageStack) {
    if (!ignorePopMessageStack &&
        bespoke.getCookieValue("pop-message-stack")) {
      bespoke.popMessageStack();
    }
    bespoke.setCookieValue("pop-message-stack", false);
    bespoke.gotoPage(event, 'message.html')
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

  async updatePage() {
    try {
      // REST: Get parent message
      let response = await fetch("/lookup_messages", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify([bespoke.peekMessageStack()]),
      });
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return;
      }
      let data = await response.json();
      bespoke.assert(data.length === 1, "Expected exactly one message");
      this.parentMessage = data[0];

      // REST: Get root message title (if necessary)
      this.rootMessageTitle = this.parentMessage["title"];
      if (this.rootMessageTitle == null) {
        response = await fetch("/lookup_messages", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify([this.parentMessage["root-message-id"]]),
        });
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          return;
        }
        data = await response.json();
        bespoke.assert(data.length === 1, "Expected exactly one message");
        const rootMessage = data[0];
        this.rootMessageTitle = rootMessage["title"];
      }

      // Populate the page
      this.populatePage();
    } catch (error) {
      console.error("Fetching failed:", error);
    }
  }

  populatePage() {
    // Populate parent message
    document.getElementById("parent-title").innerHTML = this.rootMessageTitle;
    document.getElementById("parent-body").innerHTML = bespoke.formatMarkdown(
      this.parentMessage["body"]
    );
    document.getElementById("parent-author").textContent =
      this.parentMessage["author"];
    document.getElementById("parent-age").textContent =
      bespoke.formatSecondsSinceEpoch(this.parentMessage["created"]);
    document.getElementById("parent-replies").textContent =
      this.parentMessage["reply-count"];
  }
}

// Instantiate the class on DOMContentLoaded
document.addEventListener("DOMContentLoaded", () => {
  bespoke.init();
  addReply.init();
});

// Export the class instance
const addReply = new AddReply();
export default addReply
