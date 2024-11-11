// Import dependencies
import bespoke2 from "/js/bespoke2.js";

class AddReply2 {
  constructor() {
    this.parentMessage = null;
    this.rootMessageTitle = null;
    this._formFields = [];
    this._addButton = null;
  }

  init() {
    bespoke2.initializeCookieState();
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
      "root-message-id": (this.parentMessage["root-message-id"] != null)
        ? this.parentMessage["root-message-id"]
        : this.parentMessage["id"];
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
    bespoke2.setCookieValue("pop-message-stack", popMessageStack);
    bespoke2.gotoPage(event, "add_reply2.html", messageId);
  }

  goBack(event, ignorePopMessageStack) {
    if (!ignorePopMessageStack &&
        bespoke2.getCookieValue("pop-message-stack")) {
      bespoke2.popMessageStack();
    }
    bespoke2.setCookieValue("pop-message-stack", false);
    bespoke2.gotoPage(event, 'message2.html')
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
        body: JSON.stringify([bespoke2.peekMessageStack()]),
      });
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return;
      }
      let data = await response.json();
      bespoke2.assert(data.length === 1, "Expected exactly one message");
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
        bespoke2.assert(data.length === 1, "Expected exactly one message");
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
    document.getElementById("parent-body").innerHTML = bespoke2.formatMarkdown(
      this.parentMessage["body"]
    );
    document.getElementById("parent-author").textContent =
      this.parentMessage["author"];
    document.getElementById("parent-age").textContent =
      bespoke2.formatSecondsSinceEpoch(this.parentMessage["created"]);
    document.getElementById("parent-replies").textContent =
      this.parentMessage["reply-count"];
  }
}

// Instantiate the class on DOMContentLoaded
document.addEventListener("DOMContentLoaded", () => {
  bespoke2.init();
  addReply2.init();
});

// Export the class instance
const addReply2 = new AddReply2();
export default addReply2
