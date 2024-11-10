// Import dependencies
import bespoke from "/js/bespoke.js";

// Ensure uhtml.min.js is imported in the HTML file before this script

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
      "root-message-id":
      this.parentMessage["root-message-id"] ?? this.parentMessage["id"],
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
        bespoke2.setCookieValue("reply-pop", false);
        bespoke.gotoPage(event, "message2.html");
      } catch (error) {
        console.error("Fetching failed:", error);
        // Optional: Display an error message to the user
      }
    };

    updateServer();
  }

  gotoReply(event, messageId) {
    bespoke2.setCookieValue("reply-pop", true);
    bespoke2.gotoPage(event, "add_reply2.html", messageId);
  }

  goBack(event) {
    if (bespoke2.getCookieValue("reply-pop")) {
      bespoke2.popMessageStack();
      bespoke2.setCookieValue("reply-pop", false);
    }

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
  addReply2.init();
});

// Export the class instance
const addReply2 = new AddReply2();
export default addReply2
