// Import dependencies
import bespoke from "/js/bespoke.js";

class Login {
  constructor() {
    this._aliasInput = null;
    this._passwordInput = null;
    this._loginButton = null;
  }

  init() {
    this._aliasInput = document.getElementById("form-alias");
    this._aliasInput.addEventListener("input", () => this._checkFormCompletion());
    this._passwordInput = document.getElementById("form-password");
    this._loginButton = document.getElementById("login-button");
    this._updatePage();
  }

  async _updatePage() {
    try {
      // REST: Get parent message
      let response = await fetch("/get_generated_alias", {
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



/*

  proceed() {
    // Signal the server to prepare for portal resolution
    fetch("/captive_portal_ack", { method: "GET", mode: "no-cors" })
      .then(() => {
        console.log('Acknowledgment sent to server');
        window.location.href = 'http://bespoke.local/posts.html';
      })
      .catch(err => console.error('Error:', err));
      }
      */

  _checkFormCompletion() {
    this._loginButton.disabled = this._aliasInput.value.trim() === "";
  }
}

// Instantiate the class on DOMContentLoaded
document.addEventListener("DOMContentLoaded", () => {
  bespoke.init();
  login.init();
});

// Export the class instance
const login = new Login();
export default login


//    <!--

//    <a href="javascript:void(0)" target="_blank" --
//      --onclick="proceed()">Click here to continue</a> -->
