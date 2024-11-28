import bespoke from "/js/bespoke.js";

class Login {
  init() {
    this._autoAlias = document.getElementById("auto-alias");
    this._formAlias = document.getElementById("form-alias");
    this._formAlias.addEventListener("input", () => this._checkFormCompletion());
    this._formAliasError = document.getElementById("form-alias-error");
    this._formPassword = document.getElementById("form-password");
    this._loginButton = document.getElementById("login-button");
    this._updatePage();
  }

  _checkFormCompletion() {
    this._loginButton.disabled = this._formAlias.value.trim() === "";
  }

  async _updatePage() {
    try {
      let response = await fetch("/get_auto_alias");
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return;
      }
      let alias = await response.json();
      this._autoAlias.innerText = alias;
      this._formAlias.value = alias;
    } catch (error) {
      console.error("Fetching failed:", error);
    }
  }

  authenticate(event) {
    event.preventDefault();

    // Hide error span under form-alias input
    this._formAliasError.style.display = "none";

    const authenticate = {
      name: this._formAlias.value,
      password: this._formPassword.value,
    };

    const updateServer = async () => {
      try {
        // REST: Authenticate
        const response = await fetch("/authenticate", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify(authenticate),
        });
        if (!response.ok) {
          console.warn(`Server error: ${response.status}`);
          // Show error span under form-alias input
          this._formAliasError.innerText = "Invalid alias or password";
          this._formAliasError.style.display = "block";
          return;
        }
        // REST: Acknowledge captive portal
        fetch("/captive_portal_ack", { method: "GET", mode: "no-cors" })
          .then(() => {
            console.log("Captive portal acknowledgment sent to server");
            window.location.href = "/top_posts.html";
          })
          .catch(err => console.error('Error:', err));
      } catch (error) {
        console.error("Fetching failed:", error);
      }
    };

    updateServer();
  }
}

document.addEventListener("DOMContentLoaded", () => {
  bespoke.init();
  login.init();
});

const login = new Login();
export default login
