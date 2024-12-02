import bespoke from "/js/bespoke.js";

class Login {
  constructor() {
    bespoke.onReady("/login.html", () => this._load());
  }

  _load() {
    this._autoUsername = document.getElementById("auto-username");
    this._formUsername = document.getElementById("form-username");
    this._formUsername.addEventListener("input", () => this._checkFormCompletion());
    this._formUsernameError = document.getElementById("form-username-error");
    this._formPassword = document.getElementById("form-password");
    this._loginButton = document.getElementById("login-button");
    this._updatePage();
  }

  _checkFormCompletion() {
    this._loginButton.disabled = this._formUsername.value.trim() === "";
  }

  async _updatePage() {
    try {
      let response = await fetch("/get_username");
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return;
      }
      let username = await response.json();
      this._autoUsername.innerText = username;
      this._formUsername.value = username;
    } catch (error) {
      console.error("Page update failed:", error);
    }
  }

  authenticate(event) {
    event.preventDefault();

    // Hide error span under form-username input
    this._formUsernameError.style.display = "none";

    const authenticate = {
      username: this._formUsername.value,
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
          // Show error span under form-username input
          this._formUsernameError.innerText = "Invalid username or password";
          this._formUsernameError.style.display = "block";
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

const login = new Login();
export default login
