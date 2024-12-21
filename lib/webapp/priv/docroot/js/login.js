import bespoke from "/js/bespoke.js";

class Login {
  constructor() {
    bespoke.onReady("login.html", () => this._load());
  }

  _load() {
    if (!bespoke.hasSessionId()) {
      bespoke.navigateTo("loader.html");
      return;
    }
    this._formUsername = document.getElementById("form-username");
    this._formUsername.addEventListener("input", () => this._checkFormCompletion());
    this._formUsername.focus();
    this._formUsernameError = document.getElementById("form-username-error");
    this._formPassword = document.getElementById("form-password");
    this._formPassword.addEventListener("input", () => this._checkFormCompletion());
    this._loginButton = document.getElementById("login-button");
    this._updatePage();
    this._checkFormCompletion();
  }

  _checkFormCompletion() {
    this._formUsernameError.style.display = "none";
    this._loginButton.disabled = this._formUsername.value.trim() === "";
  }

  async _updatePage() {
    try {
      const username = bespoke.getCookieValue("username");
      this._formUsername.value = username;
    } catch (error) {
      console.error("Page update failed:", error);
    }
  }

  loginNow(event) {
    event.preventDefault();
    this._formUsernameError.style.display = "none";
    const updateServer = async () => {
      try {
        // REST: Login
        const payload = {
          username: this._formUsername.value,
          password: this._formPassword.value,
        };
        const response = await fetch("/login", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify(payload),
        });
        if (!response.ok) {
          if (response.status === 401) {
            this._formUsernameError.innerText = "Invalid username or password";
            this._formUsernameError.style.display = "block";
          } else {
            console.error(`Server error: ${response.status}`);
          }
          this._formUsername.focus();
          return;
        }
        const result = await response.json();
        bespoke.setCookieValue("userId", result["user-id"]);
        bespoke.setCookieValue("username", result["username"]);
        bespoke.setCookieValue("sessionId", result["session-id"]);
        // REST: Acknowledge captive portal
        fetch("/captive_portal_ack", { method: "GET", mode: "no-cors" })
          .then(() => {
            console.log("Captive portal acknowledgment sent to server");
            bespoke.navigateTo("top_posts.html");
          })
          .catch(err => console.error("Error:", err));
      } catch (error) {
        console.error("Login failed", error);
      }
    };
    updateServer();
  }

  togglePassword(event) {
    event.preventDefault();
    const togglePassword = document.getElementById("toggle-password");
    var type =
        this._formPassword.getAttribute("type") === "password" ? "text" :
        "password";
    this._formPassword.setAttribute("type", type);
    var iconType = type === "password" ? "lock" : "unlock";
    togglePassword.setAttribute("uk-icon", "icon: " + iconType);
    UIkit.icon(togglePassword).$destroy();
    UIkit.icon(togglePassword);
  }
}

const login = new Login();
export default login
