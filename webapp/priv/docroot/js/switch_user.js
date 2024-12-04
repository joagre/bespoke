import bespoke from "/js/bespoke.js";

class SwitchUser {
  constructor() {
    bespoke.onReady("/switch_user.html", () => this._load());
  }

  _load() {
    this._formUsername = document.getElementById("form-username");
    this._formUsername.addEventListener("input", () => this._checkFormCompletion());
    this._formUsernameError = document.getElementById("form-username-error");
    this._formPassword = document.getElementById("form-password");
    this._switchButton = document.getElementById("switch-button");
    this._updatePage();
  }

  _checkFormCompletion() {
    this._switchButton.disabled = this._formUsername.value.trim() === "";
  }

  async _updatePage() {
    try {
      const username = bespoke.getCookieValue("username");
      document.getElementById("title-username").textContent = username;
    } catch (error) {
      console.error("Page update failed:", error);
    }
  }

  switchNow(event) {
    event.preventDefault();

    // Hide error span under form-username input
    this._formUsernameError.style.display = "none";

    const switchUser = {
      username: this._formUsername.value,
      password: this._formPassword.value,
    };

    const updateServer = async () => {
      // No need to switch user if already logged in as the same user
      if (this._formUsername.value == bespoke.getCookieValue("username")) {
        bespoke.navigateTo("/top_posts.html");
        return;
      }
      try {
        // REST: Switch user
        const response = await fetch("/switch_user", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify(switchUser),
        });
        if (!response.ok) {
          console.warn(`Server error: ${response.status}`);
          // Show error span under form-username input
          this._formUsernameError.innerText = "Invalid username or password";
          this._formUsernameError.style.display = "block";
          return;
        }
        const switchUserResult = await response.json();
        // Set cookies
        bespoke.setCookieValue("username", switchUserResult["username"]);
        bespoke.setCookieValue("sessionId", switchUserResult["session-id"]);
        bespoke.navigateTo("/top_posts.html");
      } catch (error) {
        console.error("User switch failed", error);
      }
    };

    updateServer();
  }

  togglePassword(event) {
    event.preventDefault();
    const togglePassword = document.getElementById("toggle-password");
    const passwordInput = document.getElementById("form-password");
    var type =
        passwordInput.getAttribute("type") === "password" ? "text" : "password";
    passwordInput.setAttribute("type", type);
    var iconType = type === "password" ? "lock" : "unlock";
    togglePassword.setAttribute("uk-icon", "icon: " + iconType);
    UIkit.icon(togglePassword).$destroy();
    UIkit.icon(togglePassword);
  }

  generateStrongPassword(event) {
    event.preventDefault();
    const passwordInput = document.getElementById("form-password");
    const password = bespoke.generateStrongPassword();
    passwordInput.value = password;
    this._checkFormCompletion();
  }
}

const switchUser = new SwitchUser();
export default switchUser
