import bespoke from "/js/bespoke.js";

class SwitchUser {
  constructor() {
    bespoke.onReady("switch_user.html", () => this._load());
  }

  _load() {
    if (!bespoke.isCookieSet()) {
      bespoke.navigateTo("loader.html");
      return;
    }
    this._formUsername = document.getElementById("form-username");
    this._formUsername.addEventListener("input", () => this._checkFormCompletion());
    this._formUsername.focus();
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
    if (this._formUsername.value == bespoke.getCookieValue("username")) {
      bespoke.navigateTo("top_posts.html");
      return;
    }
    this._formUsernameError.style.display = "none";
    const updateServer = async () => {
      try {
        // REST: Switch user
        const payload = {
          username: this._formUsername.value,
          password: this._formPassword.value,
        };
        const response = await fetch("/switch_user", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify(payload),
        });
        if (!response.ok) {
          if (response.status === 403) {
            this._formUsernameError.innerText = "User could be switched";
            this._formUsernameError.style.display = "block";
          } else {
            console.error(`Server error: ${response.status}`);
          }
          this._formUsername.focus();
          return;
        }
        const result = await response.json();
        bespoke.setCookieValue("username", result["username"]);
        bespoke.setCookieValue("sessionId", result["session-id"]);
        bespoke.navigateTo("top_posts.html");
      } catch (error) {
        console.error("User switch failed", error);
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

  generateStrongPassword(event) {
    event.preventDefault();
    const password = bespoke.generateStrongPassword();
    this._formPassword.value = password;
    this._checkFormCompletion();
  }
}

const switchUser = new SwitchUser();
export default switchUser
