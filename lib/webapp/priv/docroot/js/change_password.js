import bespoke from "/js/bespoke.js";
import auth from "/js/auth.js";

class ChangePassword {
  _MIN_PASSWORD_LENGTH = 12;

  constructor() {
    bespoke.onReady("change_password.html", () => this._load());
  }

  _load() {
    if (!bespoke.hasSessionId()) {
      bespoke.navigateTo("loader.html");
      return;
    }
    this._formPassword1 = document.getElementById("form-password1");
    this._formPassword1.addEventListener("input",
                                         () => this._checkFormCompletion());
    this._formPassword2 = document.getElementById("form-password2");
    this._formPassword2.addEventListener("input",
                                         () => this._checkFormCompletion());
    this._formPasswordError = document.getElementById("form-password-error");
    this._changeButton = document.getElementById("change-button");
    this._updatePage();
  }

  _checkFormCompletion() {
    let disabled = false;
    if (this._formPassword1.value.length < this._MIN_PASSWORD_LENGTH ||
        this._formPassword2.value.length < this._MIN_PASSWORD_LENGTH) {
      disabled = true;
    } else {
      if (this._formPassword1.value != this._formPassword2.value) {
        this._formPasswordError.innerText = "Passwords do not match";
        this._formPasswordError.style.display = "block";
        disabled = true;
      } else {
        this._formPasswordError.style.display = "none";
      }
    }
    this._changeButton.disabled = disabled;
  }

  async _updatePage() {
    try {
      const username = bespoke.getCookieValue("username");
      document.getElementById("title-username").textContent = username;
      document.body.hidden = false;
      this._formPassword1.focus();
    } catch (error) {
      console.error("Page update failed:", error);
    }
  }

  changeNow(event) {
    event.preventDefault();
    this._formPasswordError.style.display = "none";
    const updateServer = async () => {
      try {
        // REST: Change password
        const result =
              await this._generatePasswordHash(this._formPassword1.value);
        const payload = {
          passwordSalt: bespoke.encodeBase64(result.passwordSalt),
          passwordHash: bespoke.encodeBase64(result.passwordHash)};
        const response = await fetch("/change_password", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify(payload)
        });
        if (!response.ok) {
          if (response.status === 401) {
            bespoke.navigateTo("loader.html");
            return;
          } else if (response.status === 403) {
            this._formPasswordError.innerText = "Password could not be changed";
            this._formPasswordError.style.display = "block";
          } else {
            console.error(`Server error: ${response.status}`);
          }
          this._formPassword1.focus();
          return;
        }
        bespoke.navigateTo("top_posts.html");
      } catch (error) {
        console.error("Change password failed", error);
      }
    };
    updateServer();
  }

  async _generatePasswordHash(password) {
    bespoke.showLoadingSpinner();
    const result = await auth.generatePasswordHash(password);
    bespoke.hideLoadingSpinner();
    return result;
  }

  togglePassword(event, n) {
    event.preventDefault();
    const togglePassword = document.getElementById("toggle-password" + n);
    const formPassword = document.getElementById("form-password" + n);
    var type = formPassword.getAttribute("type") === "password" ? "text" :
        "password";
    formPassword.setAttribute("type", type);
    var iconType = type === "password" ? "lock" : "unlock";
    togglePassword.setAttribute("uk-icon", "icon: " + iconType);
    UIkit.icon(togglePassword).$destroy();
    UIkit.icon(togglePassword);
  }

  generateStrongPassword(event) {
    event.preventDefault();
    const password = bespoke.generateStrongPassword();
    this._formPassword1.value = password;
    this._formPassword2.focus();
    this._checkFormCompletion();
  }
}

const changePassword = new ChangePassword();
export default changePassword
