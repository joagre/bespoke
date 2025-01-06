import bespoke from "/js/bespoke.js";
import auth from "/js/auth.js";

class SwitchUser {
  constructor() {
    bespoke.onReady("switch_user.html", () => this._load());
  }

  _load() {
    if (!bespoke.hasSessionId()) {
      bespoke.navigateTo("loader.html");
      return;
    }
    this._formUsername = document.getElementById("form-username");
    this._formUsername.addEventListener("input",
                                        () => this._checkFormCompletion());
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
      document.body.hidden = false;
      this._formUsername.focus();
    } catch (error) {
      console.error("Page update failed:", error);
    }
  }

  switchNow(event) {
    bespoke.ignoreEvent(event);
    if (this._formUsername.value == bespoke.getCookieValue("username")) {
      this._formUsernameError.innerText = "You are already that user!";
      this._formUsernameError.style.display = "block";
      this._formUsername.focus();
      return;
    }
    this._formUsernameError.style.display = "none";
    const updateServer = async () => {
      try {
        let encodedPasswordSalt = null;
        let encodedPasswordHash = null;
        let encodedClientResponse = null;
        // REST: Generate client response if password is not empty
        if (this._formPassword.value.length > 0) {
          const getChallengeResponse = await fetch("/generate_challenge", {
            method: "POST",
            headers: {
              "Content-Type": "application/json",
            },
            body: JSON.stringify(this._formUsername.value),
          });
          if (!getChallengeResponse.ok) {
            console.error(`Server error: ${getChallengeResponse.status}`);
            this._formUsername.focus();
            return;
          }
          const getChallengeResult = await getChallengeResponse.json();
          const clientResponseResult =
                await this._generateClientResponse(
                  this._formPassword.value,
                  bespoke.decodeBase64(getChallengeResult.passwordSalt),
                  bespoke.decodeBase64(getChallengeResult.challenge));
          encodedPasswordSalt = getChallengeResult.passwordSalt;
          encodedPasswordHash =
            bespoke.encodeBase64(clientResponseResult.passwordHash);
          encodedClientResponse = bespoke.encodeBase64(clientResponseResult.data);
        }
        // REST: Switch user
        const switchUserPayload = {
          username: this._formUsername.value,
          passwordSalt: encodedPasswordSalt,
          passwordHash: encodedPasswordHash,
          clientResponse: encodedClientResponse
        };
        const switchUserResponse = await fetch("/switch_user", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify(switchUserPayload),
        });
        if (!switchUserResponse.ok) {
          if (switchUserResponse.status === 403) {
            this._formUsernameError.innerText = "Invalid username or password";
            this._formUsernameError.style.display = "block";
          } else {
            console.error(`Server error: ${switchUserResponse.status}`);
          }
          this._formUsername.focus();
          return;
        }
        const switchUserResult = await switchUserResponse.json();
        bespoke.setCookieValue("userId", switchUserResult.userId);
        bespoke.setCookieValue("username", switchUserResult.username);
        bespoke.setCookieValue("sessionId", switchUserResult.sessionId);
        bespoke.navigateTo("index.html");
      } catch (error) {
        console.error("User switch failed", error);
      }
    };
    updateServer();
  }

  togglePassword(event) {
    bespoke.ignoreEvent(event);
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
    bespoke.ignoreEvent(event);
    const password = bespoke.generateStrongPassword();
    this._formPassword.value = password;
    this._formPassword.focus();
    this._checkFormCompletion();
  }

  async _generateClientResponse(password, passwordSalt, challenge) {
    bespoke.showLoadingSpinner();
    const result =
          await auth.generateClientResponse(password, passwordSalt, challenge);
    bespoke.hideLoadingSpinner();
    return result;
  }
}

const switchUser = new SwitchUser();
export default switchUser
