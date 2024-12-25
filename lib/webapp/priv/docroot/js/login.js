import bespoke from "/js/bespoke.js";
import auth from "/js/auth.js";

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
    this._formUsername.addEventListener("input",
                                        () => this._checkFormCompletion());
    this._formUsername.focus();
    this._formUsernameError = document.getElementById("form-username-error");
    this._formPassword = document.getElementById("form-password");
    this._formPassword.addEventListener("input",
                                        () => this._checkFormCompletion());
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
        // REST: Generate challenge
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
        // REST: Verify client response
        const clientResponse = await this._generateClientResponse(
          this._formPassword.value,
          bespoke.decodeBase64(getChallengeResult.salt),
          bespoke.decodeBase64(getChallengeResult.challenge));
        const verifyClientResponsePayload =
              {username: this._formUsername.value,
               clientResponse: bespoke.encodeBase64(clientResponse),
               salt: getChallengeResult.salt,
               challenge: getChallengeResult.challenge};
        const verifyClientResponse = await fetch("/verify_client_response", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify(verifyClientResponsePayload),
        });
        if (!verifyClientResponse.ok) {
          if (verifyClientResponse.status === 401) {
            this._formUsernameError.innerText = "Invalid username or password";
            this._formUsernameError.style.display = "block";
          } else {
            console.error(`Server error: ${verifyClientResponse.status}`);
          }
          this._formUsername.focus();
          return;
        }
        const verifyClientResult = await verifyClientResponse.json();
        bespoke.setCookieValue("userId", verifyClientResult["user-id"]);
        bespoke.setCookieValue("username", verifyClientResult["username"]);
        bespoke.setCookieValue("sessionId", verifyClientResult["session-id"]);
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

  async _generateClientResponse(password, salt, challenge) {
    bespoke.showLoadingSpinner();
    const result =
          await auth.generateClientResponse(password, salt, challenge);
    bespoke.hideLoadingSpinner();
    return result;
  }
}

const login = new Login();
export default login
