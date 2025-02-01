// -*- fill-column: 100 -*-

"use strict";

const SwitchUser = (function () {
  let _formUsername;
  let _formUsernameError;
  let _formPassword;
  let _switchButton;
  const _MIN_PASSWORD_LENGTH = 12;

  function init() {
    Bespoke.onReady("switch_user.html", () => _load());
  }

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }
    _formUsername = document.getElementById("form-username");
    _formUsername.addEventListener("input", () => _checkFormCompletion());
    _formUsernameError = document.getElementById("form-username-error");
    _formPassword = document.getElementById("form-password");
    _formPassword.addEventListener("input", () => _checkFormCompletion());
    _switchButton = document.getElementById("switch-button");
    _updatePage();
  }

  function _checkFormCompletion() {
    let disabled = false;
    if (_formUsername.value.trim() === "") {
      disabled = true;
    } else if (_formPassword.value.trim().length > 0 &&
               _formPassword.value.trim().length < _MIN_PASSWORD_LENGTH) {
      disabled = true;
    }
    _switchButton.disabled = disabled;
  }

  async function _updatePage() {
    try {
      const username = Bespoke.getCookieValue("username");
      document.getElementById("title-username").textContent = username;
      document.body.hidden = false;
      _formUsername.focus();
    } catch (error) {
      console.error("Page update failed:", error);
    }
  }

  function switchNow(event) {
    Bespoke.ignoreEvent(event);
    if (_formUsername.value == Bespoke.getCookieValue("username")) {
      _formUsernameError.innerText = "You are already that user!";
      _formUsernameError.style.display = "block";
      _formUsername.focus();
      return;
    }
    _formUsernameError.style.display = "none";
    _updateServer();
  }

  async function _updateServer() {
    try {
      let encodedPasswordSalt = null;
      let encodedPasswordHash = null;
      let encodedClientResponse = null;
      // REST: Generate client response if password is not empty
      if (_formPassword.value.length > 0) {
        const getChallengeResponse = await fetch("/api/generate_challenge", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify(_formUsername.value),
        });
        if (!getChallengeResponse.ok) {
          console.error(`Server error: ${getChallengeResponse.status}`);
          _formUsername.focus();
          return;
        }
        const getChallengeResult = await getChallengeResponse.json();
        const clientResponseResult =
              await _generateClientResponse(
                _formPassword.value,
                Bespoke.decodeBase64(getChallengeResult.passwordSalt),
                Bespoke.decodeBase64(getChallengeResult.challenge));
        encodedPasswordSalt = getChallengeResult.passwordSalt;
        encodedPasswordHash = Bespoke.encodeBase64(clientResponseResult.passwordHash);
        encodedClientResponse = Bespoke.encodeBase64(clientResponseResult.data);
      }
      // REST: Switch user
      const switchUserPayload = {
        username: _formUsername.value,
        passwordSalt: encodedPasswordSalt,
        passwordHash: encodedPasswordHash,
        clientResponse: encodedClientResponse
      };
      const switchUserResponse = await fetch("/api/switch_user", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(switchUserPayload),
      });
      if (!switchUserResponse.ok) {
        if (switchUserResponse.status === 401) {
          Bespoke.navigateTo("loader.html");
          return;
        } else if (switchUserResponse.status === 403) {
          _formUsernameError.innerText = "Invalid username or password";
          _formUsernameError.style.display = "block";
        } else {
          console.error(`Server error: ${switchUserResponse.status}`);
        }
        _formUsername.focus();
        return;
      }
      const switchUserResult = await switchUserResponse.json();
      Bespoke.setCookieValue("userId", switchUserResult.userId);
      Bespoke.setCookieValue("username", switchUserResult.username);
      Bespoke.setCookieValue("sessionId", switchUserResult.sessionId);
      Bespoke.navigateTo("index.html");
    } catch (error) {
      console.error("User switch failed", error);
    }
  }

  function togglePassword(event) {
    Bespoke.ignoreEvent(event);
    const togglePassword = document.getElementById("toggle-password");
    var type =
        _formPassword.getAttribute("type") === "password" ? "text" :
        "password";
    _formPassword.setAttribute("type", type);
    var iconType = type === "password" ? "lock" : "unlock";
    togglePassword.setAttribute("uk-icon", "icon: " + iconType);
    UIkit.icon(togglePassword).$destroy();
    UIkit.icon(togglePassword);
  }

  function generateStrongPassword(event) {
    Bespoke.ignoreEvent(event);
    const password = Bespoke.generateStrongPassword();
    _formPassword.value = password;
    _formPassword.focus();
    _checkFormCompletion();
  }

  async function _generateClientResponse(password, passwordSalt, challenge) {
    Bespoke.showLoadingSpinner();
    const result =
          await Auth.generateClientResponse(password, passwordSalt, challenge);
    Bespoke.hideLoadingSpinner();
    return result;
  }

  return {
    init,
    switchNow,
    togglePassword,
    generateStrongPassword
  };
})();

SwitchUser.init();
