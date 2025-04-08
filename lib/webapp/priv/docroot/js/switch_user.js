// -*- fill-column: 100 -*-

"use strict";

const SwitchUser = (function () {
  const _MIN_PASSWORD_LENGTH = 8;
  const _username = Bespoke.getCookieValue("username");

  let _formUsernameElement;
  let _formUsernameErrorElement;
  let _formPasswordElement;
  let _switchButton;

  Bespoke.onReady("switch_user.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get form elements
    _formUsernameElement = document.getElementById("form-username");
    _formUsernameElement.addEventListener("input", () => _checkFormCompletion());
    _formUsernameErrorElement = document.getElementById("form-username-error");
    _formPasswordElement = document.getElementById("form-password");
    _formPasswordElement.addEventListener("input", () => _checkFormCompletion());
    _switchButton = document.getElementById("switch-button");

    _updatePage();
  }

  function _checkFormCompletion() {
    let disabled = false;
    if (_formUsernameElement.value.trim() === "") {
      disabled = true;
    } else if (_formPasswordElement.value.trim().length > 0 &&
               _formPasswordElement.value.trim().length < _MIN_PASSWORD_LENGTH) {
      disabled = true;
    }
    _switchButton.disabled = disabled;
  }

  function _updatePage() {
    document.getElementById("title-username").textContent = _username;
    document.body.hidden = false;
    _formUsernameElement.focus();
  }

  function switchNow(event) {
    Bespoke.ignoreEvent(event);
    if (_formUsernameElement.value == Bespoke.getCookieValue("username")) {
      _formUsernameErrorElement.innerText = "You are already that user!";
      _formUsernameErrorElement.style.display = "block";
      _formUsernameElement.focus();
      return;
    }
    _formUsernameErrorElement.style.display = "none";
    _updateServer();
  }

  async function _updateServer() {
    try {
      let encodedPasswordSalt = null;
      let encodedPasswordHash = null;
      let encodedClientResponse = null;

      // REST: Generate client response if password is not empty
      if (_formPasswordElement.value.length > 0) {
        const getChallengeResponse = await fetch("/api/generate_challenge", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify(_formUsernameElement.value),
        });
        if (!getChallengeResponse.ok) {
          console.error(`Server error: ${getChallengeResponse.status}`);
          _formUsernameElement.focus();
          return;
        }
        const getChallengeResult = await getChallengeResponse.json();
        const clientResponseResult =
              await _generateClientResponse(
                _formPasswordElement.value,
                Bespoke.decodeBase64(getChallengeResult.passwordSalt),
                Bespoke.decodeBase64(getChallengeResult.challenge));
        encodedPasswordSalt = getChallengeResult.passwordSalt;
        encodedPasswordHash = Bespoke.encodeBase64(clientResponseResult.passwordHash);
        encodedClientResponse = Bespoke.encodeBase64(clientResponseResult.data);
      }

      // REST: Switch user
      const switchUserPayload = {
        username: _formUsernameElement.value,
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
          _formUsernameErrorElement.innerText = "Invalid username or password";
          _formUsernameErrorElement.style.display = "block";
        } else {
          console.error(`Server error: ${switchUserResponse.status}`);
        }
        _formUsernameElement.focus();
        return;
      }
      const switchUserResult = await switchUserResponse.json();

      // Set cookies
      Bespoke.setCookieValue("userId", switchUserResult.userId);
      Bespoke.setCookieValue("username", switchUserResult.username);
      Bespoke.setCookieValue("sessionId", switchUserResult.sessionId);

      Bespoke.navigateTo("index.html");
    } catch (error) {
      console.log("User switch failed", error);
    }
  }

  function togglePassword(event) {
    Bespoke.ignoreEvent(event);
    const togglePassword = document.getElementById("toggle-password");
    var type = _formPasswordElement.getAttribute("type") === "password" ? "text" : "password";
    _formPasswordElement.setAttribute("type", type);
    var iconType = type === "password" ? "lock" : "unlock";
    togglePassword.setAttribute("uk-icon", "icon: " + iconType);
    UIkit.icon(togglePassword).$destroy();
    UIkit.icon(togglePassword);
  }

  function generateStrongPassword(event) {
    Bespoke.ignoreEvent(event);
    const password = Bespoke.generateStrongPassword();
    _formPasswordElement.value = password;
    _formPasswordElement.focus();
    _checkFormCompletion();
  }

  async function _generateClientResponse(password, passwordSalt, challenge) {
    Bespoke.showLoadingSpinner();
    const result = await Crypto.generateClientResponse(password, passwordSalt, challenge);
    Bespoke.hideLoadingSpinner();
    return result;
  }

  return {
    switchNow,
    togglePassword,
    generateStrongPassword
  };
})();
