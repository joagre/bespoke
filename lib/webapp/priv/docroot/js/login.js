// -*- fill-column: 100 -*-

"use strict";

const Login = (function () {
  let _formUsername;
  let _formUsernameError;
  let _formPassword;
  let _loginButton;

  Bespoke.onReady("login.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }
    _formUsername = document.getElementById("form-username");
    _formUsername.addEventListener("input", () => _checkFormCompletion());
    _formUsername.focus();
    _formUsernameError = document.getElementById("form-username-error");
    _formPassword = document.getElementById("form-password");
    _formPassword.addEventListener("input", () => _checkFormCompletion());
    _loginButton = document.getElementById("login-button");
    _updatePage();
    _checkFormCompletion();
  }

  function _checkFormCompletion() {
    _formUsernameError.style.display = "none";
    _loginButton.disabled = _formUsername.value.trim() === "" || _formPassword.value.trim() === "";
  }

  function _updatePage() {
    try {
      const username = Bespoke.getCookieValue("username");
      _formUsername.value = username;
    } catch (error) {
      console.error("Page update failed:", error);
    }
  }

  function loginNow(event) {
    Bespoke.ignoreEvent(event);
    _formUsernameError.style.display = "none";
    _updateServer();
  }

  async function _updateServer() {
    try {
      // REST: Generate challenge
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
      const generateClientResponseResult =
            await _generateClientResponse(
              _formPassword.value,
              Bespoke.decodeBase64(getChallengeResult.passwordSalt),
              Bespoke.decodeBase64(getChallengeResult.challenge));
      // REST: Login
      const loginPayload =
            {username: _formUsername.value,
             clientResponse: Bespoke.encodeBase64(generateClientResponseResult.data)};
      const loginResponse = await fetch("/api/login", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(loginPayload),
      });
      if (!loginResponse.ok) {
        if (loginResponse.status === 403) {
          _formUsernameError.innerText = "Invalid username or password";
          _formUsernameError.style.display = "block";
        } else {
          console.error(`Server error: ${loginResponse.status}`);
        }
        _formUsername.focus();
        return;
      }
      const loginResult = await loginResponse.json();
      Bespoke.setCookieValue("userId", loginResult.userId);
      Bespoke.setCookieValue("username", loginResult.username);
      Bespoke.setCookieValue("sessionId", loginResult.sessionId);
      Bespoke.navigateTo("index.html");
    } catch (error) {
      console.error("Login failed", error);
    }
  }

  async function _generateClientResponse(password, passwordSalt, challenge) {
    Bespoke.showLoadingSpinner();
    const result = await Auth.generateClientResponse(password, passwordSalt, challenge);
    Bespoke.hideLoadingSpinner();
    return result;
  }

  function togglePassword(event) {
    Bespoke.ignoreEvent(event);
    const togglePassword = document.getElementById("toggle-password");
    var type = _formPassword.getAttribute("type") === "password" ? "text" : "password";
    _formPassword.setAttribute("type", type);
    var iconType = type === "password" ? "lock" : "unlock";
    togglePassword.setAttribute("uk-icon", "icon: " + iconType);
    UIkit.icon(togglePassword).$destroy();
    UIkit.icon(togglePassword);
  }

  return {
    loginNow,
    togglePassword
  };
})();
