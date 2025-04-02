// -*- fill-column: 100 -*-

"use strict";

const Login = (function () {
  const _username = Bespoke.getCookieValue("username");

  let _formUsernameElement;
  let _formUsernameErrorElement;
  let _formPasswordElement;
  let _loginButtonElement;

  Bespoke.onReady("login.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get form elements and add event listeners
    _formUsernameElement = document.getElementById("form-username");
    _formUsernameElement.addEventListener("input", () => _checkFormCompletion());
    _formUsernameElement.focus();
    _formUsernameErrorElement = document.getElementById("form-username-error");
    _formPasswordElement = document.getElementById("form-password");
    _formPasswordElement.addEventListener("input", () => _checkFormCompletion());
    _loginButtonElement = document.getElementById("login-button");

    _updatePage();
    _checkFormCompletion();
  }

  function _checkFormCompletion() {
    _formUsernameErrorElement.style.display = "none";
    _loginButtonElement.disabled =
      _formUsernameElement.value.trim() === "" ||
      _formPasswordElement.value.trim() === "";
  }

  function _updatePage() {
    _formUsernameElement.value = _username;
  }

  function loginNow(event) {
    Bespoke.ignoreEvent(event);
    _formUsernameErrorElement.style.display = "none";
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
        body: JSON.stringify(_formUsernameElement.value),
      });
      if (!getChallengeResponse.ok) {
        console.error(`Server error: ${getChallengeResponse.status}`);
        _formUsernameElement.focus();
        return;
      }
      const getChallengeResult = await getChallengeResponse.json();
      const generateClientResponseResult =
            await _generateClientResponse(
              _formPasswordElement.value,
              Bespoke.decodeBase64(getChallengeResult.passwordSalt),
              Bespoke.decodeBase64(getChallengeResult.challenge));

      // REST: Login
      const loginPayload =
            {username: _formUsernameElement.value,
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
          _formUsernameErrorElement.innerText = "Invalid username or password";
          _formUsernameErrorElement.style.display = "block";
        } else {
          console.error(`Server error: ${loginResponse.status}`);
        }
        _formUsernameElement.focus();
        return;
      }
      const loginResult = await loginResponse.json();

      // Set cookies
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
    const result = await Crypto.generateClientResponse(password, passwordSalt, challenge);
    Bespoke.hideLoadingSpinner();
    return result;
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

  return {
    loginNow,
    togglePassword
  };
})();
