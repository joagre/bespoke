// -*- fill-column: 100 -*-

"use strict";

const ChangePassword = (function () {
  const _MIN_PASSWORD_LENGTH = 12;
  let _formPassword1;
  let _formPassword2;
  let _formPasswordError;
  let _changeButton;

  Bespoke.onReady("change_password.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }
    _formPassword1 = document.getElementById("form-password1");
    _formPassword1.addEventListener("input", () => _checkFormCompletion());
    _formPassword2 = document.getElementById("form-password2");
    _formPassword2.addEventListener("input", () => _checkFormCompletion());
    _formPasswordError = document.getElementById("form-password-error");
    _changeButton = document.getElementById("change-button");
    _updatePage();
  }

  function _checkFormCompletion() {
    let disabled = false;
    if (_formPassword1.value.length < _MIN_PASSWORD_LENGTH ||
        _formPassword2.value.length < _MIN_PASSWORD_LENGTH) {
      disabled = true;
    } else if (_formPassword1.value != _formPassword2.value) {
      _formPasswordError.innerText = "Passwords do not match";
      _formPasswordError.style.display = "block";
      disabled = true;
    } else {
      _formPasswordError.style.display = "none";
    }
    _changeButton.disabled = disabled;
  }

  async function _updatePage() {
    try {
      const username = Bespoke.getCookieValue("username");
      document.getElementById("title-username").textContent = username;
      document.body.hidden = false;
      _formPassword1.focus();
    } catch (error) {
      console.error("Page update failed:", error);
    }
  }

  function changeNow(event) {
    Bespoke.ignoreEvent(event);
    _formPasswordError.style.display = "none";
    _updateServer();
  }

  async function _updateServer() {
    try {
      // REST: Change password
      const result = await _generatePasswordHash(_formPassword1.value);
      const payload = {
        passwordSalt: Bespoke.encodeBase64(result.passwordSalt),
        passwordHash: Bespoke.encodeBase64(result.passwordHash)};
      const response = await fetch("/api/change_password", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(payload)
      });
      if (!response.ok) {
        if (response.status === 401) {
          Bespoke.navigateTo("loader.html");
          return;
        } else if (response.status === 403) {
          _formPasswordError.innerText = "Password could not be changed";
          _formPasswordError.style.display = "block";
        } else {
          console.error(`Server error: ${response.status}`);
        }
        _formPassword1.focus();
        return;
      }
      Bespoke.navigateTo("loader.html");
    } catch (error) {
      console.error("Change password failed", error);
    }
  }

  async function _generatePasswordHash(password) {
    Bespoke.showLoadingSpinner();
    const result = await Crypto.generatePasswordHash(password);
    Bespoke.hideLoadingSpinner();
    return result;
  }

  function togglePassword(event, n) {
    Bespoke.ignoreEvent(event);
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

  function generateStrongPassword(event) {
    Bespoke.ignoreEvent(event);
    const password = Bespoke.generateStrongPassword();
    _formPassword1.value = password;
    _formPassword2.focus();
    _checkFormCompletion();
  }

  return {
    changeNow,
    togglePassword,
    generateStrongPassword
  };
})();
