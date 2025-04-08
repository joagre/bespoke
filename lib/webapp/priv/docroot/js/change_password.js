// -*- fill-column: 100 -*-

"use strict";

const ChangePassword = (function () {
  const _MIN_PASSWORD_LENGTH = 8;
  const _username = Bespoke.getCookieValue("username");

  let _titleUsernameElement;
  let _formPassword1Element;
  let _formPassword2Element;
  let _formPasswordErrorElement;
  let _changeButtonElement;

  Bespoke.onReady("change_password.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get header elements
    _titleUsernameElement = document.getElementById("title-username");

    // Get form elements
    _formPassword1Element = document.getElementById("form-password1");
    _formPassword1Element.addEventListener("input", () => _checkFormCompletion());
    _formPassword2Element = document.getElementById("form-password2");
    _formPassword2Element.addEventListener("input", () => _checkFormCompletion());
    _formPasswordErrorElement = document.getElementById("form-password-error");
    _changeButtonElement = document.getElementById("change-button");

    _updatePage();
  }

  function _checkFormCompletion() {
    let disabled = false;
    if (_formPassword1Element.value.length < _MIN_PASSWORD_LENGTH ||
        _formPassword2Element.value.length < _MIN_PASSWORD_LENGTH) {
      disabled = true;
    } else if (_formPassword1Element.value != _formPassword2Element.value) {
      _formPasswordErrorElement.innerText = "Passwords do not match";
      _formPasswordErrorElement.style.display = "block";
      disabled = true;
    } else {
      _formPasswordErrorElement.style.display = "none";
    }
    _changeButtonElement.disabled = disabled;
  }

  function _updatePage() {
    _titleUsernameElement.textContent = _username;
    document.body.hidden = false;
    _formPassword1Element.focus();
  }

  function changeNow(event) {
    Bespoke.ignoreEvent(event);
    _formPasswordErrorElement.style.display = "none";
    _updateServer();
  }

  async function _updateServer() {
    try {
      // REST: Change password
      const result = await _generatePasswordHash(_formPassword1Element.value);
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
          _formPasswordErrorElement.innerText = "Password could not be changed";
          _formPasswordErrorElement.style.display = "block";
        } else {
          console.error(`Server error: ${response.status}`);
        }
        _formPassword1Element.focus();
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
    _formPassword1Element.value = password;
    _formPassword2Element.focus();
    _checkFormCompletion();
  }

  return {
    changeNow,
    togglePassword,
    generateStrongPassword
  };
})();
