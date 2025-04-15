// -*- fill-column: 100 -*-

"use strict";

const Bootstrap = (function () {
  const _MIN_PASSWORD_LENGTH = 8;

  let _overlayTextElement;
  let _formSSIDElement;
  let _formSSIDErrorElement;
  let _formPassword1Element;
  let _formPassword2Element;
  let _formPasswordErrorElement;
  let _bootstrapButtonElement;

  Bespoke.onReady("bootstrap.html", () => _load());

  function _load() {
    // Get overlay text element
    _overlayTextElement = document.getElementById("overlay-text");

    // Get form elements
    _formSSIDElement = document.getElementById("form-ssid");
    _formSSIDElement.addEventListener("input", () => _checkFormCompletion());
    _formSSIDErrorElement = document.getElementById("form-ssid-error");
    _formPassword1Element = document.getElementById("form-password1");
    _formPassword1Element.addEventListener("input", () => _checkFormCompletion());
    _formPassword2Element = document.getElementById("form-password2");
    _formPassword2Element.addEventListener("input", () => _checkFormCompletion());
    _formPasswordErrorElement = document.getElementById("form-password-error");
    _bootstrapButtonElement = document.getElementById("bootstrap-button");

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
      if (!_isValidSSID(_formSSIDElement.value)) {
        _formSSIDErrorElement.innerText = "1–32 chars: letters, numbers, _ or -";
        _formSSIDErrorElement.style.display = "block";
        disabled = true;
      } else {
        _formSSIDErrorElement.style.display = "none";
      }
    }
    _bootstrapButtonElement.disabled = disabled;
  }

  function _isValidSSID(ssid) {
    if (ssid.length == 0) {
      return false;
    }
    const regex = /^[A-Za-z0-9_-]{1,32}$/;
    return regex.test(ssid);
  }

  function _updatePage() {
    _formSSIDElement.focus();
  }

  function bootstrapNow(event) {
    Bespoke.ignoreEvent(event);
    _formSSIDErrorElement.style.display = "none";
    _updateServer();
  }

  async function _updateServer() {
    try {
      // REST: Change admin password
      const result = await _generatePasswordHash(_formPassword1Element.value);
      const payload = {
        passwordSalt: Bespoke.encodeBase64(result.passwordSalt),
        passwordHash: Bespoke.encodeBase64(result.passwordHash)};
      const response = await fetch("/api/change_admin_password", {
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

      // REST: Bootstrap
      const bootstrapPayload = {
        ssid: _formSSIDElement.value
      };
      const bootstrapResponse = await fetch("/api/bootstrap", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(bootstrapPayload)
      });
      if (!bootstrapResponse.ok) {
        console.error(`Server error: ${bootstrapResponse.status}`);
        _formSSIDErrorElement.innerText = "Internal error";
        _formSSIDErrorElement.style.display = "block";
        _formSSIDElement.focus();
        return;
      }

      // Countdown if SSID is non-default
      if (_formSSIDElement.value != "BespokeBBS") {
        _overlayTextElement.innerText = "100";
        Bespoke.showLoadingSpinner();
        const interval = setInterval(() => {
          const currentText = _overlayTextElement.innerText;
          const currentNumber = parseInt(currentText);
          if (currentNumber > 0) {
            _overlayTextElement.innerText = (currentNumber - 1).toString();
          } else {
            clearInterval(interval);
            _overlayTextElement.innerText = "SSID is ready";
          }
        }, 500);
      } else {
        Bespoke.navigateTo("loader.html");
      }
    } catch (error) {
      console.error("Bootstrap failed", error);
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
    bootstrapNow,
    togglePassword,
    generateStrongPassword
  };
})();
