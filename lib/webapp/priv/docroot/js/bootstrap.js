// -*- fill-column: 100 -*-

"use strict";

const Bootstrap = (function () {
  let _formSSIDElement;
  let _formSSIDErrorElement;
  let _bootstrapButtonElement;

  Bespoke.onReady("bootstrap.html", () => _load());

  function _load() {
    _formSSIDElement = document.getElementById("form-ssid");
    _formSSIDElement.addEventListener("input", () => _checkFormCompletion());
    _formSSIDErrorElement = document.getElementById("form-ssid-error");
    _bootstrapButtonElement = document.getElementById("bootstrap-button");
    _updatePage();
    _checkFormCompletion();
  }

  function _checkFormCompletion() {
    if (!_isValidSSID(_formSSIDElement.value)) {
      _formSSIDErrorElement.innerText = "1–32 chars: letters, numbers, _ or -";
      _formSSIDErrorElement.style.display = "block";
      _bootstrapButtonElement.disabled = true;
    } else {
      _formSSIDErrorElement.style.display = "none";
      _bootstrapButtonElement.disabled = false;
    }
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
      // REST: Bootstrap
      const payload = {
        ssid: _formSSIDElement.value
      };
      const response = await fetch("/api/bootstrap", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(payload)
      });
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        _formSSIDErrorElement.innerText = "Internal error";
        _formSSIDErrorElement.style.display = "block";
        _formSSIDElement.focus();
        return;
      }
      Bespoke.navigateTo("loader.html");
    } catch (error) {
      console.error("Bootstrap failed", error);
    }
  }

  return {
    bootstrapNow
  };
})();
