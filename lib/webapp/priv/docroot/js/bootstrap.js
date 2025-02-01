// -*- fill-column: 100 -*-

"use strict";

const Bootstrap = (function () {
  let _formSSID;
  let _formSSIDError;
  let _bootstrapButton;
  
  function init() {
    Bespoke.onReady("bootstrap.html", () => _load());
  }
  
  function _load() {
    _formSSID = document.getElementById("form-ssid");
    _formSSID.addEventListener("input", () => _checkFormCompletion());
    _formSSIDError = document.getElementById("form-ssid-error");
    _bootstrapButton = document.getElementById("bootstrap-button");
    _updatePage();
    _checkFormCompletion();
  }
  
  function _checkFormCompletion() {
    if (!_isValidSSID(_formSSID.value)) {
      _formSSIDError.innerText = "1–32 chars: letters, numbers, _ or -";
      _formSSIDError.style.display = "block";
      _bootstrapButton.disabled = true;
    } else {
      _formSSIDError.style.display = "none";
      _bootstrapButton.disabled = false;
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
    _formSSID.focus();
  }

  function bootstrapNow(event) {
    Bespoke.ignoreEvent(event);
    _formSSIDError.style.display = "none";
    _updateServer();
  }
  
  async function _updateServer() {
    try {
      // REST: Bootstrap
      const payload = {
        ssid: _formSSID.value
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
        _formSSIDError.innerText = "Internal error";
        _formSSIDError.style.display = "block";
        _formSSID.focus();
        return;
      }
      Bespoke.navigateTo("loader.html");
    } catch (error) {
      console.error("Bootstrap failed", error);
    }
  }

  return {
    init,
    bootstrapNow
  };
})();

Bootstrap.init();
