// -*- fill-column: 100 -*-

"use strict";

const Settings = (function () {
  const _username = Bespoke.getCookieValue("username");
  const _bbsName = Bespoke.getCookieValue("bbsName");

  let _uploadControllers = [];

  let _titleUsernameElement;
  let _formBBSNameElement;
  let _formAboutElement;
  let _saveButtonElement;

  Bespoke.onReady("settings.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get header elements
    _titleUsernameElement = document.getElementById("title-username");

    // Get form elements
    _formBBSNameElement = document.getElementById("form-bbs-name");
    _formBBSNameElement.addEventListener("input", () => _checkFormCompletion());
    _formAboutElement = document.getElementById("form-about");
    _formAboutElement.addEventListener("input", () => _checkFormCompletion());

    // Get savebutton element
    _saveButtonElement = document.getElementById("save-button");

    _updatePage();
    Bespoke.initMobileKeyboardResizing("#form-body");
  }

  function _checkFormCompletion() {
    _saveButtonElement.disabled = (_formBBSNameElement.value.trim() === "" ||
                                   _formAboutElement.value.trim() === "");
  }

  async function _updatePage() {
    // Update header
    _titleUsernameElement.textContent = _username;

    document.body.hidden = false;

    // Update form
    _formBBSNameElement.value = _bbsName;
    fetch("/local/about.md")
      .then((response) => {
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          if (response.status === 401) {
            Bespoke.navigateTo("loader.html");
          }
          return;
        }
        return response.text();
      })
      .then((text) => {
        if (text != null) {
          _formAboutElement.textContent = text;
          _checkFormCompletion();
          _formBBSNameElement.focus();
        }
      })
      .catch((error) => {
        console.error("Error fetching about text:", error);
      });
  }

  async function saveNow(event) {
    Bespoke.ignoreEvent(event);

    // Save settings
    const settings = {
      bbsName: _formBBSNameElement.value.trim(),
      about: _formAboutElement.value.trim()};
    const response = await fetch("/api/save_settings", {
      method: "POST",
      headers: {
        "Content-Type": "application/json"
      },
      body: JSON.stringify(settings)
    });
    if (!response.ok) {
      console.error(`Server error: ${response.status}`);
      if (response.status === 401) {
        Bespoke.navigateTo("loader.html");
      }
      return;
    }

    Bespoke.setCookieValue("bbsName", settings.bbsName);
    Bespoke.gotoPage(event, "index.html");
  }

  return {
    saveNow
  };
})();
