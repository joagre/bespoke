// -*- fill-column: 100 -*-

"use strict";

const _About = (function () {
  const _bbsName = Bespoke.getCookieValue("bbsName");

  let _headerTitleElement;
  let _aboutElement;

  Bespoke.onReady("about.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get header elements
    _headerTitleElement = document.getElementById("header-title");

    // Get about element
    _aboutElement = document.getElementById("about");
    fetch("/about.md")
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
          Bespoke.formatMarkdown(_aboutElement, text);
          document.body.hidden = false;
        }
      })
      .catch((error) => {
        console.error("Error fetching about text:", error);
      });

    _updatePage();
  }

  function _updatePage() {
    // Update header
    _headerTitleElement.textContent = _bbsName;
  }
})();
