// -*- fill-column: 100 -*-

"use strict";

const Loader = (function () {
  if (window.location.hostname !== "localhost" &&
      window.location.hostname !== "b3s.f0ff" &&
      !_isIPv4Address(window.location.hostname)) {
    const targetUrl = `http://b3s.f0ff${window.location.pathname}`;
    window.location.href = targetUrl;
    return;
  }

  function init() {
    Bespoke.onReady("loader.html", () => _load());
  }

  function _isIPv4Address(string) {
    const IPv4Regex =
          /^(25[0-5]|(2[0-4]|1\d|[1-9])?\d)\.(25[0-5]|(2[0-4]|1\d|[1-9])?\d)\.(25[0-5]|(2[0-4]|1\d|[1-9])?\d)\.(25[0-5]|(2[0-4]|1\d|[1-9])?\d)$/;
    return IPv4Regex.test(string);
  }

  function _load() {
    const redirect = async () => {
      try {
        // REST: Auto login
        const response = await fetch("/api/auto_login");
        if (response.redirected) {
          Bespoke.navigateTo(response.url);
          return;
        } else if (!response.ok) {
          console.error(`Login failed: ${response.status}`);
          return;
        }
        const result = await response.json();
        Bespoke.setCookieValue("userId", result.userId);
        Bespoke.setCookieValue("username", result.username);
        Bespoke.setCookieValue("sessionId", result.sessionId);
        // Redirect to next page
        let absPath;
        if (result.noPassword) {
          absPath = "/index.html";
        } else {
          absPath = "/login.html";
        }
        const isAndroid = navigator.userAgentData
              ? navigator.userAgentData.platform.toLowerCase().includes("android")
              : /Android/i.test(navigator.userAgent);
        if (isAndroid) {
          const scheme = window.location.protocol.slice(0, -1);
          const hostname = window.location.hostname;
          const url = `intent://${hostname}${absPath}#Intent;scheme=${scheme};end`;
          Bespoke.navigateTo(url);
        } else {
          Bespoke.navigateTo(absPath);
        }
      } catch (error) {
        console.error("Auto login failed:", error);
      }
    };
    redirect();
  }

  return {
    init
  };
})();

Loader.init();
