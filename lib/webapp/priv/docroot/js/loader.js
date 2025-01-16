import bespoke from "/js/bespoke.js";

class Loader {
  constructor() {
    if (window.location.hostname !== "localhost" &&
        window.location.hostname !== "b3s.f0ff" &&
        !this._isIPv4Address(window.location.hostname)) {
      const targetUrl = `http://b3s.f0ff${window.location.pathname}`;
      window.location.href = targetUrl;
      return;
    }
    bespoke.onReady(true, () => this._load());
  }

  _isIPv4Address(string) {
    const IPv4Regex =
          /^(25[0-5]|(2[0-4]|1\d|[1-9])?\d)\.(25[0-5]|(2[0-4]|1\d|[1-9])?\d)\.(25[0-5]|(2[0-4]|1\d|[1-9])?\d)\.(25[0-5]|(2[0-4]|1\d|[1-9])?\d)$/;
    return IPv4Regex.test(string);
  }

  _load() {
    const redirect = async () => {
      try {
        // REST: Auto login
        const response = await fetch("/api/auto_login");
        if (response.redirected) {
          bespoke.navigateTo(response.url);
          return;
        } else if (!response.ok) {
          console.error(`Login failed: ${response.status}`);
          return;
        }
        this._result = await response.json();
        bespoke.setCookieValue("userId", this._result.userId);
        bespoke.setCookieValue("username", this._result.username);
        bespoke.setCookieValue("sessionId", this._result.sessionId);
        // Redirect to next page
        let absPath;
        if (this._result.noPassword) {
          absPath = "/index.html";
        } else {
          absPath = "/login.html";
        }
        const isAndroid = /Android/i.test(navigator.userAgent);
        if (isAndroid) {
          const intentUrl = `intent://${window.location.hostname}${absPath}#Intent;scheme=http;end`;
          bespoke.navigateTo(intentUrl);
        } else {
          bespoke.navigateTo(absPath);
        }
      } catch (error) {
        console.error("Auto login failed:", error);
      }
    };
    redirect();
  }
}

const loader = new Loader();
export default loader
