import bespoke from "/js/bespoke.js";

class Loader {
  constructor() {
    bespoke.onReady("loader.html", () => this._load());
  }

  _load() {
    const redirect = async () => {
      try {
        if (!bespoke.captivePortalAck()) {
          console.error("Captive portal not acknowledged!");
          return;
        }
        // REST: Auto login
        const response = await fetch("/auto_login");
        if (response.redirected) {
          bespoke.navigateTo(response.url);
          return;
        } else if (!response.ok) {
          console.error(`Login failed: ${response.status}`);
          return;
        }
        const result = await response.json();
        bespoke.setCookieValue("userId", result.userId);
        bespoke.setCookieValue("username", result.username);
        bespoke.setCookieValue("sessionId", result.sessionId);
        if (result.noPassword) {
          bespoke.navigateTo("index.html");
        } else {
          bespoke.navigateTo("login.html");
        }
      } catch (error) {
        console.error("Load failed:", error);
      }
    };
    redirect();
  }
}

const loader = new Loader();
export default loader
