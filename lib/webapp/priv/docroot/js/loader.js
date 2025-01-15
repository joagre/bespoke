import bespoke from "/js/bespoke.js";

class Loader {
  constructor() {
    bespoke.onReady(true, () => this._load());
    this._LOAD_DELAY = 4000;
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
        const result = await response.json();
        bespoke.setCookieValue("userId", result.userId);
        bespoke.setCookieValue("username", result.username);
        bespoke.setCookieValue("sessionId", result.sessionId);
        setTimeout(() => {
          if (!bespoke.captivePortalAck()) {
            console.error("Captive portal not acknowledged!");
            return;
          }
          if (result.noPassword) {
            bespoke.navigateTo("index.html");
          } else {
            bespoke.navigateTo("login.html");
          }
        }, this._LOAD_DELAY);
      } catch (error) {
        console.error("Load failed:", error);
      }
    };
    redirect();
  }
}

const loader = new Loader();
export default loader
