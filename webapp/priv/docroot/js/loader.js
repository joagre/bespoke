import bespoke from "/js/bespoke.js";

class Loader {
  constructor() {
    bespoke.onReady("loader.html", () => this._load());
  }

  _load() {
    const redirect = async () => {
      try {
        // REST: Acknowledge captive portal
        const response = await fetch("/captive_portal_ack",
                                     { method: "GET", mode: "no-cors" });
        if (!response.ok) {
          console.error(`Captive portal failed: ${response.status}`);
          return;
        }
        console.log("Captive portal acknowledgment sent to server");
        // REST: Auto login
        const autoLoginResponse = await fetch("/auto_login");
        if (autoLoginResponse.redirected) {
          bespoke.navigateTo(autoLoginResponse.url);
          return;
        } else if (!autoLoginResponse.ok) {
          console.error(`Login failed: ${autoLoginResponse.status}`);
          return;
        }
        const autoLoginResult = await autoLoginResponse.json();
        bespoke.setCookieValue("userId", autoLoginResult["user-id"]);
        bespoke.setCookieValue("username", autoLoginResult["username"]);
        bespoke.setCookieValue("sessionId", autoLoginResult["session-id"]);
        if (autoLoginResult["no-password"]) {
          bespoke.navigateTo("top_posts.html");
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
