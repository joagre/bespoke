import bespoke from "/js/bespoke.js";

class Loader {
  constructor() {
    bespoke.onReady("/loader.html", () => this._load());
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
        // REST: Login
        const autoLoginResponse = await fetch("/auto_login");
        if (!autoLoginResponse.ok) {
          console.error(`Login failed: ${response.status}`);
          return;
        }
        const autoLoginResult = await response.json();
        // Set cookies
        bespoke.resetCookieState();
        bespoke.setCookieValue("username", autoLoginResult["username"]);
        bespoke.setCookieValue("sessionId", autoLoginResult["session-id"]);
        // Redirect
        if (autoLoginResult["no-password"]) {
          bespoke.navigateTo("/top_posts.html");
        } else {
          bespoke.navigateTo("/login.html");
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
