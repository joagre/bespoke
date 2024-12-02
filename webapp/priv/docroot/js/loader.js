import bespoke from "/js/bespoke.js";

class Loader {
  constructor() {
    bespoke.onReady("/loader.html", () => this._load());
  }

  _load() {
    const redirect = async () => {
      try {
        // REST: Acknowledge captive portal
        let response = await fetch("/captive_portal_ack",
                                   { method: "GET", mode: "no-cors" });
        if (!response.ok) {
          console.error(`Captive portal failed: ${response.status}`);
          return;
        }
        console.log("Captive portal acknowledgment sent to server");
        // REST: Login
        response = await fetch("/login");
        if (!response.ok) {
          console.error(`Login failed: ${response.status}`);
          return;
        }
        const loginResult = await response.json();
        // Set cookies
        bespoke.setCookieValue("username", loginResult["username"]);
        bespoke.setCookieValue("userId", loginResult["user-id"]);
        bespoke.setCookieValue("sessionId", loginResult["session-id"]);
        // Redirect
        if (loginResult["no-password"]) {
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
