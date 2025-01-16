import bespoke from "/js/bespoke.js";

class Loader {
  constructor() {
    if (window.location.hostname !== "localhost" &&
        window.location.hostname !== "b3s.f0ff") {
      const targetUrl = `http://b3s.f0ff${window.location.pathname}`;
      window.location.href = targetUrl;
      return;
    }
    bespoke.onReady(true, () => this._load());
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
      } catch (error) {
        console.error("Load failed:", error);
      }
    };
    redirect();
  }

  enter() {
    if (this._result.noPassword) {
      bespoke.navigateTo("/index.html");
    } else {
      bespoke.navigateTo("/login.html");
    }
  }
}

const loader = new Loader();
export default loader
