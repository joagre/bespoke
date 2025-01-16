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
        // Check if we need to redirect


/*
        const params = new URLSearchParams(window.location.search);
        const redirect = params.get("redirect");
        if (redirect) {
          window.location.href = `/${redirect}`;
        }
*/



/*



    // Switch over from mini-browser to system browser on Android
    if (bespoke.isAndroid() && !bespoke.isMiniBrowser()) {
      setTimeout(() => {
        bespoke.captivePortalCheck();
        const intentUrl = `intent://${window.location.hostname}/index.html#Intent;scheme=http;end`;
        bespoke.navigateTo(intentUrl);
      }, 60000);
    }
*/


/*

        if (bespoke.isAndroid() && !bespoke.isMiniBrowser()) {
          let targetAbsPath;
          if (this._result.noPassword) {
            targetAbsPath = "/index.html";
          } else {
            targetAbsPath = "/login.html";
          }
          bespoke.captivePortalAck();



          const intentUrl = `intent://${window.location.hostname}/add_attachments.html#Intent;scheme=http;end`;
          window.location.href = intentUrl;


          bespoke.navigateTo(intentUrl);
        }


        maybe I can check if input type ="text" isn't available because I'm in the mini-browser and then check if I'm on Android and in that case try this:


          const intentUrl = `intent://${window.location.hostname}/add_attachments.html#Intent;scheme=http;end`;
          window.location.href = intentUrl;
          return;



*/


      } catch (error) {
        console.error("Load failed:", error);
      }
    };
    redirect();
  }

  enter() {
    let targetUrl;

    if (this._result.noPassword) {
      targetUrl = "/index.html";
    } else {
      targetUrl = "/login.html";
    }

    window.location.href = targetUrl;
    return;







    if (!bespoke.isMobile()) {
      window.location.href = targetUrl;
      return;
    }

    targetUrl = `http://b3s.f0ff${targetUrl}`;

    // Check if running on Android
    if (bespoke.isAndroid()) {
      //const intentUrl = `intent://${window.location.hostname}/loader.html?redirect=index.html#Intent;scheme=http;end`;
      const intentUrl = `intent://${targetUrl.replace(/^https?:\/\//, "")}#Intent;scheme=http;end`;
      //const intentUrl = `intent://${window.location.hostname}/index.html#Intent;scheme=http;end`;
      //const intentUrl = `intent://${window.location.hostname}/loader.html#Intent;scheme=http;end`;
      //window.location.href = intentUrl;

      //return;
    }

    // Option 1: Try to open in the system browser
    try {
      const opened = window.open(targetUrl, "_system");
      if (!opened) throw new Error("System browser failed");
      return;
    } catch (e) {
      console.warn("System browser failed:", e);
    }

    // Option 2: Fallback to opening in a new tab
    try {
      const opened = window.open(targetUrl, "_blank");
      if (!opened) throw new Error("New tab failed");
      return;
    } catch (e) {
      console.warn("New tab failed:", e);
    }

    // Option 3: Notify user with URL for manual copy
    alert(`Please open the following URL in your browser: ${targetUrl}`);
  }
}

const loader = new Loader();
export default loader
