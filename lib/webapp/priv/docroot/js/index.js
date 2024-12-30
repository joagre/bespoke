import bespoke from "/js/bespoke.js";

// Ensure uhtml.min.js is imported in the HTML file before this script
const { html, render } = uhtml;

class Index {
  constructor() {
    bespoke.onReady("index.html", () => this._load());
  }

  _load() {
    if (!bespoke.hasSessionId()) {
      bespoke.navigateTo("loader.html");
      return;
    }
    this._updatePage();
  }

  _updatePage() {
    const username = bespoke.getCookieValue("username");
    document.getElementById("title-username").textContent = username;
    document.body.hidden = false;
  }
}

const index = new Index();
export default index
