import bespoke from "/js/bespoke.js";

class Attachments {
  constructor() {
    bespoke.onReady("attachments.html", () => this._load());
  }

  _load() {
    if (!bespoke.hasSessionId()) {
      bespoke.navigateTo("loader.html");
      return;
    }
    this._progressBar = document.getElementById("progress-bar");
    this._progressCounter = document.getElementById("progress-counter");
    this._initializeUpload(this);
    this._updatePage();
  }

  _updatePage() {
    try {
      const username = bespoke.getCookieValue("username");
      document.getElementById("title-username").textContent = username;
      document.body.hidden = false;
    } catch (error) {
      console.error("Page update failed:", error);
    }
  }

  _initializeUpload(self) {
    UIkit.upload(".js-upload", {
      url: "/upload_attachments",
      multiple: true,
      beforeSend: (environment) => {
        console.log("beforeSend", arguments);
        // The environment object can still be modified here.
        // var {data, method, headers, xhr, responseType} = environment;
      },
      abort: () => {
        console.log("abort", arguments);
      },
      beforeAll: () => {
        console.log("beforeAll", arguments);
      },
      beforeSend: () => {
        console.log("beforeSend", arguments);
      },
      complete: () => {
        console.log("complete", arguments);
      },
      completeAll: () => {
        console.log("completeAll", arguments);
        setTimeout(() => {
          this._hideProgressBar();
        }, 1000);
      },
      error: () => {
        this._hideProgressBar();
        console.log("error", arguments);
      },
      load: () => {
        console.log("load", arguments);
      },
      loadEnd: (e) => {
        console.log("loadEnd", arguments);
      },
      loadStart: (e) => {
        console.log("loadStart", arguments);
        this._showProgressBar();
        this._updateProgressBar(e.loaded, e.total);
      },
      progress: (e) => {
        console.log("progress", arguments);
        this._updateProgressBar(e.loaded, e.total);
      },
      fail: () => {
        this._removeProgressBar();
        console.log("fail", arguments);
      }
    });
  }

  _showProgressBar() {
    this._progressCounter.value = "0%";
    bespoke.showLoadingSpinner();
  }

  _updateProgressBar(loaded, total) {
    this._progressBar.max = total;
    this._progressBar.value = loaded;
    if (total === 0) {
      this._progressCounter.innerText = "0%";
    } else {
      const percentage = Math.round((loaded / total) * 100);
      console.log("percentage", (loaded / total) * 100);
      this._progressCounter.innerText = `${percentage}%`;
    }
  }

  _hideProgressBar() {
    bespoke.hideLoadingSpinner();
    this._progressCounter.innerText = "0%";
  }
}

const attachments = new Attachments();
export default attachments
