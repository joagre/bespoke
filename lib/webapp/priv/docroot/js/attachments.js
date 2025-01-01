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
      beforeSend: function (environment) {
        console.log("beforeSend", arguments);
        // The environment object can still be modified here.
        // var {data, method, headers, xhr, responseType} = environment;
      },
      abort: function() {
        console.log("abort", arguments);
      },
      beforeAll: function () {
        console.log("beforeAll", arguments);
      },
      beforeSend: function () {
        console.log("beforeSend", arguments);
      },
      complete: function () {
        console.log("complete", arguments);
      },
      completeAll: function () {
        console.log("completeAll", arguments);
        setTimeout(function () {
          self._progressBar.setAttribute("hidden", "hidden");
        }, 1000);
        alert("Upload Completed");
      },
      error: function () {
        console.log("error", arguments);
      },
      load: function () {
        console.log("load", arguments);
      },
      loadEnd: function (e) {
        console.log("loadEnd", arguments);
        self._progressBar.max = e.total;
        self._progressBar.value = e.loaded;
      },
      loadStart: function (e) {
        console.log("loadStart", arguments);
        self._progressBar.removeAttribute("hidden");
        self._progressBar.max = e.total;
        self._progressBar.value = e.loaded;
      },
      progress: function (e) {
        console.log("progress", arguments);
        self._progressBar.max = e.total;
        self._progressBar.value = e.loaded;
        console.error("progress done");
      },
      fail: function() {
        console.log("fail", arguments);
      }
    });
  }
}

const attachments = new Attachments();
export default attachments
