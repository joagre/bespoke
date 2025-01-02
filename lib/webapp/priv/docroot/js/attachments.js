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
    } catch (e) {
      console.error("Page update failed:", e);
    }
  }

  _initializeUpload(self) {
    UIkit.upload(".js-upload", {
      url: "/upload_attachments",
      multiple: true,
      name: "filename",
      beforeSend: (env) => {
        if (env.data instanceof FormData) {
          for (let [key, value] of env.data.entries()) {
            if (key == "filename" && value instanceof File) {
              console.log("beforeSend", value.name);
            }
          }
        }
      },
      abort: (e) => {
        this._removeProgressBar();
        console.error("abort", e);
      },
      beforeAll: (files) => {
        this._filenames = [];
      },
      complete: (response) => {
        if (response.status === 200) {
          console.log("complete", response);
          const file = JSON.parse(response.responseText);
          this._filenames.push(file);
        } else {
          console.error("complete", response);
        }
      },
      completeAll: () => {
        console.log("completeAll");
        setTimeout(() => {
          this._hideProgressBar();
          console.log("files", this._filenames);
        }, 1000);
      },
      error: (e) => {
        console.error("error", e);
        this._hideProgressBar();
      },
      loadStart: (e) => {
        console.log("loadStart");
        this._showProgressBar();
        this._updateProgressBar(e.loaded, e.total);
      },
      progress: (e) => {
        console.log("progress");
        this._updateProgressBar(e.loaded, e.total);
      },
      fail: (e) => {
        console.error("fail", e);
        this._removeProgressBar();
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
