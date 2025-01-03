import bespoke from "/js/bespoke.js";

// uhtml.min.js must be imported in the HTML file before this script
const { html, render } = uhtml;

class AddAttachments {
  constructor() {
    bespoke.onReady("add_attachments.html", () => this._load());
  }

  _load() {
    if (!bespoke.hasSessionId()) {
      bespoke.navigateTo("loader.html");
      return;
    }
    //this._filenames = bespoke.getRawLocalItem("filenames", []);
    this._filenames = [];
    console.log("Attachments known on load:", this._filenames);
    this._haveNoAttachments = document.getElementById("have-no-attachments");
    this._haveAttachments = document.getElementById("have-attachments");
    this._attachments = document.getElementById("attachments");
    this._progressBar = document.getElementById("progress-bar");
    this._progressCounter = document.getElementById("progress-counter");
    this._updatePage();
    this._initializeUpload(this);
  }

  _updatePage() {
    const username = bespoke.getCookieValue("username");
    document.getElementById("title-username").textContent = username;
    this._renderSlideshow();
    document.body.hidden = false;
  }

  _renderSlideshow() {
    if (this._filenames.length === 0) {
      this._haveNoAttachments.hidden = false;
      this._haveAttachments.hidden = true;
    } else {
      this._haveNoAttachments.hidden = true;
      this._haveAttachments.hidden = false;
      const items = [];
      for (const file of this._filenames) {
        const item = html`
          <div class="uk-flex uk-flex-center uk-flex-middle"
                                 uk-lightbox>
            <a href="${file.absPath}" data-caption="${file.filename}">
              <img src="${file.absPath}" alt="${file.filename}">
            </a>
          </div>`;
        items.push(item);
      }
      render(this._attachments, html`${items}`);
      UIkit.slideshow(".uk-slideshow", {});
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
              console.log("beforeSend:", value.name);
            }
          }
        }
      },
      abort: (e) => {
        this._removeProgressBar();
        console.error("abort:", e);
      },
      beforeAll: (files) => {
        this._filenames = [];
      },
      complete: (response) => {
        if (response.status === 200) {
          console.log("complete:", response);
          const newFile = JSON.parse(response.responseText);
          this._filenames =
            this._filenames.filter(
              (file) => file.filename !== newFile.filename);
          this._filenames.push(newFile);
          bespoke.setRawLocalItem("filenames", this._filenames);
          console.log("Attachments known after upload:", this._filenames);
          this._renderSlideshow();
        } else {
          console.error("complete:", response);
        }
      },
      completeAll: () => {
        console.log("completeAll");
        setTimeout(() => {
          this._hideProgressBar();
          console.log("files:", this._filenames);
        }, 1000);
      },
      error: (e) => {
        console.error("error:", e);
        this._hideProgressBar();
      },
      loadStart: (progress) => {
        console.log("loadStart");
        this._showProgressBar();
        this._updateProgressBar(progress.loaded, progress.total);
      },
      progress: (progress) => {
        console.log("progress");
        this._updateProgressBar(progress.loaded, progress.total);
      },
      fail: (e) => {
        console.error("fail:", e);
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

const addAttachments = new AddAttachments();
export default addAttachments
