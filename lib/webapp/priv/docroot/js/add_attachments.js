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
    this._removeButton = document.getElementById("remove-button");
    //bespoke.clearLocalItem("uploadedFiles");
    this._uploadedFiles = bespoke.getLocalItem("uploadedFiles", []);
    console.log("Attachments known on load:", this._uploadedFiles);
    this._haveNoAttachments = document.getElementById("have-no-attachments");
    this._haveAttachments = document.getElementById("have-attachments");
    this._itemFilename = document.getElementById("itemFilename");
    this._itemCounter = document.getElementById("itemCounter");
    this._attachments = document.getElementById("attachments");
    this._progressBar = document.getElementById("progress-bar");
    this._progressCounter = document.getElementById("progress-counter");
    this._updatePage();
    this._addEventListeners();
    this._initializeUpload(this);
  }
  
  _updatePage() {
    const username = bespoke.getCookieValue("username");
    document.getElementById("title-username").textContent = username;
    this._renderSlideshow();
    document.body.hidden = false;
  }

  _renderSlideshow() {
    if (this._uploadedFiles.length === 0) {
      this._removeButton.disabled = true;
      this._haveNoAttachments.hidden = false;
      this._haveAttachments.hidden = true;
    } else {
      this._removeButton.disabled = false;
      this._haveNoAttachments.hidden = true;
      this._haveAttachments.hidden = false;
      console.log("Attachments known on render:", this._uploadedFiles);
      const items = [];
      for (let i = 0; i < this._uploadedFiles.length; i++) {
        const file = this._uploadedFiles[i];
        console.log("Rendering file:", file.filename);
        let item;
        if (file.contentType.startsWith("image/")) {
          item = html`
            <div class="uk-flex uk-flex-center uk-flex-middle" data-index="${i}"
                 uk-lightbox>
              <a href="${file.absPath}" data-caption="${file.filename}">
                <img src="${file.absPath}" alt="${file.filename}">
              </a>
            </div>`;
        } else if (file.contentType.startsWith("video/")) {
          item = html`
            <div class="uk-flex uk-flex-center uk-flex-middle" data-index="${i}"
                 uk-lightbox>
              <a href="${file.absPath}" data-caption="${file.filename}">
                <video controls autoplay loop muted playsinline>
                  <source src="${file.absPath}" type="${file.contentType}">
                  Your browser does not support the video tag.
                </video>
              </a>
            </div>`;
        } else {
          item = html`
            <div class="uk-flex uk-flex-center uk-flex-middle" data-index="${i}">
              <span uk-icon="icon: file-text; ratio: 4"></span>
            </div>`;
        }
        items.push(item);
      }
      render(this._attachments, html`${items}`);
      UIkit.slideshow(".uk-slideshow", {});
    }
  }
  
  _addEventListeners() {
    this._attachments.addEventListener("beforeitemhide", (e) => {
      this._itemFilename.hidden = true;
      this._itemCounter.hidden = true;
    });
    
    this._attachments.addEventListener("itemshown", (e) => {
      this._shownUploadedFileIndex =
        parseInt(e.srcElement.attributes["data-index"].value);
      this._itemFilename.firstChild.innerText =
        `${this._uploadedFiles[this._shownUploadedFileIndex].filename}`;
      this._itemFilename.hidden = false;
      this._itemCounter.firstChild.innerText =
        `${this._shownUploadedFileIndex + 1} / ${this._uploadedFiles.length}`;
      this._itemCounter.hidden = false
    });
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
      complete: (response) => {
        if (response.status === 200) {
          console.log("complete:", response);
          const newFile = JSON.parse(response.responseText);
          this._uploadedFiles =
            this._uploadedFiles.filter(
              (file) => file.filename !== newFile.filename);
          this._uploadedFiles.unshift(newFile);
          bespoke.setLocalItem("uploadedFiles", this._uploadedFiles);
          console.log("Attachments known after upload:", this._uploadedFiles);
          this._renderSlideshow();
        } else {
          console.error("complete:", response);
        }
      },
      completeAll: () => {
        console.log("completeAll");
        setTimeout(() => {
          this._hideProgressBar();
          console.log("files:", this._uploadedFiles);
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

  removeAttachment(event) {
    event.preventDefault();
    this._uploadedFiles.splice(this._shownUploadedFileIndex, 1);
    bespoke.setLocalItem("uploadedFiles", this._uploadedFiles);
    this._renderSlideshow();
  }
}

const addAttachments = new AddAttachments();
export default addAttachments
