// -*- fill-column: 100 -*-

"use strict";

const AddAttachments = (function () {
  const { html, render } = uhtml;
  let _titleUsername;
  let _haveNoAttachments;
  let _haveAttachments;
  let _itemFilename;
  let _itemCounter;
  let _attachments;
  let _removeButton;
  let _progressBar;
  let _progressCounter;
  let _uploadedFiles;
  let _shownUploadedFileIndex;

  _resetUploadedFiles();

  function init() {
    Bespoke.onReady("add_attachments.html", () => _load());
  }

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }
    console.log("Attachments known on load:", _uploadedFiles);
    _titleUsername = document.getElementById("title-username");
    _haveNoAttachments = document.getElementById("have-no-attachments");
    _haveAttachments = document.getElementById("have-attachments");
    _itemFilename = document.getElementById("item-filename");
    _itemCounter = document.getElementById("item-counter");
    _attachments = document.getElementById("attachments");
    _removeButton = document.getElementById("remove-button");
    _progressBar = document.getElementById("progress-bar");
    _progressCounter = document.getElementById("progress-counter");
    _updatePage();
    _addEventListeners();
    _initUpload();
  }

  function _updatePage() {
    const username = Bespoke.getCookieValue("username");
    _titleUsername.textContent = username;
    _renderSlideshow();
    document.body.hidden = false;
  }

  function _renderSlideshow() {
    if (_uploadedFiles.length === 0) {
      _removeButton.disabled = true;
      _haveNoAttachments.hidden = false;
      _haveAttachments.hidden = true;
    } else {
      _removeButton.disabled = false;
      _haveNoAttachments.hidden = true;
      _haveAttachments.hidden = false;
      console.log("Attachments known on render:", _uploadedFiles);
      const items = [];
      for (let i = 0; i < _uploadedFiles.length; i++) {
        const file = _uploadedFiles[i];
        console.log("Rendering file:", file.filename);
        let item;
        if (file.contentType.startsWith("image/")) {
          item = html`
            <div class="uk-flex uk-flex-center uk-flex-middle" data-index="${i}" uk-lightbox>
              <a href="${file.absPath}" data-caption="${file.filename}">
                <img src="${file.absPath}" alt="${file.filename}">
              </a>
            </div>`;
        } else if (file.contentType.startsWith("video/")) {
          item = html`
            <div class="uk-flex uk-flex-center uk-flex-middle" data-index="${i}" uk-lightbox>
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
              <span class="uk-text-muted" uk-icon="icon: file-text; ratio: 4"></span>
            </div>`;
        }
        items.push(item);
      }
      render(_attachments, html`${items}`);
      UIkit.slideshow(".uk-slideshow", {});
    }
  }

  function _addEventListeners() {
    _attachments.addEventListener("beforeitemhide", _e => {
      _itemFilename.hidden = true;
      _itemCounter.hidden = true;
    });
    _attachments.addEventListener("itemshown", e => {
      _shownUploadedFileIndex =
        parseInt(e.srcElement.attributes["data-index"].value);
      _itemFilename.firstChild.innerText = `${_uploadedFiles[_shownUploadedFileIndex].filename}`;
      _itemFilename.hidden = false;
      _itemCounter.firstChild.innerText =
        `${_shownUploadedFileIndex + 1} (${_uploadedFiles.length})`;
      _itemCounter.hidden = false
    });
  }

  function _initUpload() {
    UIkit.upload(".js-upload", {
      url: "/api/upload_attachments",
      multiple: true,
      name: "filename",
      beforeSend: env => {
        if (env.data instanceof FormData) {
          for (let [key, value] of env.data.entries()) {
            if (key == "filename" && value instanceof File) {
              console.log("beforeSend:", value.name);
            }
          }
        }
      },
      abort: e => {
        _removeProgressBar();
        console.error("abort:", e);
      },
      complete: response => {
        if (response.status === 200) {
          console.log("complete:", response);
          const newFile = JSON.parse(response.responseText);
          _uploadedFiles = _uploadedFiles.filter(file => file.filename !== newFile.filename);
          _uploadedFiles.unshift(newFile);
          _storeUploadedFiles();
          console.log("Attachments known after upload:", _uploadedFiles);
          _renderSlideshow();
        } else {
          console.error("complete:", response);
        }
      },
      completeAll: () => {
        console.log("completeAll");
        setTimeout(() => {
          _hideProgressBar();
          console.log("files:", _uploadedFiles);
        }, 1000);
      },
      error: e => {
        console.error("error:", e);
        _hideProgressBar();
      },
      loadStart: progress => {
        console.log("loadStart");
        _showProgressBar();
        _updateProgressBar(progress.loaded, progress.total);
      },
      progress: progress => {
        console.log("progress");
        _updateProgressBar(progress.loaded, progress.total);
      },
      fail: e => {
        console.error("fail:", e);
        _removeProgressBar();
      }
    });
  }

  function _showProgressBar() {
    _progressCounter.value = "0%";
    Bespoke.showLoadingSpinner();
  }

  function _removeProgressBar() {
    Bespoke.hideLoadingSpinner();
    _progressCounter.innerText = "0%";
  }

  function _updateProgressBar(loaded, total) {
    _progressBar.max = total;
    _progressBar.value = loaded;
    if (total === 0) {
      _progressCounter.innerText = "0%";
    } else {
      const percentage = Math.round((loaded / total) * 100);
      _progressCounter.innerText = `${percentage}%`;
    }
  }

  function _hideProgressBar() {
    Bespoke.hideLoadingSpinner();
    _progressCounter.innerText = "0%";
  }

  function removeAttachment(event) {
    Bespoke.ignoreEvent(event);
    _uploadedFiles.splice(_shownUploadedFileIndex, 1);
    _setUploadedFiles(_uploadedFiles);
    _renderSlideshow();
  }

  function clearUploadedFiles() {
    _uploadedFiles = [];
    _storeUploadedFiles();
  }

  function _setUploadedFiles(uploadedFiles) {
    _uploadedFiles = uploadedFiles;
    _storeUploadedFiles();
  }

  function getUploadedFiles() {
    return _uploadedFiles;
  }

  function _storeUploadedFiles() {
    Bespoke.setLocalItem("uploadedFiles", _uploadedFiles);
  }

  function _resetUploadedFiles() {
    _uploadedFiles = Bespoke.getLocalItem("uploadedFiles", []);
    _storeUploadedFiles();
  }

  function numberOfUploadedFiles() {
    return _uploadedFiles.length;
  }

  return {
    init,
    removeAttachment,
    clearUploadedFiles,
    getUploadedFiles,
    numberOfUploadedFiles
  };
})();

AddAttachments.init();
