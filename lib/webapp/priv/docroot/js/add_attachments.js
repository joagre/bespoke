// -*- fill-column: 100 -*-

"use strict";

const AddAttachments = (function () {
  const {html, render} = uhtml;
  let _progressBar;
  let _progressCounter;
  let _titleUsername;
  let _haveNoAttachments;
  let _haveAttachments;
  let _fileUpload;
  let _uploadControllers = [];
  let _itemFilename;
  let _itemCounter;
  let _attachments;
  let _removeButton;
  let _uploadedFiles;
  let _shownUploadedFileIndex;

  _resetUploadedFiles();

  window.addEventListener("beforeunload", () => {
    // Abort still ongoing uploads
    for (const uploadController in _uploadControllers) {
      Bespoke.abortUpload(uploadController);
    }
  });

  Bespoke.onReady("add_attachments.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }
    console.log("Attachments known on load:", _uploadedFiles);
    _progressBar = document.getElementById("progress-bar");
    _progressCounter = document.getElementById("progress-counter");
    _titleUsername = document.getElementById("title-username");
    _haveNoAttachments = document.getElementById("have-no-attachments");
    _haveAttachments = document.getElementById("have-attachments");
    _fileUpload = document.getElementById("file-upload");
    _itemFilename = document.getElementById("item-filename");
    _itemCounter = document.getElementById("item-counter");
    _attachments = document.getElementById("attachments");
    _removeButton = document.getElementById("remove-button");
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
        `${_shownUploadedFileIndex + 1} / ${_uploadedFiles.length}`;
      _itemCounter.hidden = false
    });
  }

  function _initUpload() {
    _fileUpload.addEventListener("change", async function (e) {
      try {
        const allFiles = e.target.files;
        if (allFiles == null || allFiles.length === 0) {
          return;
        }
        for (const file of allFiles) {
          FileDB.saveFile(file);
          _showProgressBar();
          // Upload file
          function onProgress(total, loaded, percentComplete) {
            _updateProgressBar(total, loaded, percentComplete);
          }
          const uploadController = Bespoke.uploadFile(file, onProgress);
          _uploadControllers.push(uploadController);
          const uploadedFile = await Bespoke.waitForUpload(uploadController);
          const index = _uploadControllers.indexOf(uploadController);
          _uploadControllers.splice(index, 1);
          console.log("File has been uploaded:", uploadedFile);
          // Update uploaded attachments
          _uploadedFiles = _uploadedFiles.filter(file => file.filename !== uploadedFile.filename);
          _uploadedFiles.unshift(uploadedFile);
          _storeUploadedFiles();
          console.log("Attachments known after upload:", _uploadedFiles);
          _renderSlideshow();
        }
      } catch (error) {
        console.error("File upload failed:", error);
      } finally {
        await Bespoke.pause(1000);
        _hideProgressBar();
      }
    });
  }

  function _showProgressBar() {
    _progressCounter.value = "0%";
    Bespoke.showLoadingSpinner();
  }

  function _hideProgressBar() {
    Bespoke.hideLoadingSpinner();
    _progressCounter.innerText = "0%";
  }

  function _updateProgressBar(total, loaded, percentComplete) {
    _progressBar.max = total;
    _progressBar.value = loaded;
    if (total === 0) {
      _progressCounter.innerText = "0%";
    } else {
      const percentage = Math.round(percentComplete);
      _progressCounter.innerText = `${percentage}%`;
    }
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
    FileDB.clearFiles();
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
    removeAttachment,
    clearUploadedFiles,
    getUploadedFiles,
    numberOfUploadedFiles
  };
})();
