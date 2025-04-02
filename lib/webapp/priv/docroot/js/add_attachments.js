// -*- fill-column: 100 -*-

"use strict";

const AddAttachments = (function () {
  const {html, render} = uhtml;

  let _shownUploadedFileIndex;
  let _files = [];
  let _objectUrls = [];

  let _titleUsernameElement;
  let _haveNoAttachmentsElement;
  let _haveAttachmentsElement;
  let _fileUploadElement;
  let _itemFilenameElement;
  let _itemCounterElement;
  let _attachmentsElement;
  let _removeButtonElement;

  Bespoke.onReady("add_attachments.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }

    // Get header element
    _titleUsernameElement = document.getElementById("title-username");

    // Get attacments elements
    _haveNoAttachmentsElement = document.getElementById("have-no-attachments");
    _haveAttachmentsElement = document.getElementById("have-attachments");
    _fileUploadElement = document.getElementById("file-upload");
    _itemFilenameElement = document.getElementById("item-filename");
    _itemCounterElement = document.getElementById("item-counter");
    _attachmentsElement = document.getElementById("attachments");
    _removeButtonElement = document.getElementById("remove-button");

    _updatePage();
    _addEventListeners();
    _initUpload();
  }

  function _updatePage() {
    const username = Bespoke.getCookieValue("username");
    _titleUsernameElement.textContent = username;
    _renderSlideshow();
    document.body.hidden = false;
  }

  async function _renderSlideshow() {
    // Revoke previous object URLs to free memory
    _objectUrls.forEach(url => URL.revokeObjectURL(url));
    _objectUrls = [];

    _files = await FileDB.getAllFiles();
    if (_files.length === 0) {
      // No attachments
      _removeButtonElement.disabled = true;
      _haveNoAttachmentsElement.hidden = false;
      _haveAttachmentsElement.hidden = true;
    } else {
      // Have attachments
      _removeButtonElement.disabled = false;
      _haveNoAttachmentsElement.hidden = true;
      _haveAttachmentsElement.hidden = false;

      // Render attachments
      const items = [];
      for (let i = 0; i < _files.length; i++) {
        const file = _files[i];
        const blob = file.file;
        const objectUrl = URL.createObjectURL(blob);
        _objectUrls.push(objectUrl);
        let item;
        if (blob.type.startsWith("image/")) {
          item = html`
          <div class="uk-flex uk-flex-center uk-flex-middle" data-index="${i}">
            <img src="${objectUrl}" alt="${file.filename}">
          </div>`;
        } else if (blob.type.startsWith("video/")) {
          item = html`
          <div class="uk-flex uk-flex-center uk-flex-middle" data-index="${i}">
            <video controls autoplay loop muted playsinline>
              <source src="${objectUrl}" type="${blob.type}">
              Your browser does not support the video tag.
            </video>
          </div>`;
        } else {
          item = html`
          <div class="uk-flex uk-flex-center uk-flex-middle" data-index="${i}">
            <span class="uk-text-muted" uk-icon="icon: file-text; ratio: 4"></span>
          </div>`;
        }
        items.push(item);
      }
      render(_attachmentsElement, html`${items}`);
      UIkit.slideshow(".uk-slideshow", {});
    }
  }

  function _addEventListeners() {
    _attachmentsElement.addEventListener("beforeitemhide", _e => {
      _itemFilenameElement.hidden = true;
      _itemCounterElement.hidden = true;
    });
    _attachmentsElement.addEventListener("itemshown", e => {
      _shownUploadedFileIndex =
        parseInt(e.srcElement.attributes["data-index"].value);
      _itemFilenameElement.firstChild.innerText = `${_files[_shownUploadedFileIndex].filename}`;
      _itemFilenameElement.hidden = false;
      _itemCounterElement.firstChild.innerText =
        `${_shownUploadedFileIndex + 1} / ${_files.length}`;
      _itemCounterElement.hidden = false
    });
  }

  function _initUpload() {
    _fileUploadElement.addEventListener("change", async function (e) {
      try {
        const allFiles = e.target.files;
        if (allFiles == null || allFiles.length === 0) {
          return;
        }
        for (const file of allFiles) {
          FileDB.saveFile(file);
          _renderSlideshow();
        }
      } catch (error) {
        console.error("File upload failed:", error);
      }
    });
  }

  async function removeAttachment(event) {
    Bespoke.ignoreEvent(event);
    await FileDB.removeFile(_shownUploadedFileIndex);
    _renderSlideshow();
  }

  async function clearFiles() {
    return await FileDB.clearFiles();
  }

  async function getAllFiles() {
    return await FileDB.getAllFiles();
  }

  async function numberOfFiles() {
    return await FileDB.numberOfFiles();
  }

  return {
    removeAttachment,
    clearFiles,
    getAllFiles,
    numberOfFiles
  };
})();
