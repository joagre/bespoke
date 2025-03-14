// -*- fill-column: 100 -*-

"use strict";

const Files = (function () {
  const {html, render} = uhtml;
  const _REFRESH_INTERVAL = 30000;
  let _stickyHeader;
  let _mainContent;
  let _haveNoFiles;
  let _haveFiles;
  let _fileUpload;
  let _uploadControllers = [];
  let _refreshTimer = null;
  let _isUpdatingPage = false;

  window.addEventListener("beforeunload", () => {
    // Abort still ongoing uploads
    for (const uploadController in _uploadControllers) {
      Bespoke.abortUpload(uploadController);
    }
    // Clear refresh timer
    if (_refreshTimer != null) {
      clearInterval(_refreshTimer);
    }
  });

  Bespoke.onReady("files.html", () => _load());

  function _load() {
    if (!Bespoke.hasSessionId()) {
      Bespoke.navigateTo("loader.html");
      return;
    }
    _stickyHeader = document.querySelector(".sticky-header");
    _mainContent = document.getElementById("main-content");
    _haveNoFiles = document.getElementById("have-no-files");
    _haveFiles = document.getElementById("have-files");
    _fileUpload = document.getElementById("file-upload");
    _refreshTimer = setInterval(() => _updatePage(), _REFRESH_INTERVAL);
    // Update header
    const username = Bespoke.getCookieValue("username");
    document.getElementById("title-username").textContent = username;
    // Make room for the header
    setTimeout(() => {
      // The timeout is needed to ensure the header has been rendered :-9
      const headerHeight = _stickyHeader.offsetHeight;
      _mainContent.style.paddingTop = headerHeight + "px";
    }, 100);
    document.body.hidden = false;
    _initUpload();
    _updatePage();
  }

  function _initUpload() {
    _fileUpload.addEventListener("change", async function (e) {
      try {
        const allFiles = e.target.files;
        if (allFiles == null || allFiles.length === 0) {
          return;
        }
        for (const file of allFiles) {
          console.log("Upload file:", file);
          const payload = {
            filename: file.name,
            size: file.size,
            contentType: file.type,
            isUploading: true
          };
          // REST: Create file
          const response = await fetch("/api/create_file", {
            method: "POST",
            headers: {
              "Content-Type": "application/json",
            },
            body: JSON.stringify(payload),
          });
          if (!response.ok) {
            if (response.status === 401) {
              Bespoke.navigateTo("loader.html");
            } else {
              console.error(`Server error: ${response.status}`);
            }
            return;
          }
          const createdFile = await response.json();
          await _updatePage();
          // Upload file
          const progressBar = document.getElementById(`progress-${createdFile.id}`);
          function onProgress(_total, loaded, _percentComplete) {
            const formattedBytes = Bespoke.formatBytes(loaded);
            console.log(`Upload progress: ${formattedBytes}`);
            progressBar.textContent = `${formattedBytes}`;
          }
          const extraHeaders = [{key: "X-Rester-Multi-Part-Prefix",
                                 value: `file-${createdFile.id}-`}];
          const uploadController = Bespoke.uploadFile(file, onProgress, extraHeaders);
          _uploadControllers.push(uploadController);
          const uploadedFile = await Bespoke.waitForUpload(uploadController);
          const index = _uploadControllers.indexOf(uploadController);
          _uploadControllers.splice(index, 1);
          console.log("File has been uploaded:", uploadedFile);
          // REST: File is uploaded
          const fileUploadedResponse = await fetch("/api/file_is_uploaded", {
            method: "POST",
            headers: {
              "Content-Type": "application/json",
            },
            body: JSON.stringify(createdFile.id),
          });
          if (!fileUploadedResponse.ok) {
            if (fileUploadedResponse.status === 401) {
              Bespoke.navigateTo("loader.html");
            } else {
              console.error(`Server error: ${response.status}`);
            }
            return;
          }
          await _updatePage();
        }
      } catch (error) {
        console.error("File upload failed:", error);
      }
    });
  }

  async function _updatePage() {
    if (_isUpdatingPage) {
      return;
    }
    _isUpdatingPage = true;
    try {
      // REST: List files
      const response = await fetch("/api/read_files");
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        if (response.status === 401) {
          Bespoke.navigateTo("loader.html");
        }
        return;
      }
      const files = await response.json();
      _renderFiles(files);
    } catch (error) {
      console.error("Page update failed:", error);
    } finally {
      _isUpdatingPage = false;
    }
  }

  function _renderFiles(files) {
    const filesContainer = document.getElementById("files");
    if (files.length === 0) {
      _haveNoFiles.hidden = false;
      _haveFiles.hidden = true;
      return;
    } else {
      _haveNoFiles.hidden = true;
      _haveFiles.hidden = false;
    }
    const fileTemplates = files.map(file => _createFileTemplate(file));
    render(filesContainer, html`${fileTemplates}`);
  }

  function _createFileTemplate(createdFile) {
    const truncatedFilename = createdFile.filename.substring(0, 48);
    let deleteButton = "";
    if (createdFile.uploaderId === Bespoke.getCookieValue("userId")) {
      deleteButton =
        html`<button onclick=${event => _deleteFile(event)}
                     class="toolbar-button" uk-icon="trash" uk-tooltip="Remove file"></button>`
    }
    if (createdFile.isUploading) {
      const progressAttr = `progress-${createdFile.id}`;
      return html`
        <div data-file-id="${createdFile.id}"
             data-filename="${createdFile.filename}">
          <div>
            <a class="uk-link-text uk-text-bold disabled-link" download>${truncatedFilename}</a>
            <span class="uk-text-meta">(${createdFile.contentType})</span>
          </div>
          <div class="uk-flex uk-flex-between uk-flex-middle">
            <div class="file-meta-data uk-text-meta">
              <span class="file-spinner" uk-spinner></span>
              ${createdFile.uploaderUsername} •
              ${Bespoke.formatSecondsSinceEpoch(createdFile.created)} •
              ${Bespoke.formatBytes(createdFile.size)} / <span id="${progressAttr}">0B</span>
            </div>
            <div>
              ${deleteButton}
            </div>
          </div>
          <hr class="uk-margin-small file-divider">
        </div>`;
    } else {
      const hrefAttr = `/file/${createdFile.id}-${createdFile.filename}`;
      return html`
        <div data-file-id="${createdFile.id}"
             data-filename="${createdFile.filename}">
          <div>
            <a href="${hrefAttr}"
               class="uk-link-text uk-text-bold" download>${truncatedFilename}</a>
            <button onclick=${event => _copyToClipboard(event, "${hrefAttr}")}
                    class="toolbar-button" uk-icon="icon: copy"
                    uk-tooltip="Copy link"></button>
            <span class="uk-text-meta">(${createdFile.contentType})</span>
          </div>
          <div class="uk-flex uk-flex-between uk-flex-middle">
            <div class="file-meta-data uk-text-meta">
              ${createdFile.uploaderUsername} •
              ${Bespoke.formatSecondsSinceEpoch(createdFile.created)} •
              ${Bespoke.formatBytes(createdFile.size)}
            </div>
            <div>
              ${deleteButton}
            </div>
          </div>
          <hr class="uk-margin-small file-divider">
        </div>`;
    }
  }

  async function _deleteFile(event) {
    Bespoke.ignoreEvent(event);
    const fileDiv = event.currentTarget.closest('div[data-file-id]');
    const fileId = Number(fileDiv.getAttribute("data-file-id"));
    try {
      // REST: Delete file
      const response = await fetch("/api/delete_file", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(fileId)
      });
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        if (response.status === 401) {
          Bespoke.navigateTo("loader.html");
        }
        return;
      }
      _updatePage();
    } catch (error) {
      console.error("File deletion failed:", error);
    }
  }

  function _copyToClipboard(event, link) {
    Bespoke.ignoreEvent(event);
    const tempInput = document.createElement("input");
    tempInput.value = link;
    document.body.appendChild(tempInput);
    tempInput.select();
    document.execCommand("copy");
    document.body.removeChild(tempInput);
    Bespoke.showNotification("Link copied to clipboard");
  }

  return {
  };
})();
