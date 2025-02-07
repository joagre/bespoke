// -*- fill-column: 100 -*-

"use strict";

const Files = (function () {
  const { html, render } = uhtml;
  let _isUpdatingPage = false;
  let _refreshTimer = null;
  let _stickyHeader;
  let _mainContent;
  let _haveNoFiles;
  let _haveFiles;
  let _fileUpload;
  const _REFRESH_INTERVAL = 30000;

  function init() {
    Bespoke.onReady("files.html", () => _load());
  }

  function destroy() {
    if (_refreshTimer != null) {
      clearInterval(_refreshTimer);
    }
  }

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
    setTimeout(() => {
      // The timeout is needed to ensure the header has been rendered :-9
      const headerHeight = _stickyHeader.offsetHeight;
      _mainContent.style.paddingTop = headerHeight + "px";
    }, 100);
    document.body.hidden = false;
    _initFileUpload();
    _updatePage();
  }

  function _initFileUpload() {
    _fileUpload.addEventListener("change", async function (e) {
      try {
        const file = e.target.files[0];
        if (!file) {
          return;
        }
        console.log("file:", file);
        const payload = {
          filename: file.name,
          size: file.size,
          contentType: file.type,
          isUploading: true
        };
        // REST: Add file
        const response = await fetch("/api/insert_file", {
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
        const insertedFile = await response.json();
        _updatePage();
        // Upload file
        const progressBar = document.getElementById(`progress-${insertedFile.id}`);
        function onProgress(_total, loaded, _percentComplete) {
          const formattedBytes = Bespoke.formatBytes(loaded);
          console.log(`Upload progress: ${formattedBytes}`);
          progressBar.textContent = `${formattedBytes}`;
        }
        const extraHeaders = [{key: "X-Rester-Multi-Part-Prefix",
                               value: `file-${insertedFile.id}-`}];
        const uploadedFile = await Bespoke.uploadFile(file, onProgress, extraHeaders);
        console.log(uploadedFile);
        // REST: File is uploaded
        const fileUploadedResponse = await fetch("/api/file_uploaded", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify(insertedFile.id),
        });
        if (!fileUploadedResponse.ok) {
          if (fileUploadedResponse.status === 401) {
            Bespoke.navigateTo("loader.html");
          } else {
            console.error(`Server error: ${response.status}`);
          }
          return;
        }
        _updatePage();
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
      // REST: Get files
      const response = await fetch("/api/list_files");
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

  function _createFileTemplate(insertedFile) {
    let deleteButton = "";
    if (insertedFile.uploader === Bespoke.getCookieValue("username")) {
      deleteButton =
        html`<button onclick=${event => _deleteFile(event)}
                     class="toolbar-button" uk-icon="trash"
                     uk-tooltip="Remove file"></button>`
    }
    if (insertedFile.isUploading) {
      const progressAttr = `progress-${insertedFile.id}`;
      return html`
        <div data-file-id="${insertedFile.id}"
             data-filename="${insertedFile.filename}">
          <div>
            <a class="uk-link-text uk-text-bold disabled-link" download>${insertedFile.filename}</a>
            <span class="uk-text-meta">(${insertedFile.contentType})</span>
          </div>
          <div class="uk-flex uk-flex-between uk-flex-middle">
            <div class="file-meta-data uk-text-meta">
              <span class="file-spinner" uk-spinner></span>
              ${insertedFile.uploader} •
              ${Bespoke.formatSecondsSinceEpoch(insertedFile.created)} •
              ${Bespoke.formatBytes(insertedFile.size)} / <span id="${progressAttr}">0B</span>
            </div>
            <div>
              ${deleteButton}
            </div>
          </div>
          <hr class="uk-margin-small file-divider">
        </div>`;
    } else {
      const hrefAttr = `/file/${insertedFile.id}-${insertedFile.filename}`;
      return html`
        <div data-file-id="${insertedFile.id}"
             data-filename="${insertedFile.filename}">
          <div>
            <a href="${hrefAttr}"
               class="uk-link-text uk-text-bold">${insertedFile.filename}</a>
            <button onclick=${event => _copyToClipboard(event, "${hrefAttr}")}
                    class="toolbar-button" uk-icon="icon: copy"
                    uk-tooltip="Copy link"></button>
            <span class="uk-text-meta">(${insertedFile.contentType})</span>
          </div>
          <div class="uk-flex uk-flex-between uk-flex-middle">
            <div class="file-meta-data uk-text-meta">
              ${insertedFile.uploader} •
              ${Bespoke.formatSecondsSinceEpoch(insertedFile.created)} •
              ${Bespoke.formatBytes(insertedFile.size)}
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

  function toggleMembershipInContacts(username) {
    Bespoke.showNotification(`Toggled membership in contacts for ${username}`);
  }

  return {
    init,
    destroy,
    toggleMembershipInContacts
  };
})();

Files.init();
