// -*- fill-column: 100 -*-

"use strict";

const MessageLib = (function () {
  const {html, render} = uhtml;

  async function readMessageAndBlob(messageId, userId) {
    // Read message
    const readMessageResult = await readMessage(messageId);
    if (!readMessageResult.ok) {
      return {ok: false};
    }
    const message = readMessageResult.message;

    // Read message body
    const readBodyResult = await readBody(messageId, userId);
    if (!readBodyResult.ok) {
      return {ok: false};
    }

    return {ok: true, message, title: readBodyResult.title, body: readBodyResult.body};
  }

  async function readMessage(messageId) {
    // REST: Read message
    const response = await fetch("/api/read_messages", {
      method: "POST",
      headers: {
        "Content-Type": "application/json"
      },
      body: JSON.stringify([messageId])
    });
    if (!response.ok) {
      if (response.status == 401) {
        Bespoke.navigateTo("loader.html");
        return {ok: false};
      } else {
        console.error(`Server error: ${response.status}`);
        return {ok: false};
      }
    }
    const result = await response.json();
    Bespoke.assert(result.length === 1, "Expected exactly one message");
    const message = result[0];
    return {ok: true, message};
  }

  async function readBody(messageId, userId) {
    // Get message body
    const bodyResponse = await fetch(`/message/${messageId}/${userId}`);
    if (!bodyResponse.ok) {
      if (bodyResponse.status === 404) {
        return {ok: true, title: null, body: null};
      } else if (bodyResponse.status === 401) {
        Bespoke.navigateTo("loader.html");
        return {ok: false};
      } else {
        console.error(`Server error: ${bodyResponse.status}`);
        return {ok: false};
      }
    }
    const blob = await bodyResponse.blob();

    // Unblobify message body
    const unblobifiedBlob = await Crypto.unblobifyData(userId, blob);
    const buffer = await unblobifiedBlob.arrayBuffer();
    const unpackedBody = unpackBody(buffer, userId);

    return {ok: true, ...unpackedBody};
  }

  function packBody(title, body) {
    const encoder = new TextEncoder();
    const titleBytes = encoder.encode(title);
    const bodyBytes = encoder.encode(body);
    const totalLength = 2 + titleBytes.length + bodyBytes.length;
    const bodyArray = new Uint8Array(totalLength);
    const view = new DataView(bodyArray.buffer);
    view.setUint16(0, titleBytes.length, false);
    bodyArray.set(titleBytes, 2);
    bodyArray.set(bodyBytes, 2 + titleBytes.length);
    return bodyArray;
  }

  function unpackBody(buffer, userId) {
    const view = new DataView(buffer);
    const titleSize = view.getUint16(0, false);
    if (titleSize > buffer.byteLength - 2) {
      throw new Error("Corrupted message: title length out of bounds");
    }
    const decoder = new TextDecoder();
    return {title: decoder.decode(new Uint8Array(buffer, 2, titleSize)),
            body: decoder.decode(new Uint8Array(buffer, 2 + titleSize))}
  }

  async function createMessageAndBlobs(title, body, parentMessageId, topMessageId, recipients,
                                       selectedRecipientUsernames, uploadControllers) {
    // Pack message body
    const packedBody = MessageLib.packBody(title, body);

    // Upload blobified message body for each recipient
    let bodyBlobs = [];
    function onProgress(total, loaded, percentComplete) {
      Progress.update(total, loaded, percentComplete);
    }
    for (const selectedRecipientUsername of selectedRecipientUsernames) {
      try {
        // Blobify message body
        const userId = _lookupUserId(selectedRecipientUsername, recipients);
        const blobifiedBody = await Crypto.blobifyData(userId, packedBody);
        Progress.show();

        // Upload message body
        console.log(`Uploading message body for ${selectedRecipientUsername}...`);
        const uploadController = Bespoke.uploadData(blobifiedBody, "body", onProgress);
        uploadControllers.push(uploadController);
        const uploadedFile = await Bespoke.waitForUpload(uploadController);
        console.log("Message body has been uploaded:", uploadedFile);

        // Push to bodyBlobs variable (used by /api/create_message REST call)
        const filename = uploadedFile.absPath.replace("/tmp/", "");
        bodyBlobs.push({userId, filename})

        // Remove upload controller
        const index = uploadControllers.indexOf(uploadController);
        uploadControllers.splice(index, 1);
      } catch (error) {
        console.error("File upload failed:", error);
      } finally {
        Progress.hide();
      }
    }

    // Upload blobified attachments for each recipient
    const files = await AddAttachments.getAllFiles();
    let attachmentBlobs = [];
    for (const file of files) {
      // Prepare metadata
      const metadata = {
        filename: file.filename,
        contentType: file.file.type,
        size: file.file.size
      };
      console.log("Attachment metadata:", metadata);
      const metadataString = JSON.stringify(metadata);

      // Upload attachment for each recipient
      let blobs = [];
      for (const selectedRecipientUsername of selectedRecipientUsernames) {
        try {
          Progress.show();
          const userId = _lookupUserId(selectedRecipientUsername, recipients);

          // Upload attachment metadata
          const metadataBlob = await Crypto.blobifyData(userId, metadataString);
          console.log(`Uploading attachment metadata for ${selectedRecipientUsername}...`);
          const metadataFile = new File([metadataBlob], "metadata",
                                        {type: "application/octet-stream"});
          const metadataUploadController = Bespoke.uploadFile(metadataFile, onProgress);
          uploadControllers.push(metadataUploadController);
          const metadataUploadedFile = await Bespoke.waitForUpload(metadataUploadController);
          const index = uploadControllers.indexOf(metadataUploadController);
          uploadControllers.splice(index, 1);
          console.log("Attachment metadata has been uploaded:", metadataUploadedFile);

          // Upload attachment
          const attachmentBlobFile = await Crypto.blobifyFile(userId, file.file);
          const attachmentFile = new File([attachmentBlobFile], "attachment",
                                          {type: "application/octet-stream"});
          console.log(`Uploading attachment for ${selectedRecipientUsername}...`);
          const attachmentUploadController = Bespoke.uploadFile(attachmentFile, onProgress);
          uploadControllers.push(attachmentUploadController);
          const attachmentUploadedFile = await Bespoke.waitForUpload(attachmentUploadController);
          const attachmentIndex = uploadControllers.indexOf(attachmentUploadController);
          uploadControllers.splice(attachmentIndex, 1);
          console.log("Attachment has been uploaded:", attachmentUploadedFile);

          // Add to blobs variable (used by /api/create_message REST call)
          const blob = {userId: userId,
                        metadata: metadataUploadedFile.absPath.replace("/tmp/", ""),
                        filename: attachmentUploadedFile.absPath.replace("/tmp/", "")};
          blobs.push(blob);
        } catch (error) {
          console.error("Failed to retrieve files:", error);
        } finally {
          Progress.hide();
        }
      }

      attachmentBlobs.push(blobs);
    }

    // REST: Create top message
    console.log("Creating message...");
    const payload = {
      parentMessageId: parentMessageId == null ? undefined : parentMessageId,
      topMessageId: topMessageId == null ? undefined : topMessageId,
      bodyBlobs,
      attachmentBlobs
    }
    const response = await fetch("/api/create_message", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(payload),
    });
    if (!response.ok) {
      if (response.status === 401) {
        Bespoke.navigateTo("loader.html");
        return;
      } else {
        console.error(`Server error: ${response.status}`);
        return;
      }
    }

    // Redirect to top top_messages.html or message.html
    response.json()
      .then(result => {
        if (topMessageId == null) {
          console.log("Top message has been created:", result);
          Bespoke.navigateTo("top_messages.html");
        } else {
          console.log("ReplyMessage has been created:", result);
          Bespoke.navigateTo("message.html");
        }
      })
      .catch(error => {
        console.error("Failed to create message:", error);
      });
  }

  function _lookupUserId(selectedRecipientUsername, recipients) {
    for (const recipient of recipients) {
      if (recipient.username === selectedRecipientUsername) {
        return recipient.userId;
      }
    }
    return null;
  }

  function searchRecipients(event, showRecipientDropdown, hideRecipientDropdown,
                            setRecipients, addSelectedRecipient, selectedRecipientUsernames,
                            formRecipientInputElement, formRecipientListElement) {
    event.stopPropagation();

    // Extract query
    const query = event.currentTarget.value.trim().toLocaleLowerCase();
    if (query.length === 0) {
      hideRecipientDropdown();
      return;
    }

    // REST: Read recipients
    const payload = {
      ignoredUsernames: selectedRecipientUsernames,
      query: query
    };
    fetch("/api/search_recipients", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(payload),
    })
      .then(response => {
        if (!response.ok) {
          if (response.status === 401) {
            Bespoke.navigateTo("loader.html");
            return;
          } else {
            console.error(`Server error: ${response.status}`);
            return;
          }
        }
        return response.json();
      })
      .then(recipients => {
        setRecipients(recipients);
        populateRecipients(query, showRecipientDropdown, hideRecipientDropdown,
                           addSelectedRecipient, selectedRecipientUsernames,
                           formRecipientInputElement, formRecipientListElement, recipients);
      })
      .catch(error => {
        console.error("Page update failed:", error);
      });
  }

  function populateRecipients(query, showRecipientDropdown, hideRecipientDropdown,
                              addSelectedRecipient, selectedRecipientUsernames,
                              formRecipientInputElement, formRecipientListElement, recipients) {
    // If no recipients found, show warning
    if (recipients.length == selectedRecipientUsernames.length) {
      formRecipientInputElement.classList.add("uk-text-danger");
      const list = html`
        <li>
          <span class="uk-text-muted uk-margin-small-right">
            No matching recipient...
          </span>
        </li>`;
      render(formRecipientListElement, list);
      showRecipientDropdown();
      return;
    }

    // Extract usernames
    const usernames = recipients
          .map(recipient => recipient.username)
          .map(username => ({original: username, lower: username.toLocaleLowerCase()}))
          .filter(recipientObj => recipientObj.lower.includes(query.toLocaleLowerCase()))
          .map(recipientObj => recipientObj.original);
    if (usernames.length > 0) {
      formRecipientInputElement.classList.remove("uk-text-danger");
      let list = [];
      for (let i = 0; i < usernames.length; i++) {
        if (!selectedRecipientUsernames.includes(usernames[i])) {
          const substrings = usernames[i].split(new RegExp(`(${query})`, "gi"));

          // Highlight substrings
          for (let i = 0; i < substrings.length; i++) {
            if (substrings[i].toLocaleLowerCase() === query) {
              substrings[i] = html`<span class="substring">${substrings[i]}</span>`;
            } else {
              substrings[i] = html`${substrings[i]}`;
            }
          }

          const entry = html`
            <li class="recipient"
                onclick=${() => {
                            addSelectedRecipient(usernames[i]);
                            hideRecipientDropdown();
                         }}>
              <span class="uk-margin-small-right" uk-icon="user"></span>${substrings}
            </li>`;
          list.push(entry);
        }
      }
      render(formRecipientListElement, html`${list}`);
      showRecipientDropdown();
    } else {
      formRecipientInputElement.classList.add("uk-text-danger");
      hideRecipientDropdown();
    }
  }

  function refreshSelectedRecipients(checkFormCompletion, refreshSelectedRecipientsNow,
                                     selectedRecipientUsernames, username,
                                     formSelectedRecipientsElement) {
    checkFormCompletion();
    formSelectedRecipientsElement.hidden = selectedRecipientUsernames.length === 0;
    const recipients = [];
    for (const selectedRecipientUsername of selectedRecipientUsernames) {
      if (selectedRecipientUsername === username) {
        continue;
      }
      const recipientElement = html`
        <span class="uk-label no-uppercase uk-margin-small-right">
          ${selectedRecipientUsername}
          <a class="uk-margin-small-left uk-link-reset"
             onclick=${_event => {
                         refreshSelectedRecipientsNow(selectedRecipientUsername);
                      }}>&times;</a>
        </span>`;
      recipients.push(recipientElement);
    }
    render(formSelectedRecipientsElement, html`${recipients}`);
  }

  async function generateAttachments(userId, message) {
    if (message.attachmentIds == null && message.attachmentIds.length === 0) {
      return {ok: true, attachments: []};
    }

    // Fetch metadata for each attachment in parallel
    const items = await Promise.all(
      message.attachmentIds.map(async attachmentId => {
        try {
          // Fetch attachment metadata
          const metadataUrl = `/message/${message.id}/${userId}-${attachmentId}.dat`;
          const metadataResponse = await fetch(metadataUrl);
          if (!metadataResponse.ok) {
            throw new Error(`Metadata fetch failed: ${metadataResponse.status}`);
          }
          const metadataBlob = await metadataResponse.blob();
          const unblobifiedMetadata = await Crypto.unblobifyData(userId, metadataBlob);
          const metadataBuffer = await unblobifiedMetadata.arrayBuffer();
          const metadata = JSON.parse(new TextDecoder().decode(metadataBuffer));

          // Layout attachment
          const attachmentUrl = `/message/${message.id}/${userId}-${attachmentId}`;
          return html`
            <span class="attachment-item uk-text-meta"
                  onclick=${event => {
                              MessageLib.downloadFile(event, attachmentUrl, metadata.filename);
                           }}
                  uk-tooltip="${metadata.filename}">
              📎 ${metadata.filename}
            </span>`;
        } catch (err) {
          console.error("Attachment load failed:", err);
          return null;
        }
      })
    );

    // Filter out null items
    const filteredItems = items.filter(item => item !== null);

    if (filteredItems.length === 0) {
      return {ok: true, attachments: []};
    }
    const attachments = html`
      <div class="attachment-list">
        ${filteredItems}
      </div>`;
    return { ok: true, attachments };
  }

  async function downloadFile(event, absPath, filename) {
    Bespoke.ignoreEvent(event);

    // Note: showSaveFilePicker is typically available only in "new" browsers
    if ("showSaveFilePicker" in window) {
      // Prompt user to choose where to save file
      const fileHandle = await window.showSaveFilePicker({suggestedName: filename});

      // Create a writable stream on chosen file
      const writableStream = await fileHandle.createWritable();

      // Fetch encrypted data as a ReadableStream
      const response = await fetch(absPath);
      if (!response.ok || !response.body) {
        throw new Error(`Request failed with status: ${response.status}`);
      }

      // Create a decryption stream
      const decryptStream = new TransformStream({
        transform(chunk, controller) {
          Crypto.unblobifyChunk(chunk)
            .then(result => {
              controller.enqueue(new Uint8Array(result));
            })
            .catch(err => controller.error(err));
        }
      });

      // Pipe encrypted stream -> decrypt stream -> writable file stream
      await response.body.pipeThrough(decryptStream).pipeTo(writableStream);

      console.log(`${filename} downloaded and unblobified via showSaveFilePicker`);
    } else {
      // Fetch the encrypted file as a ReadableStream
      const response = await fetch(absPath);
      if (!response.ok || !response.body) {
        throw new Error(`Fetch failed with status: ${response.status}`);
      }

      // Create a decryption TransformStream
      const decryptStream = new TransformStream({
        async transform(chunk, controller) {
          try {
            // Decrypt chunk asynchronously
            const result = await Crypto.unblobifyChunk(chunk);
            controller.enqueue(new Uint8Array(result));
          } catch (err) {
            controller.error(err);
          }
        }
      });

      // Create a writable stream via StreamSaver.js
      const fileStream = streamSaver.createWriteStream(filename);

      // Pipe from fetch -> decrypt -> file
      await response.body.pipeThrough(decryptStream).pipeTo(fileStream);

      console.log(`${absPath} downloaded and unblobified via StreamSaver`);
    }
  }

  return {
    readMessageAndBlob,
    readMessage,
    readBody,
    packBody,
    unpackBody,
    createMessageAndBlobs,
    searchRecipients,
    refreshSelectedRecipients,
    generateAttachments,
    downloadFile
  };
})();
