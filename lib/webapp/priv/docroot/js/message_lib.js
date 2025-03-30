// -*- fill-column: 100 -*-

"use strict";

const MessageLib = (function () {
  const {html} = uhtml;

  async function readMessageAndBody(messageId, userId) {
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

    return {ok: true, message, title: readBodyResult.title,
            body: readBodyResult.body};
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
    // Fetch message body
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

    // Parse message body
    const parsedBodyBlob =
          await _parseBodyBlob(await bodyResponse.blob(), userId);

    return {ok: true, ...parsedBodyBlob};
  }

  async function _parseBodyBlob(blob, userId) {
    const unblobifiedBlob = await Crypto.unblobifyData(userId, blob);
    const buffer = await unblobifiedBlob.arrayBuffer();
    const view = new DataView(buffer);
    const titleSize = view.getUint16(0, false);
    const decoder = new TextDecoder();
    return {title: decoder.decode(new Uint8Array(buffer, 2, titleSize)),
            body: decoder.decode(new Uint8Array(buffer, 2 + titleSize))}
  }

  async function generateAttachments(userId, message) {
    let attachments = "";
    if (message.attachmentIds.length > 0) {
      const items = [];
      for (const attachmentId of message.attachmentIds) {
        // Fetch attachment metadata
        const metadataResponse =
              await fetch(`/message/${message.id}/${userId}-${attachmentId}.dat`);
        if (!metadataResponse.ok) {
          if (metadataResponse.status === 401) {
            Bespoke.navigateTo("loader.html");
            return;
          } else {
            console.error(`Server error: ${metadataResponse.status}`);
            return;
          }
        }
        const metadataBlob = await metadataResponse.blob();
        const unblobifiedMetadata = await Crypto.unblobifyData(userId, metadataBlob);
        const metadata =
              JSON.parse(new TextDecoder().decode(await unblobifiedMetadata.arrayBuffer()));

        // Fetch attachment
        let objectUrl = null;
        const attachmentAbsPath = `/message/${message.id}/${userId}-${attachmentId}`;
        if (metadata.contentType.startsWith("image/")) {
          const attachmentResponse = await fetch(attachmentAbsPath);
          if (!attachmentResponse.ok) {
            if (attachmentResponse.status === 401) {
              Bespoke.navigateTo("loader.html");
              return;
            } else {
              console.error(`Server error: ${attachmentResponse.status}`);
              return;
            }
          }
          const attachmentBlob = await attachmentResponse.blob();
          const unblobifiedAttachment = await Crypto.unblobifyData(userId, attachmentBlob);
          objectUrl = URL.createObjectURL(unblobifiedAttachment);
        }

        // Layout attachment
        if (objectUrl != null) {
          const item = html`<div class="attachment-item long-click" uk-lightbox>
            <a href="${objectUrl}" data-caption="${metadata.filename}" data-type="image"
               uk-tooltip="${metadata.filename}">
              <img class="attachment-content" src="${objectUrl}" alt="${metadata.filename}">
            </a>
          </div>`;
          items.push(item);
        } else {
          const aHref = `javascript:MessageLib.downloadFile(event, "${attachmentAbsPath}",
                                                            "${metadata.filename}")`;
          const item = html`<div class="attachment-item long-click">
            <a href="${aHref}" data-caption="${metadata.filename}"
               uk-tooltip="${metadata.filename}">
              <img class="attachment-content" src="/images/1x1.png" alt="${metadata.filename}">
              <div class="uk-position-center uk-text-break uk-text-muted
                          attachment-misc">${Bespoke.truncateMiddle(metadata.filename, 12)}</div>
            </a>
          </div>`;
          items.push(item);
        }
      }

      attachments = html`<div id="attachments" class="uk-position-relative" uk-slider>
        <div class="uk-slider-items">
          ${items}
        </div>
        <a class="uk-slidenav uk-position-small uk-position-center-left uk-overlay
                  uk-overlay-default" uk-slidenav-previous uk-slider-item="previous"></a>
        <a class="uk-slidenav uk-position-small uk-position-center-right uk-overlay
                  uk-overlay-default" uk-slidenav-next uk-slider-item="next"></a>
      </div>`;
    }

    return attachments;
  }

  async function downloadFile(event, absPath, filename) {
    Bespoke.ignoreEvent(event);

    // Prompt user to choose where to save file.
    const fileHandle = await window.showSaveFilePicker({suggestedName: filename});

    // Create a writable stream on chosen file.
    const writableStream = await fileHandle.createWritable();

    // Fetch encrypted data as a ReadableStream.
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

    // Pipe encrypted stream -> decrypt stream -> writable file stream.
    await response.body.pipeThrough(decryptStream).pipeTo(writableStream);

    console.log(`${absPath} downloaded and unblobified`);
  }

  return {
    readMessageAndBody,
    readMessage,
    readBody,
    generateAttachments,
    downloadFile
  };
})();
