// -*- fill-column: 100 -*-

"use strict";

const Crypto = (function () {
  let _worker;

  document.addEventListener("DOMContentLoaded", () => {
    _worker = new Worker("/js/crypto_worker.js");
  });

  async function generatePasswordHash(password) {
    try {
      const result = await _postMessage("generatePasswordHash", password);
      return {passwordSalt: result.passwordSalt, passwordHash: result.passwordHash};
    } catch (error) {
      console.error("generatePassword:", error);
      return null;
    }
  }

  async function generateClientResponse(password, passwordSalt, challenge) {
    try {
      const payload = {password, passwordSalt, challenge};
      const result = await _postMessage("generateClientResponse", payload);
      return {passwordHash: result.passwordHash, data: result.data};
    } catch (error) {
      console.error("generatePassword:", error);
      return null;
    }
  }

  async function blobifyData(_userId, data) {
    // FIXME: Implement
    return data;
  }

  async function unblobifyData(_userId, data) {
    // FIXME: Implement
    return data;
  }

  async function blobifyFile(_userId, file) {
    // FIXME: Implement
    return file;
  }

  async function unblobifyFile(_userId, file) {
    // FIXME: Implement
    return file;
  }

  async function unblobifyChunk(_userId, chunk) {
    // FIXME: Implement
    return chunk;
  }

  async function _postMessage(action, payload) {
    return new Promise((resolve, reject) => {
      _worker.onmessage = (e) => {
        if (e.data.success) {
          resolve(e.data);
        } else {
          reject(e.data.error);
        }
      };
      _worker.onerror = err => {
        reject(err);
      };
      _worker.postMessage({action, payload});
    });
  }

  function terminate() {
    _worker.terminate();
  }

  return {
    generatePasswordHash,
    generateClientResponse,
    terminate,
    blobifyData,
    unblobifyData,
    blobifyFile,
    unblobifyFile,
    unblobifyChunk
  };
})();
