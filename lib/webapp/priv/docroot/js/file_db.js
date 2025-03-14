// -*- fill-column: 100 -*-

"use strict";

const FileDB = (function () {
  let _db;
  // Expose a "ready" promise that resolves when the DB is initialized.
  const ready = new Promise((resolve, reject) => {
    const request = indexedDB.open("FileObjectsDB", 1);
    request.onerror = event => {
      console.error("IndexedDB error:", event.target.error);
      reject(event.target.error);
    };
    request.onupgradeneeded = event => {
      _db = event.target.result;
      if (!_db.objectStoreNames.contains("files")) {
        _db.createObjectStore("files", { keyPath: "id", autoIncrement: true });
      }
    };
    request.onsuccess = event => {
      _db = event.target.result;
      resolve(_db);
    };
  });

  async function saveFile(file) {
    await ready; // Ensure DB is ready
    return new Promise((resolve, reject) => {
      const transaction = _db.transaction(["files"], "readwrite");
      const store = transaction.objectStore("files");
      const record = { filename: file.name, file: file };
      const request = store.add(record);
      request.onerror = event => {
        console.error("Error storing file in IndexedDB:", event.target.error);
        reject(event.target.error);
      };
      request.onsuccess = event => {
        console.log("File stored in IndexedDB:", file.name);
        resolve(event.target.result);
      };
    });
  }

  async function clearFiles() {
    await ready; // Wait until DB is ready
    return new Promise((resolve, reject) => {
      const transaction = _db.transaction(["files"], "readwrite");
      const store = transaction.objectStore("files");
      const request = store.clear();
      request.onerror = event => {
        console.error("Error clearing IndexedDB files:", event.target.error);
        reject(event.target.error);
      };
      request.onsuccess = _event => {
        console.log("IndexedDB files cleared.");
        resolve();
      };
    });
  }

  async function getAllFiles() {
    await ready; // Wait until DB is ready
    return new Promise((resolve, reject) => {
      const transaction = _db.transaction(["files"], "readonly");
      const store = transaction.objectStore("files");
      const request = store.getAll();
      request.onsuccess = () => resolve(request.result);
      request.onerror = () => reject(request.error);
    });
  }

  return {
    ready,
    saveFile,
    clearFiles,
    getAllFiles
  };
})();
