// -*- fill-column: 100 -*-

"use strict";

const FileDB = (function () {
  let _db;

  // Expose a "ready" promise that resolves when DB is initialized.
  const ready = new Promise((resolve, reject) => {
    const request = indexedDB.open("FileObjectsDB", 1);
    request.onerror = event => {
      console.error("IndexedDB error:", event.target.error);
      reject(event.target.error);
    };
    request.onupgradeneeded = event => {
      _db = event.target.result;
      if (!_db.objectStoreNames.contains("files")) {
        _db.createObjectStore("files", {keyPath: "id", autoIncrement: true});
      }
    };
    request.onsuccess = event => {
      _db = event.target.result;
      resolve(_db);
    };
  });

  async function saveFile(file) {
    await ready;
    return new Promise((resolve, reject) => {
      const transaction = _db.transaction(["files"], "readwrite");
      const store = transaction.objectStore("files");
      const record = {filename: file.name, file: file};
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

  async function removeFile(reversedIndex) {
    await ready;
    return new Promise((resolve, reject) => {
      const transaction = _db.transaction(["files"], "readwrite");
      const store = transaction.objectStore("files");
      const request = store.getAll();
      request.onsuccess = () => {
        const files = request.result;
        if (reversedIndex < 0 || reversedIndex >= files.length) {
          reject(new Error("Index out of bounds"));
          return;
        }
        const actualIndex = files.length - reversedIndex - 1;
        const recordToDelete = files[actualIndex];
        const deleteRequest = store.delete(recordToDelete.id);
        deleteRequest.onsuccess = () => resolve();
        deleteRequest.onerror = () => reject(deleteRequest.error);
      };
      request.onerror = () => reject(request.error);
    });
  }

  async function clearFiles() {
    await ready;
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
    await ready;
    try {
      const transaction = _db.transaction(["files"], "readonly");
      const store = transaction.objectStore("files");
      const request = store.getAll();
      return await new Promise((resolve, reject) => {
        request.onsuccess = () => resolve(request.result.reverse());
        request.onerror = () => reject(request.error);
      });
    } catch (err) {
      if (err.name === "NotFoundError") {
        return [];
      }
      throw err;
    }
  }

  async function numberOfFiles() {
    await ready;
    return new Promise((resolve, reject) => {
      const transaction = _db.transaction(["files"], "readonly");
      const store = transaction.objectStore("files");
      const request = store.count();
      request.onsuccess = () => resolve(request.result);
      request.onerror = () => reject(request.error);
    });
  }

  return {
    ready,
    saveFile,
    removeFile,
    clearFiles,
    getAllFiles,
    numberOfFiles
  };
})();
