"use strict";

// See: /home/jocke/.bespoke/keys/bespoke-pem.pub
const publicKeyPem = `MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAqOEtEHI0tPHjk9Lo5f5i
sCYeBswhek1xuaFRFtxtO8r6j+wxa1z6Z1d3rK6hgvCSf3/2H06kgEIhF2ONXHkc
kVLe0D4RapNuVoiAZCXHvaalTvb0xwT9/ExP8O7+DZioA26ayiD2FXEqNdoUzGwV
DuFm0e9+rmemiLhZCki89p+3qZVs4CWlJfchUpDMfpYchrI7kWq2/PjJ0QhygRVf
TddgbV0doy3kpfe3+uROOvDp8zVqVnhnqUQ5nghxQZDg/rVK+pTmYvu/gxc+/TIj
v+2JCXQKj8eC2GhI2kAGHbw55iTozK1o+8mmepfoaPd0ZtWVx/KCBIls8rEP8Ygt
owIDAQAB`;
// Instantiated by importPublicKey()
let publicKey;
let publicKeyPromise = importPublicKey();

const FILE_CACHE_NAME = "trusted-files-v1";
const DB_NAME = "SignatureDB";
const DB_VERSION = 1;
const STORE_NAME = "signatures";

function openSignatureDB() {
  return new Promise((resolve, reject) => {
    const request = indexedDB.open(DB_NAME, DB_VERSION);
    request.onupgradeneeded = (_event) => {
      const db = request.result;
      if (!db.objectStoreNames.contains(STORE_NAME)) {
        db.createObjectStore(STORE_NAME, {keyPath: "fileUrl"});
      }
    };
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => reject(request.error);
  });
}

async function getStoredSignature(fileUrl) {
  const db = await openSignatureDB();
  return new Promise((resolve, reject) => {
    const tx = db.transaction(STORE_NAME, "readonly");
    const store = tx.objectStore(STORE_NAME);
    const getReq = store.get(fileUrl);
    getReq.onsuccess = () => {
      const entry = getReq.result;
      resolve(entry ? entry.signature : null);
    };
    getReq.onerror = () => reject(getReq.error);
  });
}

async function storeSignature(fileUrl, signatureBase64) {
  const db = await openSignatureDB();
  return new Promise((resolve, reject) => {
    const tx = db.transaction(STORE_NAME, "readwrite");
    const store = tx.objectStore(STORE_NAME);
    store.put({ fileUrl, signature: signatureBase64 });
    tx.oncomplete = () => resolve();
    tx.onerror = () => reject(tx.error);
  });
}

function base64ToArrayBuffer(base64) {
  const bin = atob(base64.replace(/\s+/g, ""));
  const bytes = new Uint8Array([...bin].map(char => char.charCodeAt(0)));
  return bytes.buffer;
}

async function importPublicKey() {
  console.log("Importing public key...");
  publicKey = await crypto.subtle.importKey(
    "spki", base64ToArrayBuffer(publicKeyPem),
    {
      name: "RSASSA-PKCS1-v1_5",
      hash: "SHA-256"
    },
    false, ["verify"]
  );
  console.log("Done!");
}

async function verifySignature(code, signature) {
  const buffer = new TextEncoder().encode(code);
  const isValid = await crypto.subtle.verify("RSASSA-PKCS1-v1_5", publicKey, signature, buffer);
  return isValid;
}

function getSignatureUrl(fileUrl) {
  const parsed = new URL(fileUrl);
  const filename = parsed.pathname.split("/").pop();
  return `${parsed.origin}/signatures/${filename}.sig`;
}

self.addEventListener("install", (event) => {
  console.log("Install event triggered");
  event.waitUntil(publicKeyPromise);
});

self.addEventListener("activate", (_event) => {
  console.log("Activate event triggered");
});

self.addEventListener("fetch", (event) => {
  event.respondWith(
    (async () => {
      await publicKeyPromise; // Ensure the public key is loaded before handling requests
      const reqUrl = new URL(event.request.url);
      const isHTML = reqUrl.pathname.endsWith(".html");
      const isJS = reqUrl.pathname.endsWith(".js");
      if (isHTML || isJS) {
        return handleHTMLorJSRequest(event.request);
      } else {
        const cached = await caches.match(event.request);
        return cached || fetch(event.request);
      }
    })()
  );
});

/*
self.addEventListener("fetch", (event) => {
  const reqUrl = new URL(event.request.url);
  const isHTML = reqUrl.pathname.endsWith(".html");
  const isJS = reqUrl.pathname.endsWith(".js");
  if (isHTML || isJS) {
    event.respondWith(handleHTMLorJSRequest(event.request));
  } else {
    event.respondWith(
      caches.match(event.request).then((cached) => {
        return cached || fetch(event.request);
      })
    );
  }
});
*/

async function handleHTMLorJSRequest(request) {
  const fileUrl = request.url;
  const sigUrl = getSignatureUrl(fileUrl);
  // 1) Fetch signature from server
  let newSigBase64;
  try {
    const sigResp = await fetch(sigUrl, {cache: "no-store"});
    if (!sigResp.ok) {
      console.error("Signature fetch failed:", sigUrl, sigResp.status);
      return fallbackOr503(request, "Signature fetch error");
    }
    newSigBase64 = await sigResp.text();
  } catch (err) {
    console.error("Signature network error:", err);
    return fallbackOr503(request, "Signature network error");
  }
  // 2) Compare with old signature
  const oldSigBase64 = await getStoredSignature(fileUrl);
  if (oldSigBase64 && oldSigBase64 === newSigBase64) {
    const fileCache = await caches.open(FILE_CACHE_NAME);
    const cachedFileResp = await fileCache.match(request);
    if (cachedFileResp) {
      console.log("Signature unchanged, serving cached verified file:", fileUrl);
      return cachedFileResp;
    } else {
      console.warn("Have old signature but missing file in cache. Refetching file...");
    }
  }
  // 3) If no old signature or it differs
  let netFileResp;
  try {
    netFileResp = await fetch(request, {cache: "no-store"});
    if (!netFileResp.ok) {
      console.error(`File fetch error ${netFileResp.status} for`, fileUrl);
      return fallbackOr503(request, "File fetch error");
    }
  } catch (err) {
    console.error("Network error fetching file:", err);
    return fallbackOr503(request, "Network error fetching file");
  }
  const fileText = await netFileResp.text();
  // 4) Verify signature
  try {
    const sigBuffer = base64ToArrayBuffer(newSigBase64);
    const valid = await verifySignature(fileText, sigBuffer);
    if (!valid) {
      console.error("Invalid signature for", fileUrl);
      return fallbackOr503(request, "Invalid signature");
    }
  } catch (err) {
    console.error("Error verifying signature for", fileUrl, err);
    const errorString = `Error verifying signature: err: ${err}\nfileUrl: ${fileUrl}\nsigUrl: ${sigUrl}\noldSig: ${oldSigBase64}\nnewSig: ${newSigBase64}\nfileText: ${fileText}`;
    return fallbackOr503(request, errorString);
  }
  console.log("Verified new file, caching:", fileUrl);
  // 5) Store new signature and file
  await storeSignature(fileUrl, newSigBase64);
  const verifiedResp = new Response(fileText, {
    status: 200,
    statusText: "OK",
    headers: netFileResp.headers
  });
  const fileCache = await caches.open(FILE_CACHE_NAME);
  await fileCache.put(request, verifiedResp.clone());
  return verifiedResp;
}

async function fallbackOr503(request, msg) {
  console.warn(`${msg} => fallback or 503 for`, request.url);
  const fileCache = await caches.open(FILE_CACHE_NAME);
  const oldResp = await fileCache.match(request);
  if (oldResp) {
    console.warn("Serving old file from cache as fallback.");
    return oldResp;
  } else {
    return new Response(msg, {status: 503});
  }
}
