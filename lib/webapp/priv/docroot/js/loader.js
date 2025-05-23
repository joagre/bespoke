// -*- fill-column: 100 -*-

"use strict";

const Loader = (function () {
  // See: /home/jocke/.bespoke/keys/bespoke-pem.pub
  const _publicKeyPem = `MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAqOEtEHI0tPHjk9Lo5f5i
sCYeBswhek1xuaFRFtxtO8r6j+wxa1z6Z1d3rK6hgvCSf3/2H06kgEIhF2ONXHkc
kVLe0D4RapNuVoiAZCXHvaalTvb0xwT9/ExP8O7+DZioA26ayiD2FXEqNdoUzGwV
DuFm0e9+rmemiLhZCki89p+3qZVs4CWlJfchUpDMfpYchrI7kWq2/PjJ0QhygRVf
TddgbV0doy3kpfe3+uROOvDp8zVqVnhnqUQ5nghxQZDg/rVK+pTmYvu/gxc+/TIj
v+2JCXQKj8eC2GhI2kAGHbw55iTozK1o+8mmepfoaPd0ZtWVx/KCBIls8rEP8Ygt
owIDAQAB`;

  let _cookieState;
  let _publicKey; // Instantiated by _importPublicKey()
  let _secure = true;

  _initCookieState();

  if (!("serviceWorker" in navigator) && window.location.hostname !== "localhost") {
    _secure = false;
    console.error("This browser does not support a secure loader");
    localStorage.setItem("insecureWarningMessage", "This browser does not support a secure loader");
  }

  _autoLogin();

  async function _autoLogin() {
    // Make sure to clear any spurious attachments
    await AddAttachments.clearFiles();

    if (_secure) {
      await _importPublicKey();
      await _initServiceWorker();
    }
    try {
      // REST: Auto login
      const response = await fetch("/api/auto_login");
      if (response.redirected) {
        window.location.href = response.url;
        return;
      } else if (!response.ok) {
        console.error(`Login failed: ${response.status}`);
        return;
      }
      const result = await response.json();

      // Set cookies
      _setCookieValue("bbsName", result.bbsName);
      _setCookieValue("userId", result.userId);
      _setCookieValue("username", result.username);
      _setCookieValue("sessionId", result.sessionId);

      // Create introduction message for new users
      if (result.isNew) {
        await MessageLib.createIntroductionMessage(result.userId, result.username);
      }

      // Redirect to next page
      let absPath;
      if (result.noPassword) {
        absPath = "/index.html";
      } else {
        absPath = "/login.html";
      }
      const isAndroid = navigator.userAgentData
            ? navigator.userAgentData.platform.toLowerCase().includes("android")
            : /Android/i.test(navigator.userAgent);
      if (isAndroid) {
        // Use an intent-based link on Android
        const scheme = window.location.protocol.slice(0, -1);
        const hostname = window.location.hostname;
        const href = `intent://${hostname}${absPath}#Intent;scheme=${scheme};end`;
        window.location.href = href;
      } else {
        window.location.href = absPath;
      }
    } catch (error) {
      console.error("Auto login failed:", error);
    }
  }

  // Extracted from /js/bespoke.js to make loader.js self-contained
  function _initCookieState() {
    _cookieState = _getCookie("bespoke");
    if (_cookieState == null) {
      _cookieState = {};
    }
  }

  // Extracted from /js/bespoke.js to make loader.js self-contained
  function _getCookie(name) {
    const value = `; ${document.cookie}`;
    const parts = value.split(`; ${name}=`);
    if (parts.length === 2) {
      try {
        return JSON.parse(decodeURIComponent(parts.pop().split(";").shift()));
      } catch (e) {
        console.error("Error parsing Bespoke cookie:", e);
      }
    }
    return null;
  }

  // Extracted from /js/bespoke.js to make loader.js self-contained
  function _setCookieValue(name, value) {
    _cookieState[name] = value;
    _updateCookieState();
  }

  // Extracted from /js/bespoke.js to make loader.js self-contained
  function _updateCookieState() {
    _setCookie("bespoke", _cookieState, 7);
  }

  // Extracted from /js/bespoke.js to make loader.js self-contained
  function _setCookie(name, value, days = 7) {
    let expires = "";
    if (days) {
      const date = new Date();
      date.setTime(date.getTime() + days * 24 * 60 * 60 * 1000);
      expires = `; expires=${date.toUTCString()}`;
    }
    const cookieValue = typeof value === "object" ? JSON.stringify(value) : value;
    document.cookie =
      `${name}=${encodeURIComponent(cookieValue)}${expires}; path=/; SameSite=Strict`;
  }

  async function _importPublicKey() {
    _publicKey = await crypto.subtle.importKey(
      "spki", _base64ToArrayBuffer(_publicKeyPem),
      {
        name: "RSASSA-PKCS1-v1_5",
        hash: "SHA-256"
      },
      false, ["verify"]
    );
  }

  function _base64ToArrayBuffer(base64) {
    const bin = atob(base64.replace(/\s+/g, ""));
    const bytes = new Uint8Array([...bin].map(char => char.charCodeAt(0)));
    return bytes.buffer;
  }

  async function _initServiceWorker() {
    localStorage.removeItem("insecureWarningMessage");
    let registration = null;
    // Register service worker
    try {
      registration = navigator.serviceWorker.register("/loader_worker.js");
    } catch (err) {
      _secure = false;
      console.error("Secure loader could not be registered:", err);
      localStorage.setItem("insecureWarningMessage", "Secure loader could not be registered");
      return;
    }
    // Install loader_worker.js
    try {
      // Fetch loader_worker.js
      const swResp = await fetch("/loader_worker.js", {cache: "no-store"});
      if (!swResp.ok) {
        throw new Error(`sw.sj: ${swResp.status}`);
      }
      const swText = await swResp.text();

      // Fetch /signatures/loader_worker.js.sig
      const sigResp = await fetch("/signatures/loader_worker.js.sig", {cache: "no-store"});
      if (!sigResp.ok) {
        throw new Error(`loader_worker.js.sig: ${sigResp.status}`);
      }
      const swSigBase64 = await sigResp.text();
      const swSig = _base64ToArrayBuffer(swSigBase64);

      // Verify sw.sj signature
      const isValid = await _verifySignature(swText, swSig);
      if (!isValid) {
        // Signature invalid!
        _secure = false;
        const errorMessage = "Secure loader signature is invalid";
        console.error(errorMessage);
        localStorage.setItem("insecureWarningMessage", errorMessage);
        if (registration != null) {
          await registration.unregister();
        }
      } else {
        // Success!
        console.log("Secure loader signature has been verified");
      }
    } catch (err) {
      // Something went wrong!
      _secure = false;
      const errorMessage = `Secure loader failed to start (${err})`;
      console.error(errorMessage);
      localStorage.setItem("insecureWarningMessage", errorMessage);
      await registration.unregister();
      return;
    }

    // Success! Wait for activation
    try {
      await navigator.serviceWorker.ready;
      console.log("Secure loader is ready");
    } catch (err) {
      _secure = false;
      const errorMessage = `Secure loader failed to go active (${err})`;
      console.error(errorMessage);
      localStorage.setItem("insecureWarningMessage", errorMessage);
    }
  }

  async function _verifySignature(code, signature) {
    const buffer = new TextEncoder().encode(code);
    const isValid = await crypto.subtle.verify("RSASSA-PKCS1-v1_5", _publicKey, signature, buffer);
    return isValid;
  }

  return {
  };
})();
