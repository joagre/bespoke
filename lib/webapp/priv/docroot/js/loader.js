// -*- fill-column: 100 -*-

"use strict";

const Loader = (function () {
  let _cookieState;
  // See: /home/jocke/.bespoke/keys/bespoke-pem.pub
  const _publicKeyPem = `MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAqOEtEHI0tPHjk9Lo5f5i
sCYeBswhek1xuaFRFtxtO8r6j+wxa1z6Z1d3rK6hgvCSf3/2H06kgEIhF2ONXHkc
kVLe0D4RapNuVoiAZCXHvaalTvb0xwT9/ExP8O7+DZioA26ayiD2FXEqNdoUzGwV
DuFm0e9+rmemiLhZCki89p+3qZVs4CWlJfchUpDMfpYchrI7kWq2/PjJ0QhygRVf
TddgbV0doy3kpfe3+uROOvDp8zVqVnhnqUQ5nghxQZDg/rVK+pTmYvu/gxc+/TIj
v+2JCXQKj8eC2GhI2kAGHbw55iTozK1o+8mmepfoaPd0ZtWVx/KCBIls8rEP8Ygt
owIDAQAB`;
  let _publicKey;

  function init() {
    if (window.location.hostname !== "localhost" &&
        window.location.hostname !== "b3s.f0ff" &&
        !_isIPv4Address(window.location.hostname)) {
      const targetUrl = `http://b3s.f0ff${window.location.pathname}`;
      window.location.href = targetUrl;
      return;
    }
    _initCookieState();
    _importPublicKey();
    _initServiceWorker();

    const redirect = async () => {
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
        _setCookieValue("userId", result.userId);
        _setCookieValue("username", result.username);
        _setCookieValue("sessionId", result.sessionId);
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
          const scheme = window.location.protocol.slice(0, -1);
          const hostname = window.location.hostname;
          const url = `intent://${hostname}${absPath}#Intent;scheme=${scheme};end`;
          window.location.href = url;
        } else {
          window.location.href =  absPath;
        }
      } catch (error) {
        console.error("Auto login failed:", error);
      }
    };
    redirect();
  }

  function _isIPv4Address(string) {
    const IPv4Regex =
          /^(25[0-5]|(2[0-4]|1\d|[1-9])?\d)\.(25[0-5]|(2[0-4]|1\d|[1-9])?\d)\.(25[0-5]|(2[0-4]|1\d|[1-9])?\d)\.(25[0-5]|(2[0-4]|1\d|[1-9])?\d)$/;
    return IPv4Regex.test(string);
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
    base64 = base64.replace(/\s/g, "");
    const binaryString = atob(base64);
    const bytes = new Uint8Array([...binaryString].map(char => char.charCodeAt(0)));
    return bytes.buffer;
  }

  function _initServiceWorker() {
    if (!("serviceWorker" in navigator)) {
      console.error("Service Worker not supported");
      return;
    }

    navigator.serviceWorker.register("/sw.js").then(async (registration) => {
      try {
        // 1) Fetch /sw.js (as text) so we can verify its exact contents
        const swResponse = await fetch("/sw.js", { cache: "no-store" });
        if (!swResponse.ok) {
          throw new Error(`Failed to fetch /sw.js. Status: ${swResponse.status}`);
        }
        const swText = await swResponse.text();
        // 2) Fetch /sw.js.sig (as ArrayBuffer)
        const sigResponse = await fetch("/sw.js.sig", { cache: "no-store" });
        if (!sigResponse.ok) {
          throw new Error(`Failed to fetch /sw.js.sig. Status: ${sigResponse.status}`);
        }
        const swSigBase64 = await sigResponse.text();
        const swSig = _base64ToArrayBuffer(swSigBase64);
        // 3) Verify the signature
        const isValid = await _verifySignature(swText, swSig);
        if (!isValid) {
          // 4) Signature invalid => remove the new /sw.js
          console.error("Service Worker: Refusing new /sw.js, signature invalid");
          await registration.unregister();
        } else {
          console.log("Service Worker: /sw.js signature verified; continuing");
          // The new SW can install/activate.
        }
      } catch (err) {
        console.error("Service Worker: Failed to verify /sw.js signature:", err);
        // Decide if you want to unregister in case of an unexpected error
        await registration.unregister();
      }
    }).catch((err) => {
      console.error("Service Worker: Registration failed:", err);
    });
  }

  async function _verifySignature(code, signature) {
    const buffer = new TextEncoder().encode(code);
    const isValid = await crypto.subtle.verify("RSASSA-PKCS1-v1_5", _publicKey, signature, buffer);
    return isValid;
  }

  return {
    init
  };
})();

Loader.init();
