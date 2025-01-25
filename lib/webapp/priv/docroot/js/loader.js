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
  // Instantiated by _importPublicKey()
  let _publicKey;

  function init() {
    if (window.location.hostname !== "localhost" &&
        window.location.hostname !== "b3s.zone" &&
        !_isIPv4Address(window.location.hostname)) {
      const targetUrl = `https://b3s.zone${window.location.pathname}`;
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
          // Use an intent-based link on Android
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
    const bin = atob(base64.replace(/\s+/g, ""));
    const bytes = new Uint8Array([...bin].map(char => char.charCodeAt(0)));
    return bytes.buffer;
  }

  function _initServiceWorker() {
    if (!("serviceWorker" in navigator)) {
      console.error("Service Worker not supported!");
      return;
    }

    navigator.serviceWorker.register("/sw.js").then(async (registration) => {
      try {
        // 1) Fetch sw.js
        const swResp = await fetch("/sw.js", {cache: "no-store"});
        if (!swResp.ok) {
          throw new Error(`Failed to fetch sw.js: ${swResp.status}`);
        }
        const swText = await swResp.text();
        // 2) Fetch /signatures/sw.js.sig
        const sigResp = await fetch("/signatures/sw.js.sig", {cache: "no-store"});
        if (!sigResp.ok) {
          throw new Error(`Failed to fetch /signatures/sw.js.sig: ${sigResp.status}`);
        }
        const swSigBase64 = await sigResp.text();
        const swSig = _base64ToArrayBuffer(swSigBase64);
        // 3) Verify signature
        const isValid = await _verifySignature(swText, swSig);
        if (!isValid) {
          // 4a) Signature invalid
          console.error("Refusing new sw.js (signature invalid)");
          await registration.unregister();
        } else {
          // 4b) Success!
          console.log("sw.js signature verified");
        }
      } catch (err) {
        // 5) Something went wrong
        console.error("Failed to verify sw.js signature:", err);
        await registration.unregister();
      }
    }).catch((err) => {
      // 6) Registration failed
      console.error("Registration failed:", err);
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
