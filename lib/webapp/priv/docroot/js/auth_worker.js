importScripts("/js/sodium.js");

self.onmessage = async (e) => {
  await sodium.ready;
  const {action, payload} = e.data;
  switch (action) {
  case "generatePasswordHash": {
    const password = payload;
    const salt = sodium.randombytes_buf(sodium.crypto_pwhash_SALTBYTES);
    const hash = sodium.crypto_pwhash(
      sodium.crypto_secretbox_KEYBYTES,
      password,
      salt,
      sodium.crypto_pwhash_OPSLIMIT_MODERATE,
      sodium.crypto_pwhash_MEMLIMIT_MODERATE,
      sodium.crypto_pwhash_ALG_ARGON2ID13
    );
    self.postMessage({success: true, salt: salt, hash:hash});
    break;
  }
  case "generateClientResponse": {
    const hash = sodium.crypto_pwhash(
      sodium.crypto_secretbox_KEYBYTES,
      payload.password,
      payload.salt,
      sodium.crypto_pwhash_OPSLIMIT_MODERATE,
      sodium.crypto_pwhash_MEMLIMIT_MODERATE,
      sodium.crypto_pwhash_ALG_ARGON2ID13
    );
    // Compute the client response using HMAC
    const clientResponse = sodium.crypto_auth(payload.challenge, hash);
    self.postMessage({success: true, clientResponse});
    break;
  }
  default:
    self.postMessage({success: false, error: "Unknown action"});
  }
};
