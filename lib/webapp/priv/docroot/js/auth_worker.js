importScripts("/js/sodium.js");

self.onmessage = async (e) => {
  await sodium.ready;
  const {action, payload} = e.data;
  switch (action) {
  case "generatePasswordHash": {
    const password = payload;
    const passwordSalt =
          sodium.randombytes_buf(sodium.crypto_pwhash_SALTBYTES);
    const passwordHash = sodium.crypto_pwhash(
      sodium.crypto_secretbox_KEYBYTES,
      password,
      passwordSalt,
      sodium.crypto_pwhash_OPSLIMIT_MODERATE,
      sodium.crypto_pwhash_MEMLIMIT_MODERATE,
      sodium.crypto_pwhash_ALG_ARGON2ID13
    );
    self.postMessage({success: true, passwordSalt, passwordHash});
    break;
  }
  case "generateClientResponse": {
    const passwordHash = sodium.crypto_pwhash(
      sodium.crypto_secretbox_KEYBYTES,
      payload.password,
      payload.passwordSalt,
      sodium.crypto_pwhash_OPSLIMIT_MODERATE,
      sodium.crypto_pwhash_MEMLIMIT_MODERATE,
      sodium.crypto_pwhash_ALG_ARGON2ID13
    );
    // Compute the client response using HMAC
    const data = sodium.crypto_auth_hmacsha256(payload.challenge, passwordHash);
    self.postMessage({success: true, passwordHash, data});
    break;
  }
  default:
    self.postMessage({success: false, error: "Unknown action"});
  }
};
