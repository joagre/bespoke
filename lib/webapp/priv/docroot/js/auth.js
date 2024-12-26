class Auth {
  constructor() {
    document.addEventListener("DOMContentLoaded", () => {
      this.worker = new Worker("/js/auth_worker.js");
    });
  }

  async generatePasswordHash(password) {
    try {
      const result = await this._postMessage("generatePasswordHash", password);
      return {passwordSalt: result.passwordSalt,
              passwordHash: result.passwordHash};
    } catch (error) {
      console.error("generatePassword:", error);
      return null;
    }
  }

  async generateClientResponse(password, passwordSalt, challenge) {
    try {
      const payload = {password, passwordSalt, challenge};
      const result = await this._postMessage("generateClientResponse", payload);
      return {passwordHash: result.passwordHash, data: result.data};
    } catch (error) {
      console.error("generatePassword:", error);
      return null;
    }
  }

  async _postMessage(action, payload) {
    return new Promise((resolve, reject) => {
      this.worker.onmessage = (e) => {
        if (e.data.success) {
          resolve(e.data);
        } else {
          reject(e.data.error);
        }
      };
      this.worker.onerror = (err) => {
        reject(err);
      };
      this.worker.postMessage({action, payload});
    });
  }

  terminate() {
    this.worker.terminate();
  }
}

const auth = new Auth();
export default auth
