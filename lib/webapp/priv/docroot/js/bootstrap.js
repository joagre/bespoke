import bespoke from "/js/bespoke.js";

class Bootstrap {
  constructor() {
    bespoke.onReady("bootstrap.html", () => this._load());
  }

  _load() {
    this._formSSID = document.getElementById("form-ssid");
    this._formSSID.addEventListener("input", () => this._checkFormCompletion());
    this._formSSIDError = document.getElementById("form-ssid-error");
    this._bootstrapButton = document.getElementById("bootstrap-button");
    this._updatePage();
    this._checkFormCompletion();
  }

  _checkFormCompletion() {
    if (!this._isValidSSID(this._formSSID.value)) {
      this._formSSIDError.innerText = "1–32 chars: letters, numbers, _ or -";
      this._formSSIDError.style.display = "block";
      this._bootstrapButton.disabled = true;
    } else {
      this._formSSIDError.style.display = "none";
      this._bootstrapButton.disabled = false;
    }
  }

  _isValidSSID(ssid) {
    if (ssid.length == 0) {
      return false;
    }
    const regex = /^[A-Za-z0-9_-]{1,32}$/;
    return regex.test(ssid);
  }

  _updatePage() {
    this._formSSID.focus();
  }

  bootstrapNow(event) {
    bespoke.ignoreEvent(event);
    this._formSSIDError.style.display = "none";
    const updateServer = async () => {
      try {
        // REST: Bootstrap
        const payload = {
          ssid: this._formSSID.value
        };
        const response = await fetch("/bootstrap", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify(payload)
        });
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          this._formSSIDError.innerText = "Internal error";
          this._formSSIDError.style.display = "block";
          this._formSSID.focus();
          return;
        }
        bespoke.navigateTo("loader.html");
      } catch (error) {
        console.error("Bootstrap failed", error);
      }
    };
    updateServer();
  }
}

const bootstrap = new Bootstrap();
export default bootstrap
