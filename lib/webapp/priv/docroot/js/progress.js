// -*- fill-column: 100 -*-

"use strict";

/*
Requires the first div in the body to be:

<!-- Modal full screen overlay -->
<div id="loading-overlay" class="uk-overlay uk-overlay-default uk-position-cover uk-flex uk-flex-center uk-flex-middle" hidden>
  <div class="uk-width-1-3">
    <progress id="progress-bar" class="uk-progress uk-margin-remove" value="0" max="100"></progress>
    <div class="uk-text-center">
      <span id="progress-counter" class="uk-text-meta">0%</span>
    </div>
  </div>
</div>
*/

const Progress = (function () {
  let _progressBar;
  let _progressCounter;

  document.addEventListener("DOMContentLoaded", () => {
    _progressBar = document.getElementById("progress-bar");
    _progressCounter = document.getElementById("progress-counter");
  });

  function show() {
    _progressCounter.value = "0%";
    Bespoke.showLoadingSpinner();
  }

  function hide() {
    Bespoke.hideLoadingSpinner();
    _progressCounter.innerText = "0%";
  }

  function update(total, loaded, percentComplete) {
    _progressBar.max = total;
    _progressBar.value = loaded;
    if (total === 0) {
      _progressCounter.innerText = "0%";
    } else {
      const percentage = Math.round(percentComplete);
      _progressCounter.innerText = `${percentage}%`;
    }
  }

  return {
    show,
    hide,
    update
  };
})();
