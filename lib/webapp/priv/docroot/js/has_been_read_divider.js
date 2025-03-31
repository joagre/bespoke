// -*- fill-column: 100 -*-

"use strict";

const HasBeenReadDivider = (function () {
  function createObservers(type) {
    const dividerElements = document.getElementsByClassName(`${type}-divider`);
    let firstUnreadFound = false;
    let element = null;
    for (const dividerElement of dividerElements) {
      element = dividerElement.closest(`[data-${type}-id]`);
      const id = _getId(element, type);
      const isRead = element.getAttribute("data-is-read") == "true";
      if (!isRead) {
        // Add observer if not read
        _addHasBeenReadObserver(type, dividerElement);
        // Mark first unread
        if (!firstUnreadFound) {
          console.log(`${id} is first unread ${type}`);
          element.setAttribute("id", `unread-${type}`);
          firstUnreadFound = true;
        }
      } else {
        // Mark it as read
        const metaDataElement = element.querySelector(".meta-data");
        const hasBeenReadElement = metaDataElement.children[0];
        hasBeenReadElement.hidden = true;
      }
    }
    // If all are read, mark last as unread to make sure that is shown
    if (!firstUnreadFound && element != null) {
      const id = _getId(element, type);
      console.log(`${id} is last`);
      element.setAttribute("id", `unread-${type}`);
    }
  }

  function _getId(element, type) {
    if (type == "message") {
      return Number(element.getAttribute("data-message-id"));
    } else {
      return element.getAttribute("data-post-id");
    }
  }

  function _addHasBeenReadObserver(type, dividerElement) {
    const options = {
      root: null, // Uses viewport as root
      rootMargin: "0px", // No margin adjustments; full viewport
      threshold: 0 // Trigger when any part is visible
    };
    const callback = (entries, observer) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          const element = dividerElement.closest(`[data-${type}-id]`);
          const id = _getId(element, type);
          console.log(`${id} has been read`);
          if (type == "message") {
            Bespoke.markMessageAsRead(id);
          } else {
            Bespoke.markPostAsRead(id);
          }
          observer.unobserve(entry.target);
        }
      });
    };
    const observer = new IntersectionObserver(callback, options);
    observer.observe(dividerElement);
  }

  return {
    createObservers
  };
})();
