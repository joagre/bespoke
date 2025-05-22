// -*- fill-column: 100 -*-

"use strict";

const ScrollNavigator = (function() {
  let _handler;
  let _initialized = false;
  let _plugin = null;
  let _sections;
  let _sectionTops;
  let _currentIndex;

  function init(plugin) {
    if (_initialized) {
      destroy();
    }

    _plugin = plugin;
    _sections = plugin.sections;
    _sectionTops = _sections.map(el => el.offsetTop);
    _currentIndex = null;
    _handler = _onKeyDown;

    // Add event listeners
    window.addEventListener("resize", _onResize);
    window.addEventListener("keydown", _onKeyDown);

    _initialized = true;
  }

  function destroy() {
    if (!_initialized) {
      return;
    }

    // Remove event listeners
    window.removeEventListener("keydown", _handler);
    window.removeEventListener("resize", _onResize);

    _sections = null;
    _sectionTops = null;
    _currentIndex = null;
    _handler = null;
    _initialized = false;
  }

  function _initIndex() {
    const scrollY = window.scrollY;
    _currentIndex = _sectionTops
      .map((top, i) => ({top, i}))
      .filter(x => x.top <= scrollY + 1)
      .reduce((best, cur) => cur.top > best.top ? cur : best, {top: -Infinity, i: 0})
      .i;
  }

  function _getIndexByScroll() {
    const scrollY = window.scrollY;
    let index = 0;
    for (let i = 0; i < _sectionTops.length; i++) {
      if (_sectionTops[i] <= scrollY + 1) {
        index = i;
      } else {
        break;
      }
    }
    return index;
  }

  function _onKeyDown(e) {
    const key = e.key;
    if (key === "ArrowDown" || key === "ArrowUp") {
      e.preventDefault();

      console.error(`Key pressed: ${key}`);

      if (_currentIndex === null) {
        _initIndex();
      }

      if (key === "ArrowDown" && _currentIndex < _sections.length - 1) {
        _currentIndex++;
      } else if (key === "ArrowUp" && _currentIndex > 0) {
        _currentIndex--;
      }

      // Scroll
      const el = _sections[_currentIndex];
      let targetPos = el.offsetTop;
      console.log(`Scrolling to section ${_currentIndex} (${el.id}) at position ${targetPos}`);
      if (_plugin.headerHeight != null) {
        targetPos -= _plugin.headerHeight;
        console.log(`Scrolling to section ${_currentIndex} (${el.id}) at position ${targetPos}`);
      }
      window.scrollTo({
        top: targetPos,
        behavior: "smooth"
      });

      // Mark as post section as active, add a active-post class
      _sections.forEach(section => section.classList.remove("active-post"));
      el.classList.add("active-post");
    } else if (key === "ArrowLeft" || key === "ArrowRight" || key === "Escape") {
      e.preventDefault();
      const index = (_currentIndex !== null) ? _currentIndex : _getIndexByScroll();
      if (_plugin.keyDown != null) {
        _plugin.keyDown(key, _sections[index]);
      }
    }
  }

  function _onResize() {
    _sectionTops = _sections.map(el => el.offsetTop);
    _currentIndex = null;
  }

  return {
    init,
    destroy
  };
})();
