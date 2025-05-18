const StickyHeader = (function () {
  let _stickyHeaderElement;
  let _mainContentElement;
  let _headerHeight;
  let _observer;

  function _adjustMainContent() {
    _headerHeight = _stickyHeaderElement.offsetHeight;
    _mainContentElement.style.paddingTop = `${_headerHeight}px`;
  }

  async function adjust(stickyHeaderElement, mainContentElement) {
    _stickyHeaderElement = stickyHeaderElement;
    _mainContentElement = mainContentElement;
    _observer = new ResizeObserver(() => {
      _adjustMainContent();
    });
    _observer.observe(_stickyHeaderElement);
    _adjustMainContent();

    // Wait for DOM to be ready
    await new Promise(requestAnimationFrame);
  }

  function stop() {
    if (_observer != null) {
      _observer.disconnect();
    }
  }

  return {
    adjust,
    stop,
    getHeaderHeight: () => _headerHeight,
  };
})();
