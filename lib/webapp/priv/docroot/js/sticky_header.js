const StickyHeader = (function () {
  let _stickyHeaderElement;
  let _mainContentElement;
  let _observer;

  function _adjustMainContent() {
    const headerHeight = _stickyHeaderElement.offsetHeight;
    _mainContentElement.style.paddingTop = `${headerHeight}px`;
  }

  function adjust(stickyHeaderElement, mainContentElement) {
    _stickyHeaderElement = stickyHeaderElement;
    _mainContentElement = mainContentElement;
    _observer = new ResizeObserver(() => {
      _adjustMainContent();
    });
    _observer.observe(_stickyHeaderElement);
    _adjustMainContent();
  }

  function stop() {
    if (_observer != null) {
      _observer.disconnect();
    }
  }

  return {
    adjust,
    stop
  };
})();
