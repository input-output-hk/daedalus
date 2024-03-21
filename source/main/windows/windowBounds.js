'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.saveWindowBoundsOnSizeAndPositionChange = exports.restoreSavedWindowBounds = void 0;
const lodash_1 = require('lodash');
const electron_store_config_1 = require('../../common/config/electron-store.config');
function maybeRectangle(data) {
  return data != null &&
    typeof data.x === 'number' &&
    typeof data.y === 'number' &&
    typeof data.width === 'number' &&
    typeof data.height === 'number'
    ? data
    : null;
}
function getRightEdge(rect) {
  return rect.x + rect.width;
}
function getBottomEdge(rect) {
  return rect.y + rect.height;
}
function isWithinBounds(rect, bounds) {
  return (
    rect.x >= bounds.x &&
    rect.y >= bounds.y &&
    getRightEdge(rect) <= getRightEdge(bounds) &&
    getBottomEdge(rect) <= getBottomEdge(bounds)
  );
}
function isFittingIntoBounds(rect, bounds) {
  return rect.width <= bounds.width && rect.height <= bounds.height;
}
function getCenteredRectInBounds(rect, bounds) {
  return {
    width: rect.width,
    height: rect.height,
    x: bounds.x + (bounds.width - rect.width) / 2,
    y: bounds.y + (bounds.height - rect.height) / 2,
  };
}
/**
 * Fetches the saved window bounds from provided store and
 * applies our business rules regarding window management.
 */
function restoreSavedWindowBounds(screen, sendStoreRequest) {
  const savedBounds = maybeRectangle(
    sendStoreRequest({
      type: electron_store_config_1.STORAGE_TYPES.GET,
      key: electron_store_config_1.STORAGE_KEYS.WINDOW_BOUNDS,
    })
  );
  if (!savedBounds) return null;
  const closestDisplay = screen.getDisplayMatching(savedBounds);
  const displayBounds = closestDisplay.workArea;
  if (isWithinBounds(savedBounds, displayBounds)) {
    // scenario 1: Size and position of Daedalus are persisted
    return savedBounds;
  }
  // scenario 2: Size is persisted and position changes (center window)
  if (isFittingIntoBounds(savedBounds, displayBounds)) {
    return getCenteredRectInBounds(savedBounds, displayBounds);
  }
  // scenario 3: Size and position change (fit to screen)
  return displayBounds;
}
exports.restoreSavedWindowBounds = restoreSavedWindowBounds;
function saveWindowBoundsOnSizeAndPositionChange(
  window,
  sendStoreRequest,
  debounceWait = 1000
) {
  const saveWindowBoundsSoon = (0, lodash_1.debounce)(
    () =>
      sendStoreRequest({
        type: electron_store_config_1.STORAGE_TYPES.SET,
        key: electron_store_config_1.STORAGE_KEYS.WINDOW_BOUNDS,
        data: window.getBounds(),
      }),
    debounceWait
  );
  window.on('resize', saveWindowBoundsSoon);
  window.on('move', saveWindowBoundsSoon);
}
exports.saveWindowBoundsOnSizeAndPositionChange = saveWindowBoundsOnSizeAndPositionChange;
//# sourceMappingURL=windowBounds.js.map
