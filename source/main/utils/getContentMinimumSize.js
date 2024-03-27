'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getContentMinimumSize = void 0;
const environment_1 = require('../environment');
const config_1 = require('../config');
/**
 *
 * getContentMinimumSize
 * ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
 * This function is necessary because Electron's function
 * `setMinimumSize` takes the native OS menu into account.
 * By setting `useContentSize` in the BrowserWindow's options,
 * it's possible to grab the menu & frame sizes,
 * then add to the minimum `width` and `height`
 *
 */
const getContentMinimumSize = (window) => {
  const { isWindows } = environment_1.environment;
  const { width: frameWidth, height: frameHeight } = window.getBounds();
  const {
    width: contentWidth,
    height: contentHeight,
  } = window.getContentBounds();
  const paddingWidth = frameWidth - contentWidth || 0;
  let paddingHeight = frameHeight - contentHeight || 0;
  if (isWindows) {
    paddingHeight += 20;
  }
  const minWindowsWidth = config_1.MIN_WINDOW_CONTENT_WIDTH + paddingWidth;
  const minWindowsHeight = config_1.MIN_WINDOW_CONTENT_HEIGHT + paddingHeight;
  return {
    minWindowsWidth,
    minWindowsHeight,
  };
};
exports.getContentMinimumSize = getContentMinimumSize;
//# sourceMappingURL=getContentMinimumSize.js.map
