import { BrowserWindow } from 'electron';
import { environment } from '../environment';
import { MIN_WINDOW_CONTENT_WIDTH, MIN_WINDOW_CONTENT_HEIGHT } from '../config';

type getContentMinimumSizeResponse = {
  minWindowsWidth: number;
  minWindowsHeight: number;
};

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
export const getContentMinimumSize = (
  window: BrowserWindow
): getContentMinimumSizeResponse => {
  const { isWindows } = environment;
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

  const minWindowsWidth = MIN_WINDOW_CONTENT_WIDTH + paddingWidth;
  const minWindowsHeight = MIN_WINDOW_CONTENT_HEIGHT + paddingHeight;
  return {
    minWindowsWidth,
    minWindowsHeight,
  };
};
