// @flow
import { BrowserWindow } from 'electron';
import {
  MIN_WINDOW_CONTENT_WIDTH,
  MIN_WINDOW_CONTENT_HEIGHT,
  WINDOW_WIDTH,
  WINDOW_HEIGHT,
} from '../config';

type getContentMinimumSizeResponse = {
  minWindowsWidth: number,
  minWindowsHeight: number,
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
  const { width: frameWidth, height: frameHeight } = window.getBounds();
  const {
    width: contentWidth,
    height: contentHeight,
  } = window.getContentBounds();
  const paddingWidth = frameWidth - contentWidth || 0;
  const paddingHeight = frameHeight - contentHeight || 0;
  const minWindowsWidth = MIN_WINDOW_CONTENT_WIDTH + paddingWidth;
  const minWindowsHeight = MIN_WINDOW_CONTENT_HEIGHT + paddingHeight;

  // Set back the window size to the desired dimensions
  window.setSize(WINDOW_WIDTH, WINDOW_HEIGHT);

  return {
    minWindowsWidth,
    minWindowsHeight,
  };
};
