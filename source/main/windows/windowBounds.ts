import { debounce } from 'lodash-es/function';
import {
  STORAGE_KEYS,
  STORAGE_TYPES,
} from '../../common/config/electron-store.config';
import type { StoreMessage } from '../../common/types/electron-store.types';

interface Rectangle {
  x: number;
  y: number;
  width: number;
  height: number;
}
interface Window {
  getBounds(): Rectangle;
  on(event: string, handler: (...args: Array<any>) => any): void;
}
interface Display {
  workArea: Rectangle;
}
interface Screen {
  getDisplayMatching(rect: Rectangle): Display;
  workArea: Rectangle;
}
type SendStoreRequest = (request: StoreMessage) => unknown;

function maybeRectangle(data: any): Rectangle | null | undefined {
  return data != null &&
    typeof data.x === 'number' &&
    typeof data.y === 'number' &&
    typeof data.width === 'number' &&
    typeof data.height === 'number'
    ? (data as Rectangle)
    : null;
}

function getRightEdge(rect: Rectangle) {
  return rect.x + rect.width;
}

function getBottomEdge(rect: Rectangle) {
  return rect.y + rect.height;
}

function isWithinBounds(rect: Rectangle, bounds: Rectangle): boolean {
  return (
    rect.x >= bounds.x &&
    rect.y >= bounds.y &&
    getRightEdge(rect) <= getRightEdge(bounds) &&
    getBottomEdge(rect) <= getBottomEdge(bounds)
  );
}

function isFittingIntoBounds(rect: Rectangle, bounds: Rectangle): boolean {
  return rect.width <= bounds.width && rect.height <= bounds.height;
}

function getCenteredRectInBounds(
  rect: Rectangle,
  bounds: Rectangle
): Rectangle {
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
export function restoreSavedWindowBounds(
  screen: Screen,
  sendStoreRequest: SendStoreRequest
): Rectangle | null | undefined {
  const savedBounds = maybeRectangle(
    sendStoreRequest({
      type: STORAGE_TYPES.GET,
      key: STORAGE_KEYS.WINDOW_BOUNDS,
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
export function saveWindowBoundsOnSizeAndPositionChange(
  window: Window,
  sendStoreRequest: SendStoreRequest,
  debounceWait = 1000
) {
  const saveWindowBoundsSoon = debounce(
    () =>
      sendStoreRequest({
        type: STORAGE_TYPES.SET,
        key: STORAGE_KEYS.WINDOW_BOUNDS,
        data: window.getBounds(),
      }),
    debounceWait
  );
  window.on('resize', saveWindowBoundsSoon);
  window.on('move', saveWindowBoundsSoon);
}
