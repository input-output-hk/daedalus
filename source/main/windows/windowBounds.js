// @flow
import { debounce } from 'lodash-es/function';

interface Rectangle {
  x: number;
  y: number;
  width: number;
  height: number;
}

interface Window {
  getBounds(): Rectangle;
  on(event: string, handler: Function): void;
}

interface Display {
  workArea: Rectangle;
}

interface Screen {
  getDisplayMatching(rect: Rectangle): Display;
  workArea: Rectangle;
}

interface Store {
  set: (key: string, value: Rectangle) => void;
  get: (key: string) => ?Rectangle;
}

const windowBoundsDefaultStoreKey = 'windowBounds';

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
  store: Store,
  storeKey: string = windowBoundsDefaultStoreKey
): ?Rectangle {
  const savedBounds = store.get(storeKey);
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
  store: Store,
  storeKey: string = windowBoundsDefaultStoreKey,
  debounceWait: number = 1000
) {
  const saveWindowBoundsSoon = debounce(
    () => store.set(storeKey, window.getBounds()),
    debounceWait
  );
  window.on('resize', saveWindowBoundsSoon);
  window.on('move', saveWindowBoundsSoon);
}
