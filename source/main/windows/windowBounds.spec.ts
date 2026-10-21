/** @jest-environment node */

import { EventEmitter } from 'events';
import { saveWindowBoundsOnSizeAndPositionChange } from './windowBounds';

// Jest excludes lodash-es from transformation; exercise the same real debounce.
jest.mock('lodash-es/function', () => ({
  debounce: jest.requireActual('lodash/debounce'),
}));

const createWindow = () => {
  let destroyed = false;
  let bounds = { x: 10, y: 20, width: 800, height: 600 };
  const window = Object.assign(new EventEmitter(), {
    isDestroyed: () => destroyed,
    getBounds: jest.fn(() => {
      if (destroyed) throw new TypeError('Object has been destroyed');
      return bounds;
    }),
  });
  return {
    window,
    move: (next: typeof bounds) => {
      bounds = next;
      window.emit('move');
    },
    destroy: (emitClosed = true) => {
      destroyed = true;
      if (emitClosed) window.emit('closed');
    },
  };
};

describe('window bounds persistence lifecycle', () => {
  beforeEach(() => jest.useFakeTimers());
  afterEach(() => {
    jest.clearAllTimers();
    jest.useRealTimers();
  });

  it('persists the final live bounds after movement settles', () => {
    const { window, move } = createWindow();
    const persist = jest.fn();
    saveWindowBoundsOnSizeAndPositionChange(window, persist);
    move({ x: 20, y: 30, width: 800, height: 600 });
    jest.advanceTimersByTime(500);
    const finalBounds = { x: 40, y: 50, width: 900, height: 700 };
    move(finalBounds);
    jest.advanceTimersByTime(999);
    expect(persist).not.toHaveBeenCalled();
    jest.advanceTimersByTime(1);
    expect(persist).toHaveBeenCalledTimes(1);
    expect(persist.mock.calls[0][0].data).toEqual(finalBounds);
  });

  it.each([true, false])(
    'never reads or persists destroyed bounds (closed event: %s)',
    (emitClosed) => {
      const { window, move, destroy } = createWindow();
      const persist = jest.fn();
      saveWindowBoundsOnSizeAndPositionChange(window, persist);
      move({ x: 20, y: 30, width: 800, height: 600 });
      destroy(emitClosed);
      jest.advanceTimersByTime(1000);
      expect(window.getBounds).not.toHaveBeenCalled();
      expect(persist).not.toHaveBeenCalled();
    }
  );
});
