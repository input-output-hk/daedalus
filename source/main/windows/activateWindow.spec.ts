/**
 * @jest-environment node
 */
import type { BrowserWindow } from 'electron';
import { activateWindow } from './activateWindow';

jest.mock('electron', () => ({
  app: { focus: jest.fn() },
}));

const { app } = jest.requireMock('electron');

type WindowState = {
  destroyed?: boolean;
  minimized?: boolean;
  visible?: boolean;
};

const makeWindow = ({
  destroyed = false,
  minimized = false,
  visible = true,
}: WindowState = {}) => {
  const calls: Array<string> = [];
  const window = {
    isDestroyed: jest.fn(() => destroyed),
    isMinimized: jest.fn(() => minimized),
    isVisible: jest.fn(() => visible),
    restore: jest.fn(() => calls.push('restore')),
    show: jest.fn(() => calls.push('show')),
    focus: jest.fn(() => calls.push('focus')),
  };
  return { window: window as unknown as BrowserWindow, mock: window, calls };
};

describe('activateWindow', () => {
  it('focuses a visible window', () => {
    const { window, mock } = makeWindow();
    expect(activateWindow(window, 'win32')).toBe(true);
    expect(mock.focus).toHaveBeenCalledTimes(1);
    expect(mock.restore).not.toHaveBeenCalled();
    expect(mock.show).not.toHaveBeenCalled();
  });

  it('restores a minimised window before focusing it', () => {
    const { window, calls } = makeWindow({ minimized: true });
    activateWindow(window, 'win32');
    expect(calls).toEqual(['restore', 'focus']);
  });

  it('shows a hidden window before focusing it', () => {
    const { window, calls } = makeWindow({ visible: false });
    activateWindow(window, 'linux');
    expect(calls).toEqual(['show', 'focus']);
  });

  it('takes application focus on macOS', () => {
    const { window } = makeWindow();
    activateWindow(window, 'darwin');
    expect(app.focus).toHaveBeenCalledWith({ steal: true });
  });

  it('leaves application focus alone on other platforms', () => {
    const { window } = makeWindow();
    activateWindow(window, 'win32');
    activateWindow(window, 'linux');
    expect(app.focus).not.toHaveBeenCalled();
  });

  it('ignores a destroyed window', () => {
    const { window, mock } = makeWindow({ destroyed: true });
    expect(activateWindow(window, 'win32')).toBe(false);
    expect(mock.focus).not.toHaveBeenCalled();
  });

  it('ignores a missing window', () => {
    expect(activateWindow(null, 'win32')).toBe(false);
    expect(activateWindow(undefined, 'darwin')).toBe(false);
    expect(app.focus).not.toHaveBeenCalled();
  });
});
