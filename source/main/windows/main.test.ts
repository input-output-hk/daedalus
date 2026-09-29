/** @jest-environment node */
import { EventEmitter } from 'events';
import { app, BrowserWindow } from 'electron';
import { createMainWindow } from './main';

jest.mock('electron', () => {
  const { EventEmitter: Emitter } = require('events');
  return {
    app: { quit: jest.fn() },
    ipcMain: { on: jest.fn() },
    BrowserWindow: jest.fn().mockImplementation(() =>
      Object.assign(new Emitter(), {
        webContents: new Emitter(),
        setMinimumSize: jest.fn(),
        loadURL: jest.fn(),
        setTitle: jest.fn(),
        setBounds: jest.fn(),
      })
    ),
  };
});
jest.mock('../environment', () => ({
  environment: { isDev: true, network: 'development' },
}));
jest.mock('../ipc', () => jest.fn());
jest.mock('../utils/logging', () => ({
  logger: { info: jest.fn(), error: jest.fn() },
}));
jest.mock('../utils/getContentMinimumSize', () => ({
  getContentMinimumSize: () => ({ minWindowsWidth: 1, minWindowsHeight: 1 }),
}));
jest.mock('../config', () => ({
  buildLabel: 'test',
  stateDirectoryPath: '/test',
}));
jest.mock('../ipc/getHardwareWalletChannel', () => ({
  ledgerStatus: { listening: false },
}));
jest.mock('../utils/rtsFlagsSettings', () => ({
  getRtsFlagsSettings: () => [],
}));

test('recovery transfers application ownership once and ignores old window errors/close', () => {
  const registration = { register: jest.fn(), dispose: jest.fn() };
  const onCreated = jest.fn();
  const bounds = () => ({ x: 0, y: 0, width: 100, height: 100 });
  const first = createMainWindow('en-US', bounds, registration, onCreated);
  first.webContents.emit('render-process-gone', {}, { reason: 'crashed' });
  expect(BrowserWindow).toHaveBeenCalledTimes(2);
  const second = (BrowserWindow as unknown as jest.Mock).mock.results[1]
    .value as BrowserWindow;
  expect(onCreated.mock.calls.map(([window]) => window)).toEqual([
    first,
    second,
  ]);
  expect(registration.register.mock.calls.map(([window]) => window)).toEqual([
    first,
    second,
  ]);
  first.webContents.emit('did-fail-load', {});
  (first as unknown as EventEmitter).emit('closed');
  expect(BrowserWindow).toHaveBeenCalledTimes(2);
  expect(app.quit).not.toHaveBeenCalled();
  second.emit('ready-to-show');
  expect(second.setBounds).toHaveBeenCalledWith(bounds());
  second.emit('closed');
  expect(app.quit).toHaveBeenCalledTimes(1);
});
