/** @jest-environment node */
import { EventEmitter } from 'events';
import { BrowserWindow, app, ipcMain } from 'electron';
import ElectronStore from 'electron-store';
import { ARIADNE_CONSENT_VERSION } from '../../common/analytics/contract';
import { registerAriadneAnalytics } from './ariadneAnalytics';
import { postAnalytics } from '../analytics/transport';
import {
  ARIADNE_ANALYTICS_CONSENT,
  ARIADNE_ANALYTICS_EVENT,
} from '../../common/ipc/api';

jest.mock('electron', () => ({
  app: Object.assign(new (require('events').EventEmitter)(), {
    isPackaged: false,
  }),
  ipcMain: { handle: jest.fn(), removeHandler: jest.fn() },
}));
jest.mock('electron-store', () =>
  jest.fn().mockImplementation(() => {
    const store = new Map();
    return {
      get: (key: string) => store.get(key),
      set: (key: string, value: unknown) => store.set(key, value),
    };
  })
);
jest.mock('../environment', () => ({
  environment: {
    platformVersion: '10.0.26100',
    ram: 16 * 1024 ** 3,
    cpu: [{ model: 'Intel(R) Core(TM) i7' }],
    version: '11.4.0',
    network: 'development',
  },
}));
jest.mock('../analytics/transport', () => ({
  postAnalytics: jest.fn(async () => ({ status: 204 })),
}));

test('registered handlers check caller and payload, return no UUID and cancel on shutdown', async () => {
  const original = { ...process.env };
  process.env.DAEDALUS_ARIADNE_ANALYTICS_ENABLED = 'true';
  process.env.DAEDALUS_ARIADNE_ANALYTICS_URL =
    'http://127.0.0.1:3000/api/analytics/event';
  process.env.NODE_ENV = 'development';
  process.env.DAEDALUS_ARIADNE_ALLOW_LOOPBACK_HTTP = 'true';
  const frame = { url: 'http://127.0.0.1:8080/#/wallets' };
  const contents = { mainFrame: frame, isDestroyed: () => false };
  const window = Object.assign(new EventEmitter(), { webContents: contents });
  try {
    registerAriadneAnalytics(
      window as unknown as BrowserWindow,
      'http://127.0.0.1:8080/'
    );
    expect(ElectronStore).toHaveBeenCalledWith({ name: 'ariadne-analytics' });
    const handlers = new Map((ipcMain.handle as jest.Mock).mock.calls);
    const consent = handlers.get(ARIADNE_ANALYTICS_CONSENT) as (
      event: unknown,
      command: unknown
    ) => { generation: number };
    const send = handlers.get(ARIADNE_ANALYTICS_EVENT) as (
      event: unknown,
      message: unknown
    ) => boolean;
    const event = { sender: contents, senderFrame: frame };
    expect(
      consent(
        { ...event, senderFrame: {} },
        { version: ARIADNE_CONSENT_VERSION, status: 'ACCEPTED' }
      )
    ).toBeNull();
    expect(consent(event, { get: true })).toMatchObject({ status: 'PENDING' });
    const view = consent(event, {
      version: ARIADNE_CONSENT_VERSION,
      status: 'ACCEPTED',
    });
    expect(Object.keys(view).sort()).toEqual([
      'enabled',
      'generation',
      'status',
      'version',
    ]);
    const payload = {
      generation: view.generation,
      type: 'page_view',
      action: 'Wallet Summary',
      uses_legacy_wallet: false,
      uses_hardware_wallet: false,
      ts: new Date().toISOString(),
    };
    expect(
      send(event, { ...payload, endpoint: 'https://example.invalid' })
    ).toBe(false);
    expect(send({ ...event, sender: {} }, payload)).toBe(false);
    expect(send(event, payload)).toBe(true);
    app.emit('before-quit');
    await Promise.resolve();
    await Promise.resolve();
    expect(postAnalytics).not.toHaveBeenCalled();
  } finally {
    window.emit('closed');
    process.env = original;
  }
  expect(ipcMain.removeHandler).toHaveBeenCalledWith(ARIADNE_ANALYTICS_EVENT);
});

test('replacement windows dispose the old owner and old close cannot remove new handlers', () => {
  const first = Object.assign(new EventEmitter(), {
    webContents: { mainFrame: {}, isDestroyed: () => false },
  });
  const second = Object.assign(new EventEmitter(), {
    webContents: { mainFrame: {}, isDestroyed: () => false },
  });
  registerAriadneAnalytics(
    first as unknown as BrowserWindow,
    'http://127.0.0.1:8080/'
  );
  (ipcMain.removeHandler as jest.Mock).mockClear();
  registerAriadneAnalytics(
    second as unknown as BrowserWindow,
    'http://127.0.0.1:8080/'
  );
  expect(ipcMain.removeHandler).toHaveBeenCalledTimes(2);
  first.emit('closed');
  expect(ipcMain.removeHandler).toHaveBeenCalledTimes(2);
  second.emit('closed');
  expect(ipcMain.removeHandler).toHaveBeenCalledTimes(4);
});
