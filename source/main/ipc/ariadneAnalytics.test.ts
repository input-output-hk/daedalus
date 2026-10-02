/** @jest-environment node */
import { EventEmitter } from 'events';
import { BrowserWindow, app, ipcMain } from 'electron';
import ElectronStore from 'electron-store';
import {
  ARIADNE_CONSENT_VERSION,
  ConsentView,
} from '../../common/analytics/contract';
import { createAriadneAnalyticsRegistration } from './ariadneAnalytics';
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
  const registration = createAriadneAnalyticsRegistration();
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
    registration.register(
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

test('replacement windows reuse the owner and old close cannot remove new handlers', () => {
  const registration = createAriadneAnalyticsRegistration();
  (ElectronStore as unknown as jest.Mock).mockClear();
  const first = Object.assign(new EventEmitter(), {
    webContents: { mainFrame: {}, isDestroyed: () => false },
  });
  const second = Object.assign(new EventEmitter(), {
    webContents: { mainFrame: {}, isDestroyed: () => false },
  });
  registration.register(
    first as unknown as BrowserWindow,
    'http://127.0.0.1:8080/'
  );
  (ipcMain.removeHandler as jest.Mock).mockClear();
  registration.register(
    second as unknown as BrowserWindow,
    'http://127.0.0.1:8080/'
  );
  expect(ipcMain.removeHandler).toHaveBeenCalledTimes(2);
  expect(ElectronStore).toHaveBeenCalledTimes(1);
  first.emit('closed');
  expect(ipcMain.removeHandler).toHaveBeenCalledTimes(2);
  second.emit('closed');
  expect(ipcMain.removeHandler).toHaveBeenCalledTimes(4);
});

test('recovery preserves admitted events and consent, rejects the previous sender and still aborts on final close', async () => {
  const original = { ...process.env };
  process.env.DAEDALUS_ARIADNE_ANALYTICS_ENABLED = 'true';
  process.env.DAEDALUS_ARIADNE_ANALYTICS_URL =
    'http://127.0.0.1:3000/api/analytics/event';
  process.env.NODE_ENV = 'development';
  process.env.DAEDALUS_ARIADNE_ALLOW_LOOPBACK_HTTP = 'true';
  const makeWindow = () => {
    const frame = { url: 'http://127.0.0.1:8080/' };
    return Object.assign(new EventEmitter(), {
      webContents: { mainFrame: frame, isDestroyed: () => false },
    });
  };
  const first = makeWindow();
  const second = makeWindow();
  const registration = createAriadneAnalyticsRegistration();
  const caller = (window: typeof first) => ({
    sender: window.webContents,
    senderFrame: window.webContents.mainFrame,
  });
  const handlers = () =>
    new Map<string, (event: unknown, payload: unknown) => unknown>(
      (ipcMain.handle as jest.Mock).mock.calls
    );
  try {
    registration.register(
      first as unknown as BrowserWindow,
      'http://127.0.0.1:8080/'
    );
    const consent = handlers().get(ARIADNE_ANALYTICS_CONSENT);
    const oldSend = handlers().get(ARIADNE_ANALYTICS_EVENT);
    const view = consent(caller(first), {
      version: ARIADNE_CONSENT_VERSION,
      status: 'ACCEPTED',
    }) as ConsentView;
    const payload = {
      generation: view.generation,
      type: 'page_view',
      action: 'Wallet Summary',
      uses_legacy_wallet: false,
      uses_hardware_wallet: false,
      ts: new Date().toISOString(),
    };
    expect(
      handlers().get(ARIADNE_ANALYTICS_EVENT)(caller(first), payload)
    ).toBe(true);
    expect(
      handlers().get(ARIADNE_ANALYTICS_EVENT)(caller(first), payload)
    ).toBe(true);
    registration.register(
      second as unknown as BrowserWindow,
      'http://127.0.0.1:8080/'
    );
    expect(
      consent(caller(first), {
        version: ARIADNE_CONSENT_VERSION,
        status: 'REJECTED',
      })
    ).toBeNull();
    expect(oldSend(caller(first), payload)).toBe(false);
    first.emit('closed');
    expect(
      handlers().get(ARIADNE_ANALYTICS_CONSENT)(caller(second), { get: true })
    ).toEqual(view);
    expect(
      handlers().get(ARIADNE_ANALYTICS_EVENT)(caller(first), payload)
    ).toBe(false);
    for (let i = 0; i < 20; i++) await Promise.resolve();
    expect(postAnalytics).toHaveBeenCalledTimes(2);
    expect(
      handlers().get(ARIADNE_ANALYTICS_EVENT)(caller(second), payload)
    ).toBe(true);
    second.emit('closed');
    expect(() =>
      registration.register(
        second as unknown as BrowserWindow,
        'http://127.0.0.1:8080/'
      )
    ).toThrow('Analytics registration is closed');
    for (let i = 0; i < 20; i++) await Promise.resolve();
    expect(postAnalytics).toHaveBeenCalledTimes(2);
  } finally {
    registration.dispose();
    process.env = original;
  }
});
