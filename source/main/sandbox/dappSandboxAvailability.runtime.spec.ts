/** @jest-environment node */

import { EventEmitter } from 'events';
import type { ProcessMetric } from 'electron';
import type * as Fs from 'fs';
import type * as DappSandboxAvailability from './dappSandboxAvailability';

const darwinRoot = '/Applications/Daedalus Preview.app';
const darwinExecutable = `${darwinRoot}/Contents/MacOS/Frontend`;
const darwinResources = `${darwinRoot}/Contents/Resources`;
const validMetric = {
  creationTime: 100.25,
  pid: 20,
  sandboxed: true,
  type: 'Tab',
} as ProcessMetric;

const processDescriptors = {
  platform: Object.getOwnPropertyDescriptor(process, 'platform'),
  arch: Object.getOwnPropertyDescriptor(process, 'arch'),
  execPath: Object.getOwnPropertyDescriptor(process, 'execPath'),
  resourcesPath: Object.getOwnPropertyDescriptor(process, 'resourcesPath'),
};
const launcherConfig = process.env.LAUNCHER_CONFIG;

type RuntimeOptions = {
  metrics?: readonly (readonly ProcessMetric[])[];
  bypass?: boolean;
  cleanupFailure?: boolean;
  packaged?: boolean;
  symlinkPackage?: boolean;
};

const setProcessValue = (name: string, value: string): void => {
  Object.defineProperty(process, name, { configurable: true, value });
};

const loadRuntime = (options: RuntimeOptions = {}) => {
  jest.resetModules();
  setProcessValue('platform', 'darwin');
  setProcessValue('arch', 'arm64');
  setProcessValue('execPath', darwinExecutable);
  setProcessValue('resourcesPath', darwinResources);
  process.env.LAUNCHER_CONFIG = `${darwinResources}/launcher-config.yaml`;

  const canarySession = {
    isPersistent: jest.fn(() => false),
    getStoragePath: jest.fn(() => null),
    clearStorageData: jest.fn(() => Promise.resolve()),
    clearCache: jest.fn(() =>
      options.cleanupFailure
        ? Promise.reject(new Error('cleanup failed'))
        : Promise.resolve()
    ),
    clearAuthCache: jest.fn(() => Promise.resolve()),
    clearHostResolverCache: jest.fn(() => Promise.resolve()),
    closeAllConnections: jest.fn(() => Promise.resolve()),
  };
  const webContents = Object.assign(new EventEmitter(), {
    setWindowOpenHandler: jest.fn(),
    getOSProcessId: jest.fn(() => 20),
    isDestroyed: jest.fn(() => false),
  });
  const window = Object.assign(new EventEmitter(), {
    webContents,
    loadURL: jest.fn(() => Promise.resolve()),
    isDestroyed: jest.fn(() => false),
    destroy: jest.fn(),
  });
  const BrowserWindow = jest.fn(() => window);
  const snapshots = options.metrics || [[validMetric], [validMetric]];
  let metricCall = 0;
  const app = {
    commandLine: { hasSwitch: jest.fn(() => options.bypass === true) },
    getAppMetrics: jest.fn(
      () => snapshots[Math.min(metricCall++, snapshots.length - 1)]
    ),
    getAppPath: jest.fn(() => `${darwinResources}/app`),
    getName: jest.fn(() => 'Daedalus Preview'),
    isPackaged: options.packaged === true,
  };
  jest.doMock('electron', () => ({
    app,
    BrowserWindow,
    session: { fromPartition: jest.fn(() => canarySession) },
  }));

  const actualFs = jest.requireActual<typeof Fs>('fs');
  jest.doMock('fs', () => ({
    ...actualFs,
    lstatSync: jest.fn((entryPath: string) => ({
      isDirectory: () => !entryPath.endsWith('Frontend'),
      isFile: () => entryPath.endsWith('Frontend'),
      isSymbolicLink: () => options.symlinkPackage === true,
    })),
    realpathSync: jest.fn((entryPath: string) => entryPath),
  }));

  let runtime!: typeof DappSandboxAvailability;
  jest.isolateModules(() => {
    runtime = require('./dappSandboxAvailability');
  });
  return { app, BrowserWindow, runtime };
};

const restoreProcessValue = (name: keyof typeof processDescriptors): void => {
  const descriptor = processDescriptors[name];
  if (descriptor) Object.defineProperty(process, name, descriptor);
  else delete ((process as unknown) as Record<string, unknown>)[name];
};

afterEach(() => {
  restoreProcessValue('platform');
  restoreProcessValue('arch');
  restoreProcessValue('execPath');
  restoreProcessValue('resourcesPath');
  if (launcherConfig === undefined) delete process.env.LAUNCHER_CONFIG;
  else process.env.LAUNCHER_CONFIG = launcherConfig;
  jest.resetModules();
  jest.restoreAllMocks();
  jest.dontMock('electron');
  jest.dontMock('fs');
});

describe('Darwin dApp sandbox runtime availability', () => {
  test('stable native evidence reaches available', async () => {
    const { runtime } = loadRuntime();
    await expect(
      runtime.startDappSandboxAvailabilityCheck({
        cluster: 'preview',
        isDevelopment: true,
      })
    ).resolves.toEqual({ status: 'available' });
    await expect(
      runtime.requireDappSandboxAvailable()
    ).resolves.toBeUndefined();
  });

  test.each([
    [
      'changed creation time',
      [[validMetric], [{ ...validMetric, creationTime: 101 }]],
    ],
    ['missing final metric', [[validMetric], []]],
  ])('%s fails closed and remains cached', async (_name, metrics) => {
    const { app, BrowserWindow, runtime } = loadRuntime({
      metrics: metrics as ProcessMetric[][],
    });
    const options = { cluster: 'preview', isDevelopment: true };
    const first = await runtime.startDappSandboxAvailabilityCheck(options);
    expect(first).toEqual({ status: 'unavailable', reason: 'canary-failed' });
    app.getAppMetrics.mockReturnValue([validMetric]);
    await expect(
      runtime.startDappSandboxAvailabilityCheck(options)
    ).resolves.toBe(first);
    expect(BrowserWindow).toHaveBeenCalledTimes(1);
  });

  test.each([
    {
      name: 'package path',
      installRoot: `${darwinRoot}/Contents`,
      symlinkPackage: false,
    },
    {
      name: 'package symlink',
      installRoot: darwinRoot,
      symlinkPackage: true,
    },
  ])('$name evidence refuses before canary creation', async (fixture) => {
    const { BrowserWindow, runtime } = loadRuntime({
      packaged: true,
      symlinkPackage: fixture.symlinkPackage,
    });
    await expect(
      runtime.startDappSandboxAvailabilityCheck({
        cluster: 'preview',
        installRoot: fixture.installRoot,
        isDevelopment: false,
      })
    ).resolves.toEqual({
      status: 'unavailable',
      reason: 'unsupported-package',
    });
    expect(BrowserWindow).not.toHaveBeenCalled();
  });

  test('sandbox bypass refuses without creating a canary', async () => {
    const { BrowserWindow, runtime } = loadRuntime({ bypass: true });
    await expect(
      runtime.startDappSandboxAvailabilityCheck({
        cluster: 'preview',
        isDevelopment: true,
      })
    ).resolves.toEqual({ status: 'unavailable', reason: 'sandbox-bypass' });
    expect(BrowserWindow).not.toHaveBeenCalled();
  });

  test('cleanup rejection overrides successful evidence', async () => {
    const { runtime } = loadRuntime({ cleanupFailure: true });
    await expect(
      runtime.startDappSandboxAvailabilityCheck({
        cluster: 'preview',
        isDevelopment: true,
      })
    ).resolves.toEqual({ status: 'unavailable', reason: 'cleanup-failed' });
  });
});
