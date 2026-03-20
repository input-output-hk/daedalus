import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import MithrilBootstrapStore from './MithrilBootstrapStore';
import { noopAnalyticsTracker } from '../analytics';

const mockSetChainStorageDirectoryRequest = jest.fn();
const mockGetChainStorageDirectoryRequest = jest.fn();
const mockValidateChainStorageDirectoryRequest = jest.fn();

jest.mock('../ipc/chainStorageChannel', () => ({
  setChainStorageDirectoryChannel: {
    request: (...args) => mockSetChainStorageDirectoryRequest(...args),
  },
  getChainStorageDirectoryChannel: {
    request: (...args) => mockGetChainStorageDirectoryRequest(...args),
  },
  validateChainStorageDirectoryChannel: {
    request: (...args) => mockValidateChainStorageDirectoryRequest(...args),
  },
}));

describe('MithrilBootstrapStore', () => {
  const api: Api = ({
    ada: jest.fn(),
  } as unknown) as Api;
  const actions: ActionsMap = (jest.fn() as unknown) as ActionsMap;

  const setupStore = () =>
    new MithrilBootstrapStore(api, actions, noopAnalyticsTracker);

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('derives bytes downloaded from file counts and snapshot size', async () => {
    const store = setupStore();

    await store._updateStatus({
      status: 'downloading',
      snapshot: {
        digest: 'snapshot-1',
        createdAt: '2026-03-06T12:00:00.000Z',
        size: 800,
      },
      filesDownloaded: 1,
      filesTotal: 4,
      elapsedSeconds: 5,
    });

    expect(store.bytesDownloaded).toBe(200);
  });

  it('caps derived bytes when file counts exceed the reported total', async () => {
    const store = setupStore();

    await store._updateStatus({
      status: 'downloading',
      snapshot: {
        digest: 'snapshot-2',
        createdAt: '2026-03-06T12:00:00.000Z',
        size: 1200,
      },
      filesDownloaded: 8,
      filesTotal: 4,
      elapsedSeconds: 4,
    });

    expect(store.bytesDownloaded).toBe(1200);
  });

  it('clears transient progress metadata when download transitions into unpacking', async () => {
    const store = setupStore();

    await store._updateStatus({
      status: 'downloading',
      snapshot: {
        digest: 'snapshot-3',
        createdAt: '2026-03-06T12:00:00.000Z',
        size: 1000,
      },
      filesDownloaded: 2,
      filesTotal: 4,
      elapsedSeconds: 10,
    });

    await store._updateStatus({
      status: 'unpacking',
      filesDownloaded: undefined,
      filesTotal: undefined,
      elapsedSeconds: undefined,
      error: null,
    });

    expect(store.status).toBe('unpacking');
    expect(store.filesDownloaded).toBeUndefined();
    expect(store.filesTotal).toBeUndefined();
    expect(store.elapsedSeconds).toBeUndefined();
    expect(store.bytesDownloaded).toBeUndefined();
  });

  it('loads chain storage config and tracks selected custom path', async () => {
    const store = setupStore();
    mockGetChainStorageDirectoryRequest.mockResolvedValue({
      customPath: '/mnt/external/daedalus-chain',
      defaultPath: '/tmp/state/chain',
      availableSpaceBytes: 123456,
      requiredSpaceBytes: 654321,
      setAt: '2026-03-09T00:00:00.000Z',
    });
    mockValidateChainStorageDirectoryRequest.mockResolvedValue({
      isValid: true,
      path: '/mnt/external/daedalus-chain',
      resolvedPath: '/mnt/external/daedalus-chain',
    });

    await store.loadChainStorageConfig();

    expect(store.customChainPath).toBe('/mnt/external/daedalus-chain');
    expect(store.defaultChainPath).toBe('/tmp/state/chain');
    expect(store.defaultChainStorageValidation).toEqual({
      isValid: true,
      path: null,
      resolvedPath: '/tmp/state/chain',
      availableSpaceBytes: 123456,
      requiredSpaceBytes: 654321,
    });
    expect(store.chainStorageValidation).toEqual({
      isValid: true,
      path: '/mnt/external/daedalus-chain',
      resolvedPath: '/mnt/external/daedalus-chain',
    });
    expect(store.isChainStorageLoading).toBe(false);
  });

  it('surfaces invalid startup custom path validation through store state', async () => {
    const store = setupStore();
    mockGetChainStorageDirectoryRequest.mockResolvedValue({
      customPath: '/mnt/missing-chain',
      defaultPath: '/tmp/state/chain',
    });
    mockValidateChainStorageDirectoryRequest.mockResolvedValue({
      isValid: false,
      path: '/mnt/missing-chain',
      reason: 'path-not-found',
      message: 'Configured chain storage target is unavailable.',
    });

    await store.loadChainStorageConfig();

    expect(mockValidateChainStorageDirectoryRequest).toHaveBeenCalledWith({
      path: '/mnt/missing-chain',
    });
    expect(store.defaultChainPath).toBe('/tmp/state/chain');
    expect(store.chainStorageValidation).toEqual({
      isValid: false,
      path: '/mnt/missing-chain',
      reason: 'path-not-found',
      message: 'Configured chain storage target is unavailable.',
    });
  });

  it('loads default chain storage metadata when no custom path is configured', async () => {
    const store = setupStore();
    mockGetChainStorageDirectoryRequest.mockResolvedValue({
      customPath: null,
      defaultPath: '/tmp/state/chain',
      availableSpaceBytes: 987654321,
      requiredSpaceBytes: 654321,
    });

    await store.loadChainStorageConfig();

    expect(mockValidateChainStorageDirectoryRequest).not.toHaveBeenCalled();
    expect(store.customChainPath).toBeNull();
    expect(store.defaultChainPath).toBe('/tmp/state/chain');
    expect(store.defaultChainStorageValidation).toEqual({
      isValid: true,
      path: null,
      resolvedPath: '/tmp/state/chain',
      availableSpaceBytes: 987654321,
      requiredSpaceBytes: 654321,
    });
    expect(store.chainStorageValidation).toEqual({
      isValid: true,
      path: null,
      resolvedPath: '/tmp/state/chain',
      availableSpaceBytes: 987654321,
      requiredSpaceBytes: 654321,
    });
  });

  it('sets custom chain storage path when validation succeeds', async () => {
    const store = setupStore();
    mockSetChainStorageDirectoryRequest.mockResolvedValue({
      isValid: true,
      path: '/mnt/custom-chain',
      resolvedPath: '/mnt/custom-chain',
    });

    const result = await store.setChainStorageDirectory('/mnt/custom-chain');

    expect(mockSetChainStorageDirectoryRequest).toHaveBeenCalledWith({
      path: '/mnt/custom-chain',
    });
    expect(result).toEqual({
      isValid: true,
      path: '/mnt/custom-chain',
      resolvedPath: '/mnt/custom-chain',
    });
    expect(store.customChainPath).toBe('/mnt/custom-chain');
    expect(store.chainStorageValidation).toEqual({
      isValid: true,
      path: '/mnt/custom-chain',
      resolvedPath: '/mnt/custom-chain',
    });
    expect(store.isChainStorageLoading).toBe(false);
  });

  it('keeps previous custom path when validation fails', async () => {
    const store = setupStore();
    store.customChainPath = '/mnt/current-chain';
    mockSetChainStorageDirectoryRequest.mockResolvedValue({
      isValid: false,
      path: '/mnt/invalid',
      reason: 'insufficient-space',
      message: 'Selected directory does not have enough free space.',
    });

    await store.setChainStorageDirectory('/mnt/invalid');

    expect(store.customChainPath).toBe('/mnt/current-chain');
    expect(store.chainStorageValidation).toEqual({
      isValid: false,
      path: '/mnt/invalid',
      reason: 'insufficient-space',
      message: 'Selected directory does not have enough free space.',
    });
  });

  it('resets chain storage to default through dedicated action', async () => {
    const store = setupStore();
    store.defaultChainPath = '/tmp/state/chain';
    mockSetChainStorageDirectoryRequest.mockResolvedValue({
      isValid: true,
      path: null,
      resolvedPath: '/tmp/state/chain',
      availableSpaceBytes: 456789,
      requiredSpaceBytes: 654321,
    });

    await store.resetChainStorageDirectory();

    expect(mockSetChainStorageDirectoryRequest).toHaveBeenCalledWith({
      path: null,
    });
    expect(store.customChainPath).toBeNull();
    expect(store.defaultChainPath).toBe('/tmp/state/chain');
    expect(store.chainStorageValidation).toEqual({
      isValid: true,
      path: null,
      resolvedPath: '/tmp/state/chain',
      availableSpaceBytes: 456789,
      requiredSpaceBytes: 654321,
    });
    expect(store.defaultChainStorageValidation).toEqual({
      isValid: true,
      path: null,
      resolvedPath: '/tmp/state/chain',
      availableSpaceBytes: 456789,
      requiredSpaceBytes: 654321,
    });
  });

  it('validates chain storage directory without mutating selected config', async () => {
    const store = setupStore();
    store.customChainPath = '/mnt/current-chain';
    mockValidateChainStorageDirectoryRequest.mockResolvedValue({
      isValid: true,
      path: '/mnt/new-chain',
      resolvedPath: '/mnt/new-chain',
    });

    const result = await store.validateChainStorageDirectory('/mnt/new-chain');

    expect(mockValidateChainStorageDirectoryRequest).toHaveBeenCalledWith({
      path: '/mnt/new-chain',
    });
    expect(result).toEqual({
      isValid: true,
      path: '/mnt/new-chain',
      resolvedPath: '/mnt/new-chain',
    });
    expect(store.customChainPath).toBe('/mnt/current-chain');
  });

  it('marks storage location as confirmed when requested', () => {
    const store = setupStore();

    expect(store.storageLocationConfirmed).toBe(false);
    store.confirmStorageLocation();
    expect(store.storageLocationConfirmed).toBe(true);
  });

  it('clears storage location confirmation when requested', () => {
    const store = setupStore();

    store.confirmStorageLocation();

    expect(store.storageLocationConfirmed).toBe(true);

    store.clearStorageLocationConfirmation();

    expect(store.storageLocationConfirmed).toBe(false);
  });

  it('resets storage location confirmation when re-entering a decision cycle', async () => {
    const store = setupStore();

    store.confirmStorageLocation();
    await store._updateStatus({
      status: 'preparing',
    });
    expect(store.storageLocationConfirmed).toBe(true);

    await store._updateStatus({
      status: 'decision',
    });

    expect(store.storageLocationConfirmed).toBe(false);
  });

  it('treats verifying as a working status for elapsed timer', async () => {
    const store = setupStore();

    await store._updateStatus({
      status: 'verifying',
    });

    expect(store.status).toBe('verifying');
    expect(store.bootstrapStartedAt).not.toBeNull();
    expect(typeof store.overallElapsedSeconds).toBe('number');
    expect(store.overallElapsedSeconds).toBeGreaterThanOrEqual(0);
  });
});
