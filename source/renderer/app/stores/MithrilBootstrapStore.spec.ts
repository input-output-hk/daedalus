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

  it('derives bytes downloaded and throughput from file counts and snapshot size', async () => {
    const store = setupStore();

    await store._updateStatus({
      status: 'downloading',
      progress: 25,
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
    expect(store.throughputBps).toBe(40);
  });

  it('caps derived bytes when file counts exceed the reported total', async () => {
    const store = setupStore();

    await store._updateStatus({
      status: 'downloading',
      progress: 100,
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
    expect(store.throughputBps).toBe(300);
  });

  it('clears transient progress metadata when download transitions into installing', async () => {
    const store = setupStore();

    await store._updateStatus({
      status: 'downloading',
      progress: 50,
      currentStep: 'Downloading snapshot',
      snapshot: {
        digest: 'snapshot-3',
        createdAt: '2026-03-06T12:00:00.000Z',
        size: 1000,
      },
      filesDownloaded: 2,
      filesTotal: 4,
      elapsedSeconds: 10,
      remainingSeconds: 8,
    });

    await store._updateStatus({
      status: 'installing',
      progress: 92.5,
      currentStep: 'Installing Mithril snapshot',
      filesDownloaded: undefined,
      filesTotal: undefined,
      elapsedSeconds: undefined,
      remainingSeconds: undefined,
      error: null,
    });

    expect(store.currentStep).toBe('Installing Mithril snapshot');
    expect(store.filesDownloaded).toBeUndefined();
    expect(store.filesTotal).toBeUndefined();
    expect(store.elapsedSeconds).toBeUndefined();
    expect(store.remainingSeconds).toBeUndefined();
    expect(store.bytesDownloaded).toBeUndefined();
    expect(store.throughputBps).toBeUndefined();
  });

  it('loads chain storage config and tracks selected custom path', async () => {
    const store = setupStore();
    mockGetChainStorageDirectoryRequest.mockResolvedValue({
      customPath: '/mnt/external/daedalus-chain',
      setAt: '2026-03-09T00:00:00.000Z',
    });
    mockValidateChainStorageDirectoryRequest.mockResolvedValue({
      isValid: true,
      path: '/mnt/external/daedalus-chain',
      resolvedPath: '/mnt/external/daedalus-chain',
    });

    await store.loadChainStorageConfig();

    expect(store.customChainPath).toBe('/mnt/external/daedalus-chain');
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
    expect(store.chainStorageValidation).toEqual({
      isValid: false,
      path: '/mnt/missing-chain',
      reason: 'path-not-found',
      message: 'Configured chain storage target is unavailable.',
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
    mockSetChainStorageDirectoryRequest.mockResolvedValue({
      isValid: true,
      path: null,
      resolvedPath: '/tmp/state/chain',
    });

    await store.resetChainStorageDirectory();

    expect(mockSetChainStorageDirectoryRequest).toHaveBeenCalledWith({
      path: null,
    });
    expect(store.customChainPath).toBeNull();
  });

  it('marks storage location as confirmed when requested', () => {
    const store = setupStore();

    expect(store.storageLocationConfirmed).toBe(false);
    store.confirmStorageLocation();
    expect(store.storageLocationConfirmed).toBe(true);
  });
});
