import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import MithrilBootstrapStore from './MithrilBootstrapStore';
import { noopAnalyticsTracker } from '../analytics';

describe('MithrilBootstrapStore', () => {
  const api: Api = {
    ada: jest.fn(),
  } as any;
  const actions: ActionsMap = jest.fn() as any;

  const setupStore = () =>
    new MithrilBootstrapStore(api, actions, noopAnalyticsTracker);

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

  it('clears transient progress metadata when the status update explicitly resets those fields', async () => {
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
      status: 'preparing',
      progress: 5,
      currentStep: undefined,
      filesDownloaded: undefined,
      filesTotal: undefined,
      elapsedSeconds: undefined,
      remainingSeconds: undefined,
      error: null,
    });

    expect(store.currentStep).toBeUndefined();
    expect(store.filesDownloaded).toBeUndefined();
    expect(store.filesTotal).toBeUndefined();
    expect(store.elapsedSeconds).toBeUndefined();
    expect(store.remainingSeconds).toBeUndefined();
    expect(store.bytesDownloaded).toBeUndefined();
    expect(store.throughputBps).toBeUndefined();
  });
});
