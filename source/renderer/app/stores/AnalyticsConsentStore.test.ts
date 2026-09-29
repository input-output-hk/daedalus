import { autorun } from 'mobx';
import AnalyticsConsentStore from './AnalyticsConsentStore';
import { Api } from '../api';
import { ActionsMap } from '../actions';
import {
  ConsentView,
  ARIADNE_CONSENT_VERSION,
} from '../../../common/analytics/contract';

const view = (status: ConsentView['status']): ConsentView => ({
  status,
  version: ARIADNE_CONSENT_VERSION,
  enabled: true,
  generation: 2,
});
function deferred<T>() {
  let resolve: (value: T) => void;
  let reject: (reason?: unknown) => void;
  const promise = new Promise<T>((yes, no) => {
    resolve = yes;
    reject = no;
  });
  return { promise, resolve, reject };
}
function fixture() {
  const api = { analyticsConsent: { get: jest.fn(), set: jest.fn() } };
  const tracker = {
    enableTracking: jest.fn(async () => {}),
    disableTracking: jest.fn(),
    sendEvent: jest.fn(),
    sendPageNavigationEvent: jest.fn(),
  };
  return {
    api,
    tracker,
    store: new AnalyticsConsentStore(
      api as unknown as Api,
      {} as ActionsMap,
      tracker
    ),
  };
}

test('one load updates the shared observable; acknowledgement updates it without another IPC read', async () => {
  const { api, tracker, store } = fixture();
  api.analyticsConsent.get.mockResolvedValue(view('PENDING'));
  const observed: string[] = [];
  const stop = autorun(() => {
    observed.push(store.view?.status);
  });
  await store.load();
  api.analyticsConsent.set.mockResolvedValue(view('ACCEPTED'));
  expect(await store.save('ACCEPTED')).toBe(true);
  expect(api.analyticsConsent.get).toHaveBeenCalledTimes(1);
  expect(observed).toEqual([undefined, 'PENDING', 'ACCEPTED']);
  expect(tracker.disableTracking).toHaveBeenCalledTimes(2);
  stop();
});

test('a stale load or acceptance cannot undo a newer revoke', async () => {
  const { api, store } = fixture();
  const load = deferred<ConsentView>();
  const accept = deferred<ConsentView>();
  const revoke = deferred<ConsentView>();
  api.analyticsConsent.get.mockReturnValue(load.promise);
  api.analyticsConsent.set
    .mockReturnValueOnce(accept.promise)
    .mockReturnValueOnce(revoke.promise);
  const loading = store.load();
  const accepting = store.save('ACCEPTED');
  const revoking = store.save('REJECTED');
  expect(api.analyticsConsent.set.mock.calls).toEqual([
    ['ACCEPTED'],
    ['REJECTED'],
  ]);
  revoke.resolve(view('REJECTED'));
  expect(await revoking).toBe(true);
  accept.resolve(view('ACCEPTED'));
  load.resolve(view('ACCEPTED'));
  expect(await accepting).toBe(false);
  await loading;
  expect(store.view.status).toBe('REJECTED');
  expect(store.saving).toBe(false);
});

test('persistence failure stops collection and exposes retry state; teardown invalidates pending responses', async () => {
  const { api, tracker, store } = fixture();
  api.analyticsConsent.set.mockRejectedValue(new Error('synthetic'));
  expect(await store.save('REJECTED')).toBe(false);
  expect(store.view).toBeNull();
  expect(store.saveFailed).toBe(true);
  expect(store.saving).toBe(false);
  expect(tracker.disableTracking).toHaveBeenCalledTimes(1);
  const accept = deferred<ConsentView>();
  api.analyticsConsent.set.mockReturnValue(accept.promise);
  const saving = store.save('ACCEPTED');
  store.teardown();
  accept.resolve(view('ACCEPTED'));
  expect(await saving).toBe(false);
  expect(store.view).toBeNull();
});

test('a failed save preserves availability for retry while tracking stays stopped', async () => {
  const { api, tracker, store } = fixture();
  api.analyticsConsent.get.mockResolvedValue(view('ACCEPTED'));
  await store.load();
  tracker.enableTracking.mockClear();
  api.analyticsConsent.set.mockRejectedValueOnce(new Error('synthetic'));
  expect(await store.save('REJECTED')).toBe(false);
  expect(store.view.enabled).toBe(true);
  expect(store.saveFailed).toBe(true);
  expect(tracker.enableTracking).not.toHaveBeenCalled();
  api.analyticsConsent.set.mockResolvedValueOnce(view('REJECTED'));
  expect(await store.save('REJECTED')).toBe(true);
  expect(store.view.status).toBe('REJECTED');
  expect(store.saveFailed).toBe(false);
  expect(tracker.enableTracking).not.toHaveBeenCalled();
});

test('refreshes cannot re-enable stale acceptance during or after a failed revoke', async () => {
  const { api, tracker, store } = fixture();
  api.analyticsConsent.get.mockResolvedValue(view('ACCEPTED'));
  await store.load();
  const revoke = deferred<ConsentView>();
  api.analyticsConsent.set.mockReturnValue(revoke.promise);
  const saving = store.save('REJECTED');
  expect(store.trackingView).toBeNull();
  await store.load();
  expect(api.analyticsConsent.get).toHaveBeenCalledTimes(1);
  revoke.reject(new Error('synthetic'));
  expect(await saving).toBe(false);
  await store.load();
  expect(api.analyticsConsent.get).toHaveBeenCalledTimes(1);
  expect(store.trackingView).toBeNull();
  expect(tracker.enableTracking).toHaveBeenCalledTimes(1);
});

test('out-of-order refreshes cannot resurrect accepted state; disposed stores cannot start IPC', async () => {
  const { api, store } = fixture();
  const old = deferred<ConsentView>();
  api.analyticsConsent.get
    .mockReturnValueOnce(old.promise)
    .mockResolvedValueOnce(view('REJECTED'));
  const loading = store.load();
  await store.load();
  old.resolve(view('ACCEPTED'));
  await loading;
  expect(store.view.status).toBe('REJECTED');
  store.teardown();
  await store.load();
  expect(await store.save('ACCEPTED')).toBe(false);
  expect(api.analyticsConsent.get).toHaveBeenCalledTimes(2);
  expect(api.analyticsConsent.set).not.toHaveBeenCalled();
  expect(store.trackingView).toBeNull();
});

test('teardown invalidates an observed tracking snapshot', async () => {
  const { api, store } = fixture();
  api.analyticsConsent.get.mockResolvedValue(view('ACCEPTED'));
  await store.load();
  const snapshots: Array<ConsentView | null> = [];
  const stop = autorun(() => snapshots.push(store.trackingView));
  store.teardown();
  expect(snapshots).toEqual([view('ACCEPTED'), null]);
  stop();
});
