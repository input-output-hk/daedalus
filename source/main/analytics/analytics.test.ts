/** @jest-environment node */
import { AnalyticsOwner, ConsentStorage } from './AnalyticsOwner';
import { analyticsConfig } from './config';
import { normalizeEvent, Device } from './normalize';
import { isAnalyticsSender } from './sender';
import {
  eventIntent,
  validMessage,
  ARIADNE_CONSENT_VERSION,
} from '../../common/analytics/contract';
import {
  analyticsActions,
  analyticsPages,
} from '../../common/analytics/vocabulary';
import { AriadneAnalyticsTracker } from '../../renderer/app/analytics/AriadneAnalyticsTracker';
import { EventCategories } from '../../renderer/app/analytics/types';

const endpoint = 'http://127.0.0.1:3000/api/analytics/event';
const device: Device = {
  platform: 'win32',
  osVersion: '10.0.26100',
  cpu: 'Intel Core i7',
  ram: 16 * 1024 ** 3,
  appVersion: '11.4.0',
  network: 'development',
};
const id = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';
const now = Date.parse('2026-09-23T12:00:00.000Z');
const event = (generation = 1) => ({
  type: 'page_view' as const,
  action: 'Wallet Summary',
  uses_legacy_wallet: false,
  uses_hardware_wallet: true,
  ts: new Date(now).toISOString(),
  generation,
});
const flush = async () => {
  for (let i = 0; i < 12; i++) await Promise.resolve();
};
const decide = (
  owner: AnalyticsOwner,
  status: 'ACCEPTED' | 'REJECTED' | 'PENDING'
) => owner.consent({ version: ARIADNE_CONSENT_VERSION, status });
function fixture() {
  let saved: unknown;
  const storage: ConsentStorage = {
    read: () => saved,
    write: (value) => {
      saved = JSON.parse(JSON.stringify(value));
    },
  };
  const send = jest.fn(
    async (_url: string, _body: string, _signal: AbortSignal) => ({
      status: 204,
    })
  );
  const create = () =>
    new AnalyticsOwner({ endpoint }, storage, device, send, () => now);
  return { storage, send, create };
}

describe('Ariadne consent and main-process admission', () => {
  test('old Matomo decision, pending and rejected never send; accepted restart retains a separate UUID; revoke rotates', async () => {
    const f = fixture();
    f.storage.write({ status: 'ACCEPTED', id }); // No Ariadne version/recipient.
    const owner = f.create();
    expect(owner.view().status).toBe('PENDING');
    expect(owner.enqueue(event())).toBe(false);
    decide(owner, 'REJECTED');
    expect(owner.enqueue(event(owner.view().generation))).toBe(false);
    decide(owner, 'ACCEPTED');
    owner.enqueue(event(owner.view().generation));
    await flush();
    const first = JSON.parse(f.send.mock.calls[0][1]).user_id;
    expect(first).not.toBe(id);
    owner.close();
    const restarted = f.create();
    restarted.enqueue(event(restarted.view().generation));
    await flush();
    expect(JSON.parse(f.send.mock.calls[1][1]).user_id).toBe(first);
    const stale = event(restarted.view().generation);
    decide(restarted, 'REJECTED');
    expect(f.storage.read()).not.toHaveProperty('id');
    expect(restarted.enqueue(stale)).toBe(false);
    decide(restarted, 'ACCEPTED');
    expect(restarted.enqueue(stale)).toBe(false);
    restarted.enqueue(event(restarted.view().generation));
    await flush();
    expect(JSON.parse(f.send.mock.calls[2][1]).user_id).not.toBe(first);
    restarted.close();
  });
  test('changed recipient/version and failed storage never inherit acceptance', () => {
    const f = fixture();
    const owner = f.create();
    decide(owner, 'ACCEPTED');
    owner.close();
    const changed = new AnalyticsOwner(
      { endpoint: 'https://example.invalid/api/analytics/event' },
      f.storage,
      device,
      f.send,
      () => now
    );
    expect(changed.view().status).toBe('PENDING');
    changed.close();
    f.storage.write({ ...(f.storage.read() as object), version: 99 });
    const bumped = f.create();
    expect(bumped.view().status).toBe('PENDING');
    bumped.close();
    const failed = new AnalyticsOwner(
      { endpoint },
      {
        read: () => null,
        write: () => {
          throw new Error('private path');
        },
      },
      device,
      f.send
    );
    expect(() => decide(failed, 'ACCEPTED')).toThrow(
      'Analytics choice could not be saved'
    );
    expect(failed.view().enabled).toBe(false);
    expect(failed.enqueue(event())).toBe(false);
    failed.close();
    const disabled = new AnalyticsOwner(null, f.storage, device, f.send);
    decide(disabled, 'ACCEPTED');
    expect(disabled.enqueue(event())).toBe(false);
    disabled.close();
    expect(f.send).not.toHaveBeenCalled();
  });
  test('revocation cancels a request, clears queued work, and prevents a pre-dispatch microtask send', async () => {
    const f = fixture();
    const owner = f.create();
    decide(owner, 'ACCEPTED');
    owner.enqueue(event(owner.view().generation));
    decide(owner, 'REJECTED');
    await flush();
    expect(f.send).not.toHaveBeenCalled();
    owner.close();
    let complete: (value: { status: number }) => void;
    let signal: AbortSignal;
    const send = jest.fn((_url: string, _body: string, cancel: AbortSignal) => {
      signal = cancel;
      return new Promise<{ status: number }>((resolve) => {
        complete = resolve;
      });
    });
    const active = new AnalyticsOwner(
      { endpoint },
      f.storage,
      device,
      send,
      () => now
    );
    decide(active, 'ACCEPTED');
    active.enqueue(event(active.view().generation));
    await flush();
    active.enqueue(event(active.view().generation));
    decide(active, 'REJECTED');
    expect(signal.aborted).toBe(true);
    complete({ status: 204 });
    await flush();
    expect(send).toHaveBeenCalledTimes(1);
    active.close();
  });
  test('32 waiting events, one active request, 30/minute budget, TTL and no ambiguous replay', async () => {
    const f = fixture();
    let clock = now;
    let complete: (value: { status: number }) => void;
    const send = jest.fn(
      () =>
        new Promise<{ status: number }>((resolve) => {
          complete = resolve;
        })
    );
    const owner = new AnalyticsOwner(
      { endpoint },
      f.storage,
      device,
      send,
      () => clock
    );
    decide(owner, 'ACCEPTED');
    owner.enqueue(event(owner.view().generation));
    await flush();
    for (let i = 0; i < 32; i++)
      expect(owner.enqueue(event(owner.view().generation))).toBe(true);
    expect(owner.enqueue(event(owner.view().generation))).toBe(false);
    clock += 30_001;
    complete({ status: 0 });
    await flush();
    expect(send).toHaveBeenCalledTimes(1);
    owner.close();
    const budget = f.create();
    decide(budget, 'ACCEPTED');
    for (let i = 0; i < 40; i++) {
      budget.enqueue(event(budget.view().generation));
      await flush();
    }
    expect(f.send).toHaveBeenCalledTimes(30);
    budget.close();
  });
  test('429 and 503 drop attempted items and pause future sends', async () => {
    const f = fixture();
    let clock = now;
    const send = jest.fn(async () => ({ status: 429, retryAfter: 9999 }));
    const owner = new AnalyticsOwner(
      { endpoint },
      f.storage,
      device,
      send,
      () => clock
    );
    decide(owner, 'ACCEPTED');
    owner.enqueue(event(owner.view().generation));
    await flush();
    owner.enqueue(event(owner.view().generation));
    await flush();
    expect(send).toHaveBeenCalledTimes(1);
    clock += 60_001;
    send.mockResolvedValue({ status: 503, retryAfter: 0 });
    owner.enqueue({
      ...event(owner.view().generation),
      ts: new Date(clock).toISOString(),
    });
    await flush();
    expect(send).toHaveBeenCalledTimes(2);
    owner.close();
  });
});

describe('contract and trust boundaries', () => {
  test('requires explicit opt in, development loopback exception and packaged HTTPS', () => {
    const env = {
      NODE_ENV: 'development',
      DAEDALUS_ARIADNE_ANALYTICS_ENABLED: 'true',
      DAEDALUS_ARIADNE_ANALYTICS_URL: endpoint,
      DAEDALUS_ARIADNE_ALLOW_LOOPBACK_HTTP: 'true',
    };
    expect(analyticsConfig({}, false)).toBeNull();
    expect(analyticsConfig(env, false)).toEqual({ endpoint });
    expect(analyticsConfig(env, true)).toBeNull();
    expect(
      analyticsConfig({ ...env, NODE_ENV: 'production' }, false)
    ).toBeNull();
    for (const url of [
      'http://example.invalid/api/analytics/event',
      `${endpoint}?token=anything`,
      `${endpoint}#x`,
      'https://user:pass@example.invalid/api/analytics/event',
      'file:///api/analytics/event',
    ])
      expect(
        analyticsConfig({ ...env, DAEDALUS_ARIADNE_ANALYTICS_URL: url }, false)
      ).toBeNull();
    expect(
      analyticsConfig(
        {
          ...env,
          DAEDALUS_ARIADNE_ANALYTICS_URL:
            'https://example.invalid/api/analytics/event',
        },
        true
      )
    ).not.toBeNull();
  });
  test('only exact main window/frame/document can use IPC; URL lookalikes and frames fail', () => {
    const frame = {};
    const contents = { mainFrame: frame, isDestroyed: () => false };
    const ipc = { sender: contents, senderFrame: frame };
    expect(
      isAnalyticsSender(
        ipc,
        contents,
        'http://127.0.0.1:8080/#/wallets',
        'http://127.0.0.1:8080/'
      )
    ).toBe(true);
    expect(
      isAnalyticsSender(
        { ...ipc, senderFrame: {} },
        contents,
        endpoint,
        endpoint
      )
    ).toBe(false);
    expect(
      isAnalyticsSender({ ...ipc, sender: {} }, contents, endpoint, endpoint)
    ).toBe(false);
    expect(
      isAnalyticsSender(
        ipc,
        contents,
        'https://127.0.0.1:8080/',
        'http://127.0.0.1:8080/'
      )
    ).toBe(false);
    expect(
      isAnalyticsSender(
        ipc,
        contents,
        'file:///other.html',
        'file:///index.html'
      )
    ).toBe(false);
  });
  test('only approved vocabulary survives; no arbitrary labels, URLs, values or dimensions', () => {
    for (const page of analyticsPages)
      expect(validMessage({ ...event(), action: page }, now)).toBe(true);
    for (const [category, actions] of Object.entries(analyticsActions))
      for (const action of actions)
        expect(
          validMessage(
            { ...event(), type: 'custom_event', category, action },
            now
          )
        ).toBe(true);
    expect(
      eventIntent(
        'custom_event',
        'Changed SMASH server',
        'Settings',
        'https://private.invalid'
      )
    ).toEqual({
      type: 'custom_event',
      category: 'SETTINGS',
      action: 'Changed SMASH server',
    });
    expect(eventIntent('custom_event', 'anything', '__proto__')).toBeNull();
    for (const extra of [
      { url: endpoint },
      { user_id: id },
      { value: 1 },
      { label: 'secret' },
      { ts: '2026-02-31T12:00:00.000Z' },
      { action: 'wallet/secret' },
      { uses_hardware_wallet: 'no' },
    ])
      expect(
        normalizeEvent({ ...event(), ...extra }, id, device, now)
      ).toBeNull();
    for (const [platform, os] of [
      ['win32', 'Windows'],
      ['darwin', 'macOS'],
      ['linux', 'Linux'],
    ])
      expect(
        normalizeEvent(
          event(),
          id,
          {
            ...device,
            platform,
            cpu: 'Unreviewed CPU',
            osVersion: '6.0-host-name',
            ram: 15.9 * 1024 ** 3,
          },
          now
        ).dimensions
      ).toMatchObject({ os, os_version: null, ram_gb: 16, cpu: 'Other' });
    expect(
      normalizeEvent(event(), id, { ...device, appVersion: '11.4.0-beta' }, now)
    ).toBeNull();
  });
  test('renderer async enable is invalidated by revoke, drops unknown wallets, bounds outstanding IPC', async () => {
    let resolve: (view: {
      version: number;
      status: 'ACCEPTED';
      enabled: boolean;
      generation: number;
    }) => void;
    const get = jest.fn(
      () =>
        new Promise<ReturnType<AnalyticsOwner['view']>>((done) => {
          resolve = done;
        })
    );
    const send = jest.fn(async (_event: unknown) => true);
    const tracker = new AriadneAnalyticsTracker(get, send, () => now);
    const first = tracker.enableTracking();
    tracker.disableTracking();
    resolve({
      version: ARIADNE_CONSENT_VERSION,
      status: 'ACCEPTED',
      enabled: true,
      generation: 2,
    });
    await first;
    tracker.sendPageNavigationEvent('Wallet Summary');
    expect(send).not.toHaveBeenCalled();
    const second = tracker.enableTracking();
    resolve({
      version: ARIADNE_CONSENT_VERSION,
      status: 'ACCEPTED',
      enabled: true,
      generation: 4,
    });
    await second;
    tracker.sendPageNavigationEvent('Wallet Summary');
    expect(send).not.toHaveBeenCalled();
    tracker.setWalletSnapshot(() => ({
      uses_legacy_wallet: false,
      uses_hardware_wallet: true,
    }));
    tracker.sendEvent(
      EventCategories.SETTINGS,
      'Changed currency',
      'private dynamic label',
      123
    );
    tracker.sendPageNavigationEvent('Wallet Summary');
    expect(send).toHaveBeenCalledTimes(1);
    expect(send.mock.calls[0][0]).not.toHaveProperty('label');
    expect(send.mock.calls[0][0]).not.toHaveProperty('value');
    tracker.disableTracking();
    await flush();
    tracker.sendPageNavigationEvent('Wallet Summary');
    expect(send).toHaveBeenCalledTimes(1);
  });
});
