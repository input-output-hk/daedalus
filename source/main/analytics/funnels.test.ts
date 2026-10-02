/** @jest-environment node */
import { AnalyticsOwner } from './AnalyticsOwner';
import { ARIADNE_CONSENT_VERSION } from '../../common/analytics/contract';
import { AriadneAnalyticsTracker } from '../../renderer/app/analytics/AriadneAnalyticsTracker';
import { FunnelCapture } from '../../renderer/app/analytics/FunnelCapture';

const flush = async () => {
  for (let i = 0; i < 40; i++) await Promise.resolve();
};
const startTime = Date.parse('2026-09-23T12:00:00Z');
function fixture() {
  let clock = startTime;
  const send = jest.fn(
    async (_url: string, _body: string, _signal: AbortSignal) => ({
      status: 204,
    })
  );
  const owner = new AnalyticsOwner(
    { endpoint: 'http://127.0.0.1:3000/api/analytics/event' },
    { read: () => null, write: () => {} },
    {
      platform: 'win32',
      osVersion: '10.0.26100',
      cpu: 'Intel Core i7',
      ram: 16 * 1024 ** 3,
      appVersion: '11.4.0',
      network: 'development',
    },
    send,
    () => clock
  );
  const decide = (status: string) =>
    owner.consent({ version: ARIADNE_CONSENT_VERSION, status });
  const step = (
    attempt: number,
    stage = 'started',
    action = 'delegation_submit'
  ) => ({
    type: 'funnel_step',
    action,
    attempt,
    stage,
    ts: new Date(clock).toISOString(),
    generation: owner.view().generation,
    uses_legacy_wallet: false,
    uses_hardware_wallet: false,
  });
  return {
    owner,
    send,
    decide,
    step,
    advance: (ms: number) => {
      clock += ms;
    },
  };
}

test('main owns attempt UUIDs, rejects duplicate/order/flow violations, bounds lifetime and invalidates revoked work', async () => {
  const f = fixture();
  try {
    expect(f.owner.enqueue(f.step(1))).toBe(false);
    f.decide('ACCEPTED');
    expect(f.owner.enqueue(f.step(1, 'completed'))).toBe(false);
    expect(f.owner.enqueue({ ...f.step(1), attempt_id: 'renderer-uuid' })).toBe(
      false
    );
    expect(f.owner.enqueue(f.step(1))).toBe(true);
    expect(f.owner.enqueue(f.step(1))).toBe(false);
    expect(
      f.owner.enqueue(f.step(1, 'completed', 'voting_registration_setup'))
    ).toBe(false);
    await flush();
    expect(f.owner.enqueue(f.step(1, 'completed'))).toBe(true);
    expect(f.owner.enqueue(f.step(1, 'cancelled'))).toBe(false);
    await flush();
    const [start, end] = f.send.mock.calls.map((call) => JSON.parse(call[1]));
    expect(start.version).toBe(2);
    expect(start.sequence).toBe(0);
    expect(end.sequence).toBe(1);
    expect(start.attempt_id).toEqual(end.attempt_id);
    expect(start.attempt_id).not.toBe(start.user_id);
    expect(start).not.toHaveProperty('attempt');
    expect(f.owner.enqueue(f.step(2))).toBe(true);
    await flush();
    f.advance(30 * 60_000 + 1);
    expect(f.owner.enqueue(f.step(2, 'completed'))).toBe(false);
    expect(f.owner.enqueue(f.step(2))).toBe(false);
    expect(f.owner.enqueue(f.step(3))).toBe(true);
    f.decide('REJECTED');
    await flush();
    expect(f.send).toHaveBeenCalledTimes(3);
    f.decide('ACCEPTED');
    expect(f.owner.enqueue(f.step(3, 'completed'))).toBe(false);
    for (let attempt = 4; attempt < 12; attempt++) {
      expect(f.owner.enqueue(f.step(attempt))).toBe(true);
      await flush();
    }
    expect(f.owner.enqueue(f.step(12))).toBe(false);
  } finally {
    f.owner.close();
  }
});

test('renderer serializes steps, bounds pending IPC and discards continuations across revocation', async () => {
  let finish: (value: boolean) => void;
  const send = jest.fn(
    () =>
      new Promise<boolean>((resolve) => {
        finish = resolve;
      })
  );
  const tracker = new AriadneAnalyticsTracker(
    async () => ({
      version: ARIADNE_CONSENT_VERSION,
      enabled: true,
      status: 'ACCEPTED',
      generation: 2,
    }),
    send,
    () => startTime
  );
  const flags = jest.fn(() => ({
    uses_legacy_wallet: false,
    uses_hardware_wallet: false,
  }));
  tracker.setWalletSnapshot(flags);
  expect(tracker.beginFunnel('delegation_submit')).toBeUndefined();
  expect(flags).not.toHaveBeenCalled();
  await tracker.enableTracking();
  const attempt = tracker.beginFunnel('delegation_submit');
  attempt.complete();
  attempt.cancel();
  for (let i = 0; i < 20; i++) tracker.beginFunnel('delegation_submit');
  await flush();
  expect(send).toHaveBeenCalledTimes(1);
  tracker.disableTracking();
  finish(true);
  await flush();
  expect(send).toHaveBeenCalledTimes(1);
  await tracker.enableTracking();
  tracker.setWalletSnapshot(() => {
    throw Error('synthetic');
  });
  expect(() => tracker.beginFunnel('delegation_submit')).not.toThrow();
});

test('dialog cancellation before submission differs from an ambiguous backend outcome; late success belongs to its original attempt', () => {
  const first = { complete: jest.fn(), cancel: jest.fn() };
  const second = { complete: jest.fn(), cancel: jest.fn() };
  const tracker = {
    beginFunnel: jest
      .fn()
      .mockReturnValueOnce(first)
      .mockReturnValueOnce(second),
    sendEvent: jest.fn(),
    sendPageNavigationEvent: jest.fn(),
    enableTracking: jest.fn(),
    disableTracking: jest.fn(),
  };
  const flow = new FunnelCapture(tracker, 'delegation_submit');
  flow.open();
  const captured = flow.submission();
  flow.close();
  expect(first.cancel).not.toHaveBeenCalled();
  flow.open();
  flow.complete(captured);
  expect(first.complete).toHaveBeenCalledTimes(1);
  expect(second.complete).not.toHaveBeenCalled();
  flow.close();
  expect(second.cancel).toHaveBeenCalledTimes(1);
  flow.complete(undefined);
  expect(second.complete).not.toHaveBeenCalled();
  tracker.beginFunnel.mockImplementation(() => {
    throw Error('synthetic');
  });
  expect(() => {
    flow.open();
    flow.submission();
    flow.complete(undefined);
    flow.close();
  }).not.toThrow();
});
