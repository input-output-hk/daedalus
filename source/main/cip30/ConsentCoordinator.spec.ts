import type { DappCip30Rejection } from '../../common/cip30/errors';
import type { DappConsentPresentation } from '../../common/ipc/api';
import { ConsentCoordinator } from './ConsentCoordinator';

const declined: DappCip30Rejection = {
  type: 'api-error',
  value: { code: -3, info: 'Refused' },
};
const identity = {
  guestWebContentsId: 7,
  documentGeneration: 2,
  origin: 'https://example.test',
  connectionId: 'connection',
  walletId: 'wallet',
  routeEpoch: 3,
  networkGenesis: 'genesis',
};
const presentation = {
  kind: 'connection' as const,
  origin: identity.origin,
  walletName: 'Wallet',
  networkName: 'Preview',
  scopes: ['connection', 'read'],
  extensions: [95],
};

const setup = (timeout = 300_000) => {
  const presented: DappConsentPresentation[] = [];
  const terminal: string[] = [];
  const hidden: boolean[] = [];
  const coordinator = new ConsentCoordinator({
    present: async (request) => {
      presented.push(request);
    },
    terminal: async (requestId) => {
      terminal.push(requestId);
    },
    setGuestHidden: (value) => hidden.push(value),
    inactivityTimeoutMs: timeout,
  });
  return { coordinator, presented, terminal, hidden };
};

const request = <T>(
  coordinator: ConsentCoordinator,
  execute: (payload: unknown, signal: AbortSignal) => Promise<T>,
  options: { payload?: unknown; submission?: boolean } = {}
) =>
  coordinator.request({
    identity,
    presentation,
    payload: options.payload ?? { bytes: 'aabb' },
    declined,
    submission: options.submission,
    execute,
  });

describe('ConsentCoordinator', () => {
  afterEach(() => jest.useRealTimers());

  it('queues requests FIFO and executes immutable broker-owned payloads', async () => {
    const { coordinator, presented, terminal, hidden } = setup();
    const payload = { bytes: 'aabb', nested: ['fixed'] };
    const firstExecute = jest.fn(async (value) => value);
    const secondExecute = jest.fn(async () => 'second');
    const first = request(coordinator, firstExecute, { payload });
    const second = request(coordinator, secondExecute);

    expect(presented).toHaveLength(1);
    expect(Object.isFrozen(presented[0])).toBe(true);
    payload.bytes = 'replaced';
    coordinator.decide(presented[0].requestId, true);
    await expect(first).resolves.toEqual({ bytes: 'aabb', nested: ['fixed'] });
    expect(firstExecute).toHaveBeenCalledWith(
      { bytes: 'aabb', nested: ['fixed'] },
      expect.any(AbortSignal),
      undefined
    );
    expect(terminal).toEqual([presented[0].requestId]);
    expect(presented).toHaveLength(2);

    coordinator.decide(presented[1].requestId, true);
    await expect(second).resolves.toBe('second');
    expect(hidden).toEqual([true, true, false]);
  });

  it('expires after five minutes of inactivity and activity resets the timer', async () => {
    jest.useFakeTimers();
    const { coordinator, presented } = setup(300_000);
    const pending = request(coordinator, async () => 'unused');

    jest.advanceTimersByTime(299_000);
    coordinator.activity(presented[0].requestId);
    jest.advanceTimersByTime(299_000);
    coordinator.decide('stale-request', true);
    jest.advanceTimersByTime(1_001);

    await expect(pending).rejects.toEqual(declined);
  });

  it('rejects refusal once and ignores late, duplicate, and replayed decisions', async () => {
    const { coordinator, presented, terminal } = setup();
    const execute = jest.fn(async () => 'unused');
    const pending = request(coordinator, execute);
    const requestId = presented[0].requestId;

    coordinator.decide(requestId, false);
    coordinator.decide(requestId, true);
    coordinator.decide(requestId, false);

    await expect(pending).rejects.toEqual(declined);
    expect(execute).not.toHaveBeenCalled();
    expect(terminal).toEqual([requestId]);
  });

  it('cancels matching work with the exact lifecycle rejection', async () => {
    const { coordinator, presented } = setup();
    let signal: AbortSignal | undefined;
    const pending = request(coordinator, async (_payload, nextSignal) => {
      signal = nextSignal;
      return new Promise(() => undefined);
    });
    coordinator.decide(presented[0].requestId, true);
    const accountChanged: DappCip30Rejection = {
      type: 'api-error',
      value: { code: -4, info: 'Account changed' },
    };

    coordinator.cancel(
      (candidate) => candidate.walletId === 'wallet',
      accountChanged
    );

    await expect(pending).rejects.toEqual(accountChanged);
    expect(signal?.aborted).toBe(true);
  });

  it('continues an authorized submission but suppresses its stale result', async () => {
    const { coordinator, presented } = setup();
    let finish: (value: string) => void = () => undefined;
    const execute = jest.fn(
      async () => new Promise<string>((resolve) => (finish = resolve))
    );
    const pending = request(coordinator, execute, { submission: true });
    coordinator.decide(presented[0].requestId, true);

    coordinator.cancel();
    expect(execute).toHaveBeenCalledTimes(1);
    finish('transaction-id');

    await expect(pending).rejects.toEqual(declined);
  });

  it('passes transient passphrase once and preserves typed execution errors', async () => {
    const { coordinator, presented } = setup();
    const typedError = Object.assign(new Error('Proof generation failed'), {
      type: 'data-sign-error' as const,
      value: { code: 1, info: 'Proof generation failed' },
    });
    const execute = jest.fn(async () => {
      throw typedError;
    });
    const pending = request(coordinator, execute);
    coordinator.decide(presented[0].requestId, true, 'secret');
    await expect(pending).rejects.toBe(typedError);
    expect(execute).toHaveBeenCalledWith(
      { bytes: 'aabb' },
      expect.any(AbortSignal),
      'secret'
    );
  });
});
