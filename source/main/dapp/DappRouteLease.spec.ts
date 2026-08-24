import type { DappRouteLease } from './DappRouteLease';
import {
  DappRouteLeaseService,
  StaleDappRouteLeaseError,
} from './DappRouteLease';

const route = (walletId: string) =>
  `file:///app/index.html#/wallets/${walletId}/dapps`;

const requireLease = (lease: DappRouteLease | null): DappRouteLease => {
  if (!lease) throw new Error('Expected route lease');
  return lease;
};

describe('DappRouteLeaseService', () => {
  it('derives an exact wallet lease and preserves it on same-wallet refresh', () => {
    const service = new DappRouteLeaseService('genesis');
    const first = service.observeTrustedRoute(route('wallet-a'));
    const refreshed = service.observeTrustedRoute(route('wallet-a'));

    expect(first).toEqual({
      walletId: 'wallet-a',
      routeEpoch: 1,
      networkGenesis: 'genesis',
    });
    expect(refreshed).toBe(first);
  });

  it.each([
    'file:///app/index.html#/wallets/wallet-a/summary',
    'file:///app/index.html#/wallets//dapps',
    'file:///app/index.html#/wallets/wallet-a/dapps?fallback=true',
    'not a url',
  ])('revokes without route fallback for %s', (invalidRoute) => {
    const revoked = jest.fn();
    const service = new DappRouteLeaseService('genesis', revoked);
    const lease = requireLease(service.observeTrustedRoute(route('wallet-a')));

    expect(service.observeTrustedRoute(invalidRoute)).toBeNull();
    expect(revoked).toHaveBeenCalledTimes(1);
    expect(() => service.requireCurrent(lease)).toThrow(
      StaleDappRouteLeaseError
    );
  });

  it('increments the epoch and revokes on wallet or network mismatch', () => {
    const revoked = jest.fn();
    const service = new DappRouteLeaseService('genesis-a', revoked);
    const first = requireLease(service.observeTrustedRoute(route('wallet-a')));
    const second = requireLease(service.observeTrustedRoute(route('wallet-b')));

    expect(second.routeEpoch).toBeGreaterThan(first.routeEpoch);
    expect(revoked).toHaveBeenCalledTimes(1);
    service.changeNetwork('genesis-b');
    expect(service.current).toBeNull();
    expect(revoked).toHaveBeenCalledTimes(2);
  });

  it('revokes wallet deletion and rejects a stale lease', () => {
    const service = new DappRouteLeaseService('genesis');
    const lease = requireLease(service.observeTrustedRoute(route('wallet-a')));
    service.revokeWallet('wallet-a');
    expect(service.isCurrent(lease)).toBe(false);
  });

  it('finishes an authorized submission but withholds its stale result', async () => {
    const service = new DappRouteLeaseService('genesis');
    const lease = requireLease(service.observeTrustedRoute(route('wallet-a')));
    let finish!: () => void;
    const submission = new Promise<void>((resolve) => {
      finish = resolve;
    });
    const result = service.completeAuthorizedSubmission(lease, submission);

    service.observeTrustedRoute(route('wallet-b'));
    finish();

    await expect(result).rejects.toThrow(StaleDappRouteLeaseError);
  });
});
