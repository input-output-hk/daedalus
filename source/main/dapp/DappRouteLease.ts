export type DappRouteLease = Readonly<{
  walletId: string;
  routeEpoch: number;
  networkGenesis: string;
}>;

export class StaleDappRouteLeaseError extends Error {
  constructor() {
    super('DApp route lease is stale');
  }
}

const walletIdFromRoute = (url: string): string | null => {
  let hash: string;
  try {
    hash = new URL(url).hash;
  } catch {
    return null;
  }
  const match = /^#\/wallets\/([^/?#]+)\/dapps$/u.exec(hash);
  if (!match) return null;
  try {
    const walletId = decodeURIComponent(match[1]);
    return walletId !== '' && !walletId.includes('/') ? walletId : null;
  } catch {
    return null;
  }
};

export class DappRouteLeaseService {
  private routeEpoch = 0;
  private lease: DappRouteLease | null = null;
  private networkGenesis: string;
  private readonly onRevoked: () => void;

  constructor(networkGenesis: string, onRevoked: () => void = () => undefined) {
    this.onRevoked = onRevoked;
    if (typeof networkGenesis !== 'string' || networkGenesis === '')
      throw new Error('Invalid network genesis');
    this.networkGenesis = networkGenesis;
  }

  get current(): DappRouteLease | null {
    return this.lease;
  }

  observeTrustedRoute(url: string): DappRouteLease | null {
    const walletId = walletIdFromRoute(url);
    if (walletId && this.lease?.walletId === walletId) return this.lease;

    this.revoke();
    if (walletId) {
      this.lease = Object.freeze({
        walletId,
        routeEpoch: this.routeEpoch,
        networkGenesis: this.networkGenesis,
      });
    }
    return this.lease;
  }

  changeNetwork(networkGenesis: string): void {
    if (
      typeof networkGenesis !== 'string' ||
      networkGenesis === '' ||
      networkGenesis === this.networkGenesis
    )
      return;
    this.networkGenesis = networkGenesis;
    this.revoke();
  }

  revokeWallet(walletId: string): void {
    if (this.lease?.walletId === walletId) this.revoke();
  }

  revoke(): void {
    this.routeEpoch += 1;
    const wasActive = this.lease !== null;
    this.lease = null;
    if (wasActive) this.onRevoked();
  }

  requireCurrent(candidate: DappRouteLease): DappRouteLease {
    if (!this.isCurrent(candidate)) throw new StaleDappRouteLeaseError();
    return this.lease as DappRouteLease;
  }

  isCurrent(candidate: DappRouteLease): boolean {
    return (
      this.lease !== null &&
      candidate !== null &&
      typeof candidate === 'object' &&
      candidate.walletId === this.lease.walletId &&
      candidate.routeEpoch === this.lease.routeEpoch &&
      candidate.networkGenesis === this.lease.networkGenesis
    );
  }

  async completeAuthorizedSubmission<T>(
    authorizedLease: DappRouteLease,
    submission: Promise<T>
  ): Promise<T> {
    const result = await submission;
    this.requireCurrent(authorizedLease);
    return result;
  }
}
