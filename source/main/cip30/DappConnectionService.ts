import type {
  DappConnectionIdentity,
  DappConnectionScope,
  DappConnectionsMainResponse,
} from '../../common/ipc/api';
import type { ConsentCoordinator, ConsentIdentity } from './ConsentCoordinator';
import type { GrantRepository } from './GrantRepository';
import type { SessionStore } from './SessionStore';

export class DappConnectionService {
  constructor(
    private readonly grants: GrantRepository,
    private readonly sessions: SessionStore,
    private readonly consent: ConsentCoordinator
  ) {}

  snapshot(): DappConnectionsMainResponse {
    return Object.freeze({
      corrupt: this.grants.isCorrupt,
      grants: Object.freeze([...this.grants.list()]),
    });
  }

  disconnect(identity: DappConnectionIdentity): DappConnectionsMainResponse {
    const grant = this.grants.find(identity);
    if (grant) this.revoke(identity, grant.origin);
    return this.snapshot();
  }

  forget(identity: DappConnectionIdentity): DappConnectionsMainResponse {
    const grant = this.grants.find(identity);
    if (grant) {
      this.revoke(identity, grant.origin);
      this.grants.forget(identity);
    }
    return this.snapshot();
  }

  revokeScope(
    identity: DappConnectionIdentity,
    scope: DappConnectionScope
  ): DappConnectionsMainResponse {
    const grant = this.grants.find(identity);
    if (grant?.readScopes.includes(scope)) {
      this.revoke(identity, grant.origin);
      this.grants.revokeScopes(identity, [scope]);
    }
    return this.snapshot();
  }

  removeWallet(walletId: string): DappConnectionsMainResponse {
    this.cancelWallet(walletId);
    this.sessions.revokeWallet(walletId);
    this.grants.removeWallet(walletId);
    return this.snapshot();
  }

  pruneWallets(walletIds: readonly string[]): DappConnectionsMainResponse {
    const retained = new Set(walletIds);
    new Set(
      this.grants
        .list()
        .map((grant) => grant.walletId)
        .filter((walletId) => !retained.has(walletId))
    ).forEach((walletId) => {
      this.cancelWallet(walletId);
      this.sessions.revokeWallet(walletId);
    });
    this.grants.pruneWallets(retained);
    return this.snapshot();
  }

  repair(): DappConnectionsMainResponse {
    this.consent.cancel();
    this.sessions.revokeAll();
    this.grants.repair();
    return this.snapshot();
  }

  private revoke(identity: DappConnectionIdentity, origin: string): void {
    this.consent.cancel((pending) => this.matches(pending, identity, origin));
    this.sessions.revokeGrant(identity);
  }

  private cancelWallet(walletId: string): void {
    this.consent.cancel((pending) => pending.walletId === walletId);
  }

  private matches(
    pending: ConsentIdentity,
    identity: DappConnectionIdentity,
    origin: string
  ): boolean {
    return (
      pending.origin === origin &&
      pending.walletId === identity.walletId &&
      pending.networkGenesis === identity.networkGenesis
    );
  }
}
