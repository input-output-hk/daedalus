import { DappCapability, DappGrantLaunch } from '../../common/types/dapp.types';
import { canonicalizeDappOrigin } from '../dapp/urlPolicy';
import type { GrantIdentity } from './GrantRepository';

export type CapabilityIdentity = Readonly<
  Pick<
    DappCapability,
    | 'guestWebContentsId'
    | 'documentGeneration'
    | 'origin'
    | 'connectionId'
    | 'walletId'
    | 'routeEpoch'
    | 'networkId'
    | 'networkMagic'
    | 'networkGenesis'
  >
>;

export type CapabilityRequirement = CapabilityIdentity &
  Readonly<{
    launch: DappGrantLaunch;
    requiredExtensions: readonly number[];
    requiredScopes: readonly DappCapability['grantedScopes'][number][];
  }>;

const matches = (capability: DappCapability, identity: CapabilityIdentity) =>
  capability.guestWebContentsId === identity.guestWebContentsId &&
  capability.documentGeneration === identity.documentGeneration &&
  capability.origin === canonicalizeDappOrigin(identity.origin) &&
  capability.connectionId === identity.connectionId &&
  capability.walletId === identity.walletId &&
  capability.routeEpoch === identity.routeEpoch &&
  capability.networkId === identity.networkId &&
  capability.networkMagic === identity.networkMagic &&
  capability.networkGenesis === identity.networkGenesis;

const matchesLaunch = (capability: DappCapability, launch: DappGrantLaunch) =>
  launch.kind === 'diagnostics'
    ? capability.dappId === undefined
    : capability.dappId === launch.catalogEntryId;

export class SessionStore {
  private readonly capabilities = new Map<string, DappCapability>();

  create(value: DappCapability): DappCapability {
    const capability = Object.freeze({
      ...value,
      origin: canonicalizeDappOrigin(value.origin),
      enabledExtensions: Object.freeze([...value.enabledExtensions]),
      grantedScopes: Object.freeze([...value.grantedScopes]),
    });
    this.revokeGuest(capability.guestWebContentsId);
    this.capabilities.set(capability.connectionId, capability);
    return capability;
  }

  get(requirement: CapabilityRequirement): DappCapability | undefined {
    try {
      const capability = this.capabilities.get(requirement.connectionId);
      return capability &&
        matches(capability, requirement) &&
        matchesLaunch(capability, requirement.launch) &&
        requirement.requiredExtensions.every((cip) =>
          capability.enabledExtensions.includes(cip)
        ) &&
        requirement.requiredScopes.every((scope) =>
          capability.grantedScopes.includes(scope)
        )
        ? capability
        : undefined;
    } catch {
      return undefined;
    }
  }

  currentForGuest(guestWebContentsId: number): DappCapability | undefined {
    for (const capability of this.capabilities.values()) {
      if (capability.guestWebContentsId === guestWebContentsId)
        return capability;
    }
    return undefined;
  }

  revoke(connectionId: string): void {
    this.capabilities.delete(connectionId);
  }

  revokeGuest(guestWebContentsId: number): void {
    this.revokeWhere(
      (capability) => capability.guestWebContentsId === guestWebContentsId
    );
  }

  revokeWallet(walletId: string): void {
    this.revokeWhere((capability) => capability.walletId === walletId);
  }

  revokeNetwork(networkGenesis: string): void {
    this.revokeWhere(
      (capability) => capability.networkGenesis === networkGenesis
    );
  }

  revokeRoute(walletId: string, routeEpoch?: number): void {
    this.revokeWhere(
      (capability) =>
        capability.walletId === walletId &&
        (routeEpoch === undefined || capability.routeEpoch === routeEpoch)
    );
  }

  revokeOrigin(origin: string): void {
    const canonicalOrigin = canonicalizeDappOrigin(origin);
    this.revokeWhere((capability) => capability.origin === canonicalOrigin);
  }

  revokeGrant(identity: GrantIdentity): void {
    const origin = canonicalizeDappOrigin(identity.origin);
    this.revokeWhere(
      (capability) =>
        capability.origin === origin &&
        capability.walletId === identity.walletId &&
        capability.networkGenesis === identity.networkGenesis &&
        matchesLaunch(capability, identity.launch)
    );
  }

  revokeAll(): void {
    this.capabilities.clear();
  }

  private revokeWhere(
    // eslint-disable-next-line no-unused-vars
    predicate: (capability: DappCapability) => boolean
  ): void {
    this.capabilities.forEach((capability, connectionId) => {
      if (predicate(capability)) this.capabilities.delete(connectionId);
    });
  }
}
