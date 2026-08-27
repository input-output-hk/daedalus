import {
  controlledCip30Utxos,
  getCip30Balance,
  getCip30Utxos,
  normalizeCip30Address,
} from '../../common/cardano/cip30Serialization';
import { selectCip30Collateral } from '../../common/cardano/collateralSelection';
import {
  DappNetwork,
  reconcileTransactionContext,
  TransactionContextSnapshot,
} from '../../common/cardano/transactionContext';
import type {
  Cip30WalletAddresses,
  Cip30WalletResponse,
} from '../../common/cip30/executor';
import type { ApiError, DappCip30Rejection } from '../../common/cip30/errors';
import type {
  DappCip30GatewayRequest,
  DappCip30Method,
  Paginate,
} from '../../common/cip30/wire';
import type {
  DappCapability,
  DappGrantLaunch,
} from '../../common/types/dapp.types';
import type { CapabilityContext } from './CapabilityService';
import { CapabilityService } from './CapabilityService';
import { SessionStore } from './SessionStore';

export class Cip30DispatchRejection {
  constructor(readonly rejection: DappCip30Rejection) {}
}

export type Cip30DispatchAuthority = Readonly<{
  guestWebContentsId: number;
  documentGeneration: number;
  origin: string;
  launch: DappGrantLaunch;
  walletId: string;
  routeEpoch: number;
  network: DappNetwork;
}>;

export type Cip30WalletExecutor = (
  operation: 'context' | 'addresses'
) => Promise<Cip30WalletResponse>;

const apiError = (code: ApiError['code'], info: string): never => {
  throw new Cip30DispatchRejection({
    type: 'api-error',
    value: { code, info },
  });
};

const executorFailure = (response: Cip30WalletResponse): never => {
  if (response.status !== 'rejected') return apiError(-2, 'Internal error');
  return response.reason === 'account-change'
    ? apiError(-4, 'Account changed')
    : apiError(-2, 'Internal error');
};

const normalizeAddresses = (
  addresses: readonly string[],
  networkId: 0 | 1
): readonly string[] =>
  Object.freeze(
    addresses.map((address) => normalizeCip30Address(address, networkId))
  );

const paginateAddresses = (
  addresses: readonly string[],
  paginate?: Paginate
): readonly string[] => {
  if (!paginate) return addresses;
  if (paginate.page > Math.floor(Number.MAX_SAFE_INTEGER / paginate.limit)) {
    throw new Cip30DispatchRejection({
      type: 'paginate-error',
      value: { maxSize: addresses.length },
    });
  }
  const start = paginate.page * paginate.limit;
  if (start > addresses.length) {
    throw new Cip30DispatchRejection({
      type: 'paginate-error',
      value: { maxSize: addresses.length },
    });
  }
  return addresses.slice(start, start + paginate.limit);
};

export class Dispatcher {
  constructor(
    private readonly capabilities: CapabilityService,
    private readonly sessions: SessionStore
  ) {}

  requireCapability(
    method: DappCip30Method,
    authority: Cip30DispatchAuthority,
    context: CapabilityContext
  ): DappCapability {
    const current = this.sessions.currentForGuest(authority.guestWebContentsId);
    if (!current) return apiError(-3, 'Refused');
    let resolved: ReturnType<CapabilityService['requireInvocation']>;
    try {
      resolved = this.capabilities.requireInvocation(
        method,
        current.enabledExtensions,
        context
      );
    } catch {
      return apiError(-3, 'Refused');
    }
    const requiredExtension = resolved.extension ?? resolved.override;
    const capability = this.sessions.get({
      guestWebContentsId: authority.guestWebContentsId,
      documentGeneration: authority.documentGeneration,
      origin: authority.origin,
      connectionId: current.connectionId,
      walletId: authority.walletId,
      routeEpoch: authority.routeEpoch,
      networkId: authority.network.networkId,
      networkMagic: authority.network.networkMagic,
      networkGenesis: authority.network.genesisHash,
      launch: authority.launch,
      requiredExtensions:
        requiredExtension === undefined ? [] : [requiredExtension],
      requiredScopes: resolved.descriptor.scopes,
    });
    return capability ?? apiError(-3, 'Refused');
  }

  private async snapshot(
    authority: Cip30DispatchAuthority,
    execute: Cip30WalletExecutor
  ): Promise<TransactionContextSnapshot> {
    const response = await execute('context');
    if (response.status !== 'fulfilled' || response.operation !== 'context')
      return executorFailure(response);
    try {
      return reconcileTransactionContext(response.value, {
        walletId: authority.walletId,
        network: authority.network,
        transactions: [],
      });
    } catch {
      return apiError(-2, 'Internal error');
    }
  }

  private async addresses(
    execute: Cip30WalletExecutor
  ): Promise<Cip30WalletAddresses> {
    const response = await execute('addresses');
    if (response.status !== 'fulfilled' || response.operation !== 'addresses')
      return executorFailure(response);
    return response.value;
  }

  async dispatch(
    request: DappCip30GatewayRequest,
    authority: Cip30DispatchAuthority,
    context: CapabilityContext,
    execute: Cip30WalletExecutor
  ): Promise<unknown> {
    const capability = this.requireCapability(
      request.method,
      authority,
      context
    );
    switch (request.method) {
      case 'api.getExtensions':
        return capability.enabledExtensions.map((cip) => ({ cip }));
      case 'api.getNetworkId':
        return authority.network.networkId;
      case 'api.cip142.getNetworkMagic':
        return authority.network.networkMagic;
      case 'api.getUtxos': {
        const [amount, paginate] = request.args;
        const result = getCip30Utxos(
          await this.snapshot(authority, execute),
          amount,
          paginate
        );
        if (result?.kind === 'paginate-error') {
          throw new Cip30DispatchRejection({
            type: 'paginate-error',
            value: { maxSize: result.maxSize },
          });
        }
        return result?.items ?? null;
      }
      case 'api.getCollateral': {
        const [params] = request.args;
        const snapshot = await this.snapshot(authority, execute);
        if (snapshot.maxCollateralInputs === undefined)
          return apiError(-2, 'Internal error');
        return selectCip30Collateral(
          controlledCip30Utxos(snapshot),
          params.amount,
          snapshot.maxCollateralInputs
        );
      }
      case 'api.getBalance':
        return getCip30Balance(await this.snapshot(authority, execute));
      case 'api.getUsedAddresses': {
        const [paginate] = request.args;
        const source = await this.addresses(execute);
        return paginateAddresses(
          normalizeAddresses(source.used, authority.network.networkId),
          paginate
        );
      }
      case 'api.getUnusedAddresses':
        return normalizeAddresses(
          (await this.addresses(execute)).unused,
          authority.network.networkId
        );
      case 'api.getChangeAddress':
        return normalizeCip30Address(
          (await this.addresses(execute)).change,
          authority.network.networkId
        );
      case 'api.getRewardAddresses':
        return normalizeAddresses(
          (await this.addresses(execute)).reward,
          authority.network.networkId
        );
      default:
        return apiError(-3, 'Refused');
    }
  }
}
