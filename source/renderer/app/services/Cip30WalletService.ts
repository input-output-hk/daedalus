import type {
  Cip30WalletNetwork,
  Cip30WalletRequest,
  Cip30WalletResponse,
} from '../../../common/cip30/executor';
import type { Api } from '../api';
import type WalletAddress from '../domains/WalletAddress';
import { bindCip30WalletRenderer } from '../ipc/cip30Wallet';
import type { StoresMap } from '../stores';

const backendNetwork = (network: Cip30WalletNetwork) => ({
  network_id: network.networkId,
  network_magic: network.networkMagic,
  genesis_hash: network.genesisHash,
});

export class Cip30WalletService {
  private unbind?: () => void;

  constructor(private readonly api: Api, private readonly stores: StoresMap) {}

  setup(): void {
    this.unbind = bindCip30WalletRenderer(this.receive);
  }

  teardown(): void {
    this.unbind?.();
    this.unbind = undefined;
  }

  private currentWallet(request: Cip30WalletRequest) {
    const wallet = this.stores.wallets.activeDappWallet;
    return wallet?.id === request.walletId ? wallet : null;
  }

  private ready(request: Cip30WalletRequest): boolean {
    return (
      this.currentWallet(request) !== null &&
      this.stores.networkStatus.isConnected &&
      this.stores.networkStatus.isSynced
    );
  }

  private rejection(
    request: Cip30WalletRequest,
    fallback: 'unavailable' | 'internal'
  ): Cip30WalletResponse {
    return Object.freeze({
      status: 'rejected',
      reason: this.currentWallet(request) ? fallback : 'account-change',
    });
  }

  private addresses = async (
    request: Cip30WalletRequest
  ): Promise<Cip30WalletResponse> => {
    const addresses = (await this.api.ada.getAddresses({
      walletId: request.walletId,
      isLegacy: false,
    })) as WalletAddress[];
    await this.stores.addresses._getStakeAddress(request.walletId, false);
    if (!this.ready(request)) return this.rejection(request, 'unavailable');

    const ordered = [...addresses].sort((left, right) =>
      left.spendingPath.localeCompare(right.spendingPath)
    );
    const used = ordered.filter(({ used: isUsed }) => isUsed);
    const unused = ordered.filter(({ used: isUsed }) => !isUsed);
    const change = unused[unused.length - 1] || ordered[ordered.length - 1];
    const reward = this.stores.addresses.stakeAddresses[request.walletId];
    if (!change || !reward)
      return Object.freeze({ status: 'rejected', reason: 'internal' });

    return Object.freeze({
      status: 'fulfilled',
      operation: 'addresses',
      value: Object.freeze({
        walletId: request.walletId,
        network: request.network,
        used: Object.freeze(used.map(({ id }) => id)),
        unused: Object.freeze(unused.map(({ id }) => id)),
        change: change.id,
        reward: Object.freeze([reward]),
      }),
    });
  };

  receive = async (
    request: Cip30WalletRequest
  ): Promise<Cip30WalletResponse> => {
    if (!this.currentWallet(request))
      return Object.freeze({ status: 'rejected', reason: 'account-change' });
    if (!this.ready(request))
      return Object.freeze({ status: 'rejected', reason: 'unavailable' });

    try {
      if (request.operation === 'capabilities') {
        const capabilities = await this.api.ada.getDappCapabilities({
          sourceRevision: request.sourceRevision,
          network: backendNetwork(request.network),
        });
        const wallet = this.currentWallet(request);
        if (!wallet) return this.rejection(request, 'unavailable');
        if (!this.ready(request)) return this.rejection(request, 'unavailable');
        let walletKind: 'shelley-software' | 'ledger' | 'trezor' =
          'shelley-software';
        if (wallet.isHardwareWallet) {
          walletKind = this.stores.hardwareWallets.checkIsTrezorByWalletId(
            wallet.id
          )
            ? 'trezor'
            : 'ledger';
        }
        return Object.freeze({
          status: 'fulfilled',
          operation: 'capabilities',
          value: Object.freeze({
            walletId: wallet.id,
            walletName: wallet.name,
            walletKind,
            network: request.network,
            backendApiVersion: capabilities.api_version,
            backendExtensions: Object.freeze([95, 103]),
          }),
        });
      }

      if (request.operation === 'context') {
        const context = await this.api.ada.getDappTransactionContext({
          walletId: request.walletId,
          request: {
            revision: 1,
            network: backendNetwork(request.network),
            transactions: [],
          },
        });
        if (!this.ready(request)) return this.rejection(request, 'unavailable');
        return Object.freeze({
          status: 'fulfilled',
          operation: 'context',
          value: context,
        });
      }

      return await this.addresses(request);
    } catch {
      return this.rejection(request, 'internal');
    }
  };
}
