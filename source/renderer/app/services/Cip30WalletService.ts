import type {
  Cip30WalletNetwork,
  Cip30WalletRequest,
  Cip30WalletResponse,
} from '../../../common/cip30/executor';
import type { Api } from '../api';
import type WalletAddress from '../domains/WalletAddress';
import { bindCip30WalletRenderer } from '../ipc/cip30Wallet';
import type { StoresMap } from '../stores';
import { validateDappTransactionContext } from '../api/transactions/dappBackend';
import { reconcileTransactionContext } from '../../../common/cardano/transactionContext';
import { prepareHardwareTransaction } from '../utils/hardwareWalletTransaction';

const backendNetwork = (network: Cip30WalletNetwork) => ({
  network_id: network.networkId,
  network_magic: network.networkMagic,
  genesis_hash: network.genesisHash,
});

const outpointMatches = (
  preferred: readonly Readonly<{ transactionId: string; index: number }>[],
  input: Readonly<{ id: string; index: number }>
): boolean =>
  preferred.some(
    ({ transactionId, index }) =>
      transactionId === input.id && index === input.index
  );

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
    if (
      request.operation !== 'submit-transaction' &&
      !this.currentWallet(request)
    )
      return Object.freeze({ status: 'rejected', reason: 'account-change' });
    if (request.operation !== 'submit-transaction' && !this.ready(request))
      return Object.freeze({ status: 'rejected', reason: 'unavailable' });

    try {
      if (request.operation === 'submit-transaction') {
        const submission = await this.stores.transactions.withWalletSendLock(
          request.walletId,
          () =>
            this.api.ada.submitDappTransaction({
              walletId: request.walletId,
              request: {
                revision: 1,
                network: backendNetwork(request.network),
                transaction: request.transaction,
              },
            })
        );
        return Object.freeze({
          status: 'fulfilled',
          operation: 'submit-transaction',
          value: submission,
        });
      }

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

      if (request.operation === 'sign-data') {
        const wallet = this.currentWallet(request);
        if (!wallet) return this.rejection(request, 'unavailable');
        if (wallet.isHardwareWallet)
          return Object.freeze({
            status: 'rejected',
            reason: 'proof-generation',
          });
        const signature = await this.api.ada.signDappData({
          walletId: request.walletId,
          request: {
            revision: 1,
            network: backendNetwork(request.network),
            address: request.address,
            payload: request.payload,
            passphrase: request.passphrase,
          },
        });
        if (!this.ready(request)) return this.rejection(request, 'unavailable');
        return Object.freeze({
          status: 'fulfilled',
          operation: 'sign-data',
          value: signature,
        });
      }

      if (request.operation === 'cip95-key-state') {
        const keyState = await this.api.ada.getDappCip95KeyState(
          request.walletId
        );
        if (!this.ready(request)) return this.rejection(request, 'unavailable');
        return Object.freeze({
          status: 'fulfilled',
          operation: 'cip95-key-state',
          value: keyState,
        });
      }

      if (request.operation === 'collateral-history') {
        const history = await this.api.ada.getDappCollateralHistory(
          request.walletId
        );
        if (!this.ready(request)) return this.rejection(request, 'unavailable');
        return Object.freeze({
          status: 'fulfilled',
          operation: 'collateral-history',
          value: Object.freeze({
            transactions: Object.freeze(
              history
                .filter(
                  ({ inputs, collateral = [] }) =>
                    inputs.some((input) =>
                      outpointMatches(request.preferredInputs, input)
                    ) ||
                    collateral.some((input) =>
                      outpointMatches(request.preferredInputs, input)
                    )
                )
                .map(
                  ({
                    id,
                    status,
                    script_validity: scriptValidity = null,
                    inputs,
                    collateral = [],
                  }) =>
                    Object.freeze({
                      transactionId: id,
                      status,
                      scriptValidity,
                      normalInputs: Object.freeze(
                        inputs.map(({ id: transactionId, index }) =>
                          Object.freeze({ transactionId, index })
                        )
                      ),
                      collateralInputs: Object.freeze(
                        collateral.map(({ id: transactionId, index }) =>
                          Object.freeze({ transactionId, index })
                        )
                      ),
                    })
                )
            ),
          }),
        });
      }

      if (request.operation === 'transaction-context') {
        const context = await this.api.ada.getDappTransactionContext({
          walletId: request.walletId,
          request: {
            revision: 1,
            network: backendNetwork(request.network),
            transactions: [...request.transactions],
          },
        });
        if (!this.ready(request)) return this.rejection(request, 'unavailable');
        return Object.freeze({
          status: 'fulfilled',
          operation: 'transaction-context',
          value: context,
        });
      }

      if (request.operation === 'sign-transactions') {
        const wallet = this.currentWallet(request);
        if (!wallet) return this.rejection(request, 'unavailable');
        if (wallet.isHardwareWallet) {
          const snapshot = reconcileTransactionContext(request.context, {
            walletId: request.walletId,
            network: request.network,
            transactions: request.transactions.map(({ cbor }) => cbor),
          });
          const witnesses = [];
          for (const [
            index,
            { partialSign },
          ] of request.transactions.entries()) {
            const preparation = prepareHardwareTransaction(
              snapshot,
              index,
              partialSign,
              this.stores.hardwareWallets.getDappTransactionCapability(
                wallet.id
              )
            );
            let witnessSetCbor: string;
            try {
              witnessSetCbor = await this.stores.hardwareWallets.signDappTransaction(
                wallet.id,
                preparation
              );
            } catch {
              return Object.freeze({
                status: 'rejected',
                reason: 'tx-proof-generation',
              });
            }
            witnesses.push(
              Object.freeze({
                transaction_index: index,
                body_hash: preparation.exact.bodyHash,
                witness_set_cbor: witnessSetCbor,
              })
            );
          }
          if (!this.ready(request))
            return this.rejection(request, 'unavailable');
          return Object.freeze({
            status: 'fulfilled',
            operation: 'sign-transactions',
            value: Object.freeze({
              revision: 1 as const,
              witnesses: Object.freeze(witnesses),
            }),
          });
        }
        if (!request.passphrase)
          return Object.freeze({
            status: 'rejected',
            reason: 'tx-proof-generation',
          });
        const witnesses = await this.api.ada.signDappTransactions({
          walletId: request.walletId,
          request: {
            revision: 1,
            context: validateDappTransactionContext(request.context),
            transactions: request.transactions.map(({ cbor, partialSign }) => ({
              cbor,
              partial_sign: partialSign,
            })),
            passphrase: request.passphrase,
          },
        });
        if (!this.ready(request)) return this.rejection(request, 'unavailable');
        return Object.freeze({
          status: 'fulfilled',
          operation: 'sign-transactions',
          value: witnesses,
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
    } catch (error) {
      if (request.operation === 'sign-data') {
        const code = (error as { code?: unknown })?.code;
        if (code === 'dapp_data_address_not_pk')
          return Object.freeze({
            status: 'rejected',
            reason: 'address-not-pk',
          });
        if (code === 'dapp_data_proof_generation')
          return Object.freeze({
            status: 'rejected',
            reason: 'proof-generation',
          });
        if (code === 'dapp_account_changed')
          return Object.freeze({
            status: 'rejected',
            reason: 'account-change',
          });
      }
      if (request.operation === 'cip95-key-state') {
        const code = (error as { code?: unknown })?.code;
        if (code === 'dapp_account_changed')
          return Object.freeze({
            status: 'rejected',
            reason: 'account-change',
          });
        if (code === 'dapp_context_unavailable')
          return Object.freeze({
            status: 'rejected',
            reason: 'unavailable',
          });
      }
      if (request.operation === 'sign-transactions') {
        const code = (error as { code?: unknown })?.code;
        if (code === 'dapp_tx_proof_generation')
          return Object.freeze({
            status: 'rejected',
            reason: 'tx-proof-generation',
          });
        if (code === 'dapp_deprecated_certificate')
          return Object.freeze({
            status: 'rejected',
            reason: 'deprecated-certificate',
          });
        if (code === 'dapp_account_changed')
          return Object.freeze({
            status: 'rejected',
            reason: 'account-change',
          });
        if (code === 'dapp_context_unavailable')
          return Object.freeze({
            status: 'rejected',
            reason: 'unavailable',
          });
      }
      if (request.operation === 'submit-transaction') {
        const code = (error as { code?: unknown })?.code;
        if (
          code === 'dapp_submission_failed' ||
          code === 'dapp_account_changed' ||
          code === 'dapp_context_unavailable'
        )
          return Object.freeze({
            status: 'rejected',
            reason: 'tx-send-failure',
          });
        return Object.freeze({ status: 'rejected', reason: 'internal' });
      }
      return this.rejection(request, 'internal');
    }
  };
}
