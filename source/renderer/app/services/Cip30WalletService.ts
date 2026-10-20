import type {
  Cip30WalletNetwork,
  Cip30WalletRequest,
  Cip30WalletResponse,
} from '../../../common/cip30/executor';
import type { Api } from '../api';
import WalletAddress, {
  getFirstReceivingAddress,
} from '../domains/WalletAddress';
import { bindCip30WalletRenderer } from '../ipc/cip30Wallet';
import { reportWalletApprovalProgress } from '../ipc/nativeTransactionApproval';
import type { StoresMap } from '../stores';
import { validateDappTransactionContext } from '../api/transactions/dappBackend';
import { reconcileTransactionContext } from '../../../common/cardano/transactionContext';
import { prepareHardwareTransaction } from '../utils/hardwareWalletTransaction';
import { prepareHardwareMessage } from '../utils/hardwareWalletMessage';

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

type TransactionLease = {
  identity: Cip30WalletRequest;
  release?: () => void;
  releaseRequested: boolean;
  inFlight: number;
};

export class Cip30WalletService {
  private unbind?: () => void;
  private disposed = false;
  private readonly transactionLeases = new Map<string, TransactionLease>();

  private matchesLease(lease: TransactionLease, request: Cip30WalletRequest) {
    const captured = lease.identity;
    return (
      captured.walletId === request.walletId &&
      captured.sourceRevision === request.sourceRevision &&
      captured.network.networkId === request.network.networkId &&
      captured.network.networkMagic === request.network.networkMagic &&
      captured.network.genesisHash === request.network.genesisHash
    );
  }

  private releaseLease(lockId: string, lease: TransactionLease): void {
    if (lease.releaseRequested && lease.release && lease.inFlight === 0) {
      lease.release();
      this.transactionLeases.delete(lockId);
    }
  }

  constructor(
    private readonly api: Api,
    private readonly stores: StoresMap
  ) {}

  setup(): void {
    if (!global.cip30Enabled) return;
    this.disposed = false;
    this.unbind = bindCip30WalletRenderer(this.receive);
  }

  teardown(): void {
    this.disposed = true;
    this.transactionLeases.forEach((lease, lockId) => {
      lease.releaseRequested = true;
      this.releaseLease(lockId, lease);
    });
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

    const wallet = this.currentWallet(request);
    const canonical =
      wallet?.singleAddressMode && !wallet.isLegacy
        ? getFirstReceivingAddress(addresses)
        : undefined;
    const ordered = [...addresses].sort((left, right) =>
      left.spendingPath.localeCompare(right.spendingPath)
    );
    const projected = canonical ? [canonical] : ordered;
    const used = projected.filter(({ used: isUsed }) => isUsed);
    const unused = projected.filter(({ used: isUsed }) => !isUsed);
    const change =
      canonical || unused[unused.length - 1] || ordered[ordered.length - 1];
    const reward = this.stores.addresses.stakeAddresses[request.walletId];
    if ((wallet?.singleAddressMode && !canonical) || !change || !reward)
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
    if (!global.cip30Enabled)
      return Object.freeze({ status: 'rejected', reason: 'unavailable' });
    if (request.operation === 'release-transaction-lock') {
      const lease = this.transactionLeases.get(request.lockId);
      if (lease && !this.matchesLease(lease, request))
        return Object.freeze({ status: 'rejected', reason: 'account-change' });
      if (lease) {
        lease.releaseRequested = true;
        this.releaseLease(request.lockId, lease);
      }
      return Object.freeze({
        status: 'fulfilled',
        operation: request.operation,
        value: Object.freeze({ lockId: request.lockId }),
      });
    }
    if (this.disposed)
      return Object.freeze({ status: 'rejected', reason: 'unavailable' });
    if (request.operation === 'acquire-transaction-lock') {
      if (!this.ready(request)) return this.rejection(request, 'unavailable');
      if (this.transactionLeases.has(request.lockId))
        return Object.freeze({ status: 'rejected', reason: 'internal' });
      const lease: TransactionLease = {
        identity: Object.freeze({
          ...request,
          network: Object.freeze({ ...request.network }),
        }),
        releaseRequested: false,
        inFlight: 0,
      };
      this.transactionLeases.set(request.lockId, lease);
      try {
        const acquired = await this.stores.transactions.acquireWalletSendLock(
          request.walletId
        );
        lease.release = acquired.release;
        if (this.disposed || lease.releaseRequested || !this.ready(request)) {
          lease.releaseRequested = true;
          this.releaseLease(request.lockId, lease);
          return this.rejection(request, 'unavailable');
        }
        return Object.freeze({
          status: 'fulfilled',
          operation: request.operation,
          value: Object.freeze({ lockId: request.lockId }),
        });
      } catch {
        this.transactionLeases.delete(request.lockId);
        return this.rejection(request, 'internal');
      }
    }
    const transactionLease =
      request.operation === 'sign-transactions' ||
      request.operation === 'submit-transaction'
        ? this.transactionLeases.get(request.lockId)
        : undefined;
    if (
      (request.operation === 'sign-transactions' ||
        request.operation === 'submit-transaction') &&
      (!transactionLease ||
        !transactionLease.release ||
        transactionLease.releaseRequested ||
        !this.matchesLease(transactionLease, request))
    )
      return Object.freeze({ status: 'rejected', reason: 'unavailable' });
    if (
      request.operation !== 'submit-transaction' &&
      !this.currentWallet(request)
    )
      return Object.freeze({ status: 'rejected', reason: 'account-change' });
    if (request.operation !== 'submit-transaction' && !this.ready(request))
      return Object.freeze({ status: 'rejected', reason: 'unavailable' });

    if (transactionLease) transactionLease.inFlight += 1;
    try {
      if (request.operation === 'submit-transaction') {
        const submission = await this.api.ada.submitDappTransaction({
          walletId: request.walletId,
          request: {
            revision: 1,
            network: backendNetwork(request.network),
            transaction: request.transaction,
          },
        });
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
        const hardware = wallet.isHardwareWallet
          ? await this.stores.hardwareWallets
              .refreshDappConnectorCapability(wallet.id)
              .catch(() => undefined)
          : undefined;
        return Object.freeze({
          status: 'fulfilled',
          operation: 'capabilities',
          value: Object.freeze({
            walletId: wallet.id,
            walletName: wallet.name,
            walletKind,
            network: request.network,
            backendApiVersion: capabilities.api_version,
            backendExtensions: Object.freeze([95, 103, 104]),
            ...(hardware ? { hardware } : {}),
          }),
        });
      }

      if (request.operation === 'sign-data') {
        const wallet = this.currentWallet(request);
        if (!wallet) return this.rejection(request, 'unavailable');
        if (wallet.isHardwareWallet) {
          if (!request.hardware)
            return Object.freeze({
              status: 'rejected',
              reason: 'proof-generation',
            });
          const context = await this.api.ada.getDappTransactionContext({
            walletId: request.walletId,
            request: {
              revision: 1,
              network: backendNetwork(request.network),
              transactions: [],
            },
          });
          const snapshot = reconcileTransactionContext(context, {
            walletId: request.walletId,
            network: request.network,
            transactions: [],
          });
          const message = prepareHardwareMessage(
            request.address,
            request.payload,
            request.network,
            snapshot.ownership,
            request.drepCredential
          );
          const signature = await this.stores.hardwareWallets.signDappData(
            wallet.id,
            message,
            request.hardware
          );
          if (!this.ready(request))
            return this.rejection(request, 'unavailable');
          return Object.freeze({
            status: 'fulfilled',
            operation: 'sign-data',
            value: Object.freeze({
              revision: 1 as const,
              credential_kind: message.credentialKind,
              credential: message.credential,
              cose_sign1: signature.signature,
              cose_key: signature.key,
            }),
          });
        }
        if (!request.passphrase)
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

      if (request.operation === 'account-public-key') {
        const wallet = this.currentWallet(request);
        if (!wallet)
          return Object.freeze({
            status: 'rejected',
            reason: 'account-change',
          });
        let accountPublicKey: string | null = null;
        if (wallet.isHardwareWallet && request.hardware)
          accountPublicKey =
            this.stores.hardwareWallets.getDappAccountPublicKey(
              wallet.id,
              request.hardware
            );
        if (!wallet.isHardwareWallet && request.passphrase)
          accountPublicKey = await this.api.ada.getAccountPublicKey({
            walletId: request.walletId,
            index: '0H',
            passphrase: request.passphrase,
            extended: true,
          });
        if (!accountPublicKey)
          return Object.freeze({
            status: 'rejected',
            reason: 'proof-generation',
          });
        if (!this.ready(request)) return this.rejection(request, 'unavailable');
        return Object.freeze({
          status: 'fulfilled',
          operation: 'account-public-key',
          value: accountPublicKey,
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
            await reportWalletApprovalProgress(
              request.approvalRequestId,
              'waiting-for-device',
              index
            ).catch(() => undefined);
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
              witnessSetCbor =
                await this.stores.hardwareWallets.signDappTransaction(
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
        await reportWalletApprovalProgress(
          request.approvalRequestId,
          'signing'
        ).catch(() => undefined);
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
        if (code === 'DataSignError.UserDeclined')
          return Object.freeze({
            status: 'rejected',
            reason: 'user-declined',
          });
        if (code === 'DataSignError.ProofGeneration')
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
    } finally {
      if (
        transactionLease &&
        (request.operation === 'sign-transactions' ||
          request.operation === 'submit-transaction')
      ) {
        transactionLease.inFlight -= 1;
        this.releaseLease(request.lockId, transactionLease);
      }
    }
  };
}
