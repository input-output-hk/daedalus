import {
  NATIVE_TRANSACTION_EXECUTE_CHANNEL,
  NativeTransactionExecuteMainRequest,
  NativeTransactionExecuteRendererResponse,
  NativeTransactionPresentation,
  WALLET_TRANSACTION_APPROVAL_CHANNEL,
  WalletTransactionApprovalMainResponse,
  WalletTransactionApprovalRendererRequest,
} from '../../common/ipc/api';
import { decodeConwayTransaction } from '../../common/cardano/transaction';
import { parseConwayTransactionEnvelope } from '../../common/cardano/transactionEnvelope';
import { createCip30TransactionReview } from '../../common/cip30/review';
import {
  createNativePlanReviewDisplay,
  nativeApprovalBindingDigest,
  parseNativePreparedApproval,
  parseNativeTransactionPlan,
} from '../../common/transactions/nativePlan';
import type {
  NativeApprovalResult,
  NativePreparedApproval,
} from '../../common/transactions/nativePlan';
import { isFlight } from '../config';
import { consentCoordinator } from './walletApproval';
import { MainIpcChannel } from './lib/MainIpcChannel';

const channel = new MainIpcChannel<
  WalletTransactionApprovalRendererRequest,
  WalletTransactionApprovalMainResponse
>(WALLET_TRANSACTION_APPROVAL_CHANNEL);
const executor = new MainIpcChannel<
  NativeTransactionExecuteRendererResponse,
  NativeTransactionExecuteMainRequest
>(NATIVE_TRANSACTION_EXECUTE_CHANNEL);
type Attempt = {
  requestId?: string;
  bindingDigest: string;
  submissionAuthorized: boolean;
};
const attempts = new Map<string, Attempt>();
const nativeFailure = (code: string, info = code) =>
  Object.freeze({
    type: 'native-transaction-error' as const,
    value: Object.freeze({ code, info }),
  });

const presentation = (
  attemptId: string,
  prepared: NativePreparedApproval
): Omit<NativeTransactionPresentation, 'requestId'> => {
  const items: Array<NativeTransactionPresentation['items'][number]> = [];
  prepared.items.forEach((item) => {
    if (item.kind === 'native-plan') {
      const plan = parseNativeTransactionPlan(item.planCbor, prepared);
      plan.items.forEach((_, index) =>
        items.push(
          Object.freeze({
            kind: 'native-plan' as const,
            display: createNativePlanReviewDisplay(plan, index),
            planCbor: item.planCbor,
            planDigest: item.planDigest,
          })
        )
      );
      return;
    }
    const transaction = decodeConwayTransaction(
      parseConwayTransactionEnvelope(Buffer.from(item.cbor, 'hex'))
    );
    const review = createCip30TransactionReview(
      transaction,
      'sign',
      item.transactionContext
    );
    items.push(
      Object.freeze({
        kind: 'exact-cbor' as const,
        review: Object.freeze({
          ...review,
          display: Object.freeze({
            ...review.display,
            deposits: item.selectionFacts.deposits,
            refunds: item.selectionFacts.refunds,
          }),
        }),
      })
    );
  });
  return Object.freeze({
    kind: 'native-transaction',
    attemptId,
    walletName: prepared.context.walletName,
    networkName: prepared.context.networkName,
    action: prepared.action,
    authorization:
      prepared.authorization === 'software'
        ? Object.freeze({ kind: 'software' as const })
        : Object.freeze({
            kind: 'hardware' as const,
            vendor: prepared.authorization,
          }),
    collection: prepared.action === 'migration' ? 'migration' : 'single',
    acknowledgements: Object.freeze([
      ...(isFlight &&
      (prepared.action === 'payment' ||
        prepared.action === 'collateral-preparation')
        ? (['flight-mainnet-funds'] as const)
        : []),
      ...(prepared.action === 'undelegate'
        ? (['undelegation-network-support', 'undelegation-rewards'] as const)
        : []),
    ]),
    ...(prepared.context.destinationWalletName
      ? { destinationWalletName: prepared.context.destinationWalletName }
      : {}),
    items: Object.freeze(items),
  });
};

const isNativeFailure = (
  value: unknown
): value is ReturnType<typeof nativeFailure> =>
  !!value &&
  typeof value === 'object' &&
  'type' in value &&
  value.type === 'native-transaction-error' &&
  'value' in value &&
  !!value.value &&
  typeof value.value === 'object' &&
  'code' in value.value &&
  typeof value.value.code === 'string';

const requestNative = async (
  raw: Extract<
    WalletTransactionApprovalRendererRequest,
    { type: 'request-native' }
  >,
  sender: NonNullable<Parameters<typeof executor.request>[1]>
): Promise<WalletTransactionApprovalMainResponse> => {
  if (attempts.has(raw.attemptId))
    return Object.freeze({ status: 'stale' as const });
  const prepared = parseNativePreparedApproval(raw.prepared);
  if (
    raw.walletId !== prepared.walletId ||
    raw.network.networkId !== prepared.network.networkId ||
    raw.network.networkMagic !== prepared.network.networkMagic ||
    raw.network.genesisHash !== prepared.network.genesisHash
  )
    throw new Error('Native transaction request binding mismatch');
  if (!('id' in sender) || typeof sender.id !== 'number')
    throw new Error('Missing trusted renderer identity');
  const bindingDigest = nativeApprovalBindingDigest(prepared);
  const attempt: Attempt = { bindingDigest, submissionAuthorized: false };
  attempts.set(raw.attemptId, attempt);
  try {
    const result = await consentCoordinator.request<NativeApprovalResult>({
      identity: {
        kind: 'native',
        trustedWebContentsId: sender.id,
        attemptId: raw.attemptId,
        walletId: prepared.walletId,
        networkGenesis: prepared.network.genesisHash,
      },
      presentation: presentation(raw.attemptId, prepared),
      payload: Object.freeze({ attemptId: raw.attemptId, bindingDigest }),
      declined: nativeFailure('user_declined', 'Transaction was rejected'),
      onCreated: (requestId) => {
        attempt.requestId = requestId;
      },
      execute: async (_payload, signal, passphrase, context) => {
        if (signal.aborted) throw nativeFailure('cancelled');
        const result = await executor.request(
          {
            type: 'execute',
            attemptId: raw.attemptId,
            requestId: context.requestId,
            bindingDigest,
            ...(passphrase === undefined ? {} : { passphrase }),
          },
          sender
        );
        if (signal.aborted && !attempt.submissionAuthorized)
          throw nativeFailure('cancelled');
        return result;
      },
    });
    return Object.freeze({
      status: attempt.submissionAuthorized
        ? ('submission-authorized' as const)
        : ('accepted' as const),
      requestId: attempt.requestId,
      bindingDigest,
      result,
    });
  } catch (error) {
    const result: NativeApprovalResult = Object.freeze({
      status: 'rejected',
      errorCode: isNativeFailure(error) ? error.value.code : 'failed',
    });
    return Object.freeze({
      status: 'accepted' as const,
      requestId: attempt.requestId,
      bindingDigest,
      result,
    });
  } finally {
    attempts.delete(raw.attemptId);
  }
};

export const handleNativeTransactionApprovalRequests = (): void => {
  channel.onRequest(async (value, event) => {
    if (!event) throw new Error('Missing wallet approval sender');
    if (!value || typeof value !== 'object' || !('type' in value))
      throw new Error('Invalid wallet approval control request');
    if (value.type === 'request-native')
      return requestNative(value, event.sender);
    if (value.type === 'progress')
      return Object.freeze({
        status: consentCoordinator.reportProgress(
          value.requestId,
          value.phase,
          value.itemIndex
        ),
      });
    const attempt = attempts.get(value.attemptId);
    if (!attempt || (value.requestId && attempt.requestId !== value.requestId))
      return Object.freeze({ status: 'stale' as const });
    if (value.type === 'commit-native-submission') {
      const status = consentCoordinator.markSubmissionAuthorized(
        value.requestId
      );
      if (status === 'submission-authorized')
        attempt.submissionAuthorized = true;
      return Object.freeze({ status });
    }
    if (attempt.submissionAuthorized)
      return Object.freeze({ status: 'submission-authorized' as const });
    consentCoordinator.cancel(
      (identity) =>
        identity.kind === 'native' && identity.attemptId === value.attemptId,
      nativeFailure('cancelled')
    );
    return Object.freeze({ status: 'accepted' as const });
  });
};
