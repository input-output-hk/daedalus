import type {
  NativeTransactionPresentation,
  WalletApprovalPresentation,
  WalletApprovalRenderMainRequest,
  WalletApprovalResult,
} from './api';
import { parseCip30TransactionReview } from '../cip30/review';
import { parseCip103BatchReview } from '../cip30/cip103Review';
import { parseCip8DataSignReview } from '../cardano/cip8';
import {
  NATIVE_TRANSACTION_ACTIONS,
  nativeTransactionPlanDigest,
  parseNativeApprovalResult,
} from '../transactions/nativePlan';
import { parseTransactionReviewDisplay } from '../transactions/reviewDisplay';

const isRecord = (value: unknown): value is Record<string, unknown> =>
  value !== null && typeof value === 'object' && !Array.isArray(value);
const hasKeys = (value: Record<string, unknown>, keys: readonly string[]) =>
  Object.keys(value).sort().join('\0') === [...keys].sort().join('\0');
const isText = (value: unknown): value is string =>
  typeof value === 'string' && value.length > 0;
const isWalletId = (value: unknown): value is string =>
  typeof value === 'string' && /^[0-9a-f]{40}$/u.test(value);
const parseAuthorization = (value: unknown) => {
  if (!isRecord(value))
    throw new Error('Invalid wallet approval authorization');
  if (value.kind === 'software' && hasKeys(value, ['kind']))
    return Object.freeze({ kind: 'software' as const });
  if (
    value.kind === 'hardware' &&
    (value.vendor === 'ledger' || value.vendor === 'trezor') &&
    hasKeys(value, ['kind', 'vendor'])
  )
    return Object.freeze({ kind: 'hardware' as const, vendor: value.vendor });
  if (value.kind === 'none' && hasKeys(value, ['kind']))
    return Object.freeze({ kind: 'none' as const });
  throw new Error('Invalid wallet approval authorization');
};

const parsePresentation = (value: unknown): WalletApprovalPresentation => {
  if (!isRecord(value)) throw new Error('Invalid dApp consent presentation');
  if (value.kind === 'native-transaction') {
    if (
      !hasKeys(value, [
        'requestId',
        'walletId',
        'kind',
        'attemptId',
        'walletName',
        'networkName',
        'action',
        'authorization',
        'collection',
        'acknowledgements',
        'items',
        ...(value.destinationWalletName === undefined
          ? []
          : ['destinationWalletName']),
      ]) ||
      !isText(value.requestId) ||
      !isWalletId(value.walletId) ||
      !isText(value.attemptId) ||
      !isText(value.walletName) ||
      !isText(value.networkName) ||
      !isText(value.action) ||
      !(NATIVE_TRANSACTION_ACTIONS as readonly string[]).includes(
        value.action
      ) ||
      value.collection !==
        (value.action === 'migration' ? 'migration' : 'single') ||
      (value.destinationWalletName !== undefined &&
        !isText(value.destinationWalletName)) ||
      !Array.isArray(value.acknowledgements) ||
      !value.acknowledgements.every((acknowledgement) =>
        [
          'flight-mainnet-funds',
          'undelegation-network-support',
          'undelegation-rewards',
        ].includes(acknowledgement)
      ) ||
      !Array.isArray(value.items) ||
      value.items.length === 0
    )
      throw new Error('Invalid native transaction presentation');
    const authorization = parseAuthorization(value.authorization);
    if (authorization.kind === 'none')
      throw new Error('Invalid native transaction authorization');
    const items = value.items.map(
      (item): NativeTransactionPresentation['items'][number] => {
        if (!isRecord(item))
          throw new Error('Invalid native transaction review');
        if (item.kind === 'exact-cbor' && hasKeys(item, ['kind', 'review'])) {
          const review = parseCip30TransactionReview(item.review);
          if (review.mode !== 'sign')
            throw new Error('Invalid native transaction review mode');
          return Object.freeze({ kind: 'exact-cbor', review });
        }
        if (
          item.kind === 'native-plan' &&
          hasKeys(item, ['kind', 'display', 'planCbor', 'planDigest']) &&
          isText(item.planCbor) &&
          isText(item.planDigest) &&
          item.planDigest === nativeTransactionPlanDigest(item.planCbor)
        )
          return Object.freeze({
            kind: 'native-plan',
            display: parseTransactionReviewDisplay(item.display),
            planCbor: item.planCbor,
            planDigest: item.planDigest,
          });
        throw new Error('Invalid native transaction review');
      }
    );
    return Object.freeze({
      kind: 'native-transaction',
      requestId: value.requestId,
      walletId: value.walletId,
      attemptId: value.attemptId,
      walletName: value.walletName,
      networkName: value.networkName,
      action: value.action as NativeTransactionPresentation['action'],
      authorization,
      collection: value.collection as NativeTransactionPresentation['collection'],
      acknowledgements: Object.freeze([...value.acknowledgements]),
      ...(value.destinationWalletName === undefined
        ? {}
        : { destinationWalletName: value.destinationWalletName as string }),
      items: Object.freeze(items),
    });
  }
  const transaction =
    value.kind === 'transaction-sign' || value.kind === 'transaction-submit';
  const batch = value.kind === 'batch-sign' || value.kind === 'batch-submit';
  const hasReview = transaction || batch || value.kind === 'data-sign';
  if (
    !hasKeys(value, [
      'requestId',
      'walletId',
      'kind',
      'origin',
      'walletName',
      'networkName',
      'scopes',
      'extensions',
      ...(hasReview ? ['review'] : []),
      ...(transaction || batch ? ['authorization'] : []),
    ]) ||
    !isText(value.requestId) ||
    !isWalletId(value.walletId) ||
    ![
      'connection',
      'key-disclosure',
      'data-sign',
      'transaction-sign',
      'transaction-submit',
      'batch-sign',
      'batch-submit',
    ].includes(value.kind as string) ||
    !isText(value.origin) ||
    !isText(value.walletName) ||
    !isText(value.networkName) ||
    !Array.isArray(value.scopes) ||
    !value.scopes.every(isText) ||
    !Array.isArray(value.extensions) ||
    !value.extensions.every(
      (extension) => Number.isSafeInteger(extension) && extension > 0
    )
  )
    throw new Error('Invalid dApp consent presentation');
  const identity = {
    requestId: value.requestId,
    walletId: value.walletId,
    kind: value.kind,
    origin: value.origin,
    walletName: value.walletName,
    networkName: value.networkName,
    scopes: Object.freeze([...value.scopes]),
    extensions: Object.freeze([...value.extensions]),
  };
  if (transaction) {
    const kind = value.kind as 'transaction-sign' | 'transaction-submit';
    const review = parseCip30TransactionReview(value.review);
    const authorization = parseAuthorization(value.authorization);
    if (
      (kind === 'transaction-sign' &&
        (review.mode !== 'sign' || authorization.kind === 'none')) ||
      (kind === 'transaction-submit' &&
        (review.mode !== 'submit' || authorization.kind !== 'none'))
    )
      throw new Error('Invalid wallet approval presentation');
    return Object.freeze({ ...identity, kind, authorization, review });
  }
  if (batch) {
    const kind = value.kind as 'batch-sign' | 'batch-submit';
    const review = parseCip103BatchReview(value.review);
    const authorization = parseAuthorization(value.authorization);
    if (
      (kind === 'batch-sign' &&
        (review.mode !== 'sign' || authorization.kind === 'none')) ||
      (kind === 'batch-submit' &&
        (review.mode !== 'submit' || authorization.kind !== 'none'))
    )
      throw new Error('Invalid wallet approval presentation');
    return Object.freeze({ ...identity, kind, authorization, review });
  }
  if (value.kind === 'data-sign')
    return Object.freeze({
      ...identity,
      kind: 'data-sign',
      review: parseCip8DataSignReview(value.review),
    });
  return Object.freeze({
    ...identity,
    kind: value.kind as 'connection' | 'key-disclosure',
  });
};
export const parseWalletApprovalResult = (
  value: unknown
): WalletApprovalResult => {
  if (
    isRecord(value) &&
    value.status === 'signed' &&
    hasKeys(value, ['status', 'transactionIds']) &&
    Array.isArray(value.transactionIds) &&
    value.transactionIds.length > 0 &&
    value.transactionIds.every(
      (transactionId) =>
        typeof transactionId === 'string' &&
        /^[0-9a-f]{64}$/u.test(transactionId)
    )
  )
    return Object.freeze({
      status: 'signed',
      transactionIds: Object.freeze([...value.transactionIds]),
    });
  return parseNativeApprovalResult(value);
};

export const parseWalletApprovalRender = (
  value: unknown
): WalletApprovalRenderMainRequest => {
  if (!isRecord(value)) throw new Error('Invalid dApp consent render request');
  if (
    value.type === 'terminal' &&
    isText(value.requestId) &&
    hasKeys(value, [
      'type',
      'requestId',
      ...(value.result === undefined ? [] : ['result']),
    ])
  )
    return Object.freeze({
      type: 'terminal',
      requestId: value.requestId,
      ...(value.result === undefined
        ? {}
        : { result: parseWalletApprovalResult(value.result) }),
    });
  if (
    value.type === 'progress' &&
    isText(value.requestId) &&
    ['signing', 'waiting-for-device', 'submitting'].includes(
      value.phase as string
    ) &&
    typeof value.submissionAuthorized === 'boolean' &&
    (value.itemIndex === undefined ||
      (Number.isSafeInteger(value.itemIndex) &&
        Number(value.itemIndex) >= 0)) &&
    hasKeys(value, [
      'type',
      'requestId',
      'phase',
      ...(value.itemIndex === undefined ? [] : ['itemIndex']),
      'submissionAuthorized',
    ])
  )
    return Object.freeze({
      type: 'progress',
      requestId: value.requestId,
      phase: value.phase as 'signing' | 'waiting-for-device' | 'submitting',
      ...(value.itemIndex === undefined
        ? {}
        : { itemIndex: Number(value.itemIndex) }),
      submissionAuthorized: value.submissionAuthorized,
    });
  if (value.type === 'present' && hasKeys(value, ['type', 'request']))
    return Object.freeze({
      type: 'present',
      request: parsePresentation(value.request),
    });
  throw new Error('Invalid dApp consent render request');
};
