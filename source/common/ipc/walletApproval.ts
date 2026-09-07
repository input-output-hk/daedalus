import type {
  WalletApprovalPresentation,
  WalletApprovalRenderMainRequest,
} from './api';
import { parseCip30TransactionReview } from '../cip30/review';
import { parseCip103BatchReview } from '../cip30/cip103Review';
import { parseCip8DataSignReview } from '../cardano/cip8';

const isRecord = (value: unknown): value is Record<string, unknown> =>
  value !== null && typeof value === 'object' && !Array.isArray(value);
const hasKeys = (value: Record<string, unknown>, keys: readonly string[]) =>
  Object.keys(value).sort().join('\0') === [...keys].sort().join('\0');
const isText = (value: unknown): value is string =>
  typeof value === 'string' && value.length > 0;
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
  const transaction =
    value.kind === 'transaction-sign' || value.kind === 'transaction-submit';
  const batch = value.kind === 'batch-sign' || value.kind === 'batch-submit';
  const hasReview = transaction || batch || value.kind === 'data-sign';
  if (
    !hasKeys(value, [
      'requestId',
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

export const parseWalletApprovalRender = (
  value: unknown
): WalletApprovalRenderMainRequest => {
  if (!isRecord(value)) throw new Error('Invalid dApp consent render request');
  if (
    value.type === 'terminal' &&
    isText(value.requestId) &&
    hasKeys(value, ['type', 'requestId'])
  )
    return Object.freeze({ type: 'terminal', requestId: value.requestId });
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
