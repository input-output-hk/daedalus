import { blake2b } from 'blakejs';
import { bytesForSpan } from '../cardano/cborSlices';
import type { SemanticTransaction } from '../cardano/transaction';

export const CIP30_REVIEW_EFFECTS = [
  'input',
  'collateral-input',
  'reference-input',
  'output',
  'fee',
  'mint',
  'burn',
  'withdrawal',
  'certificate',
  'redeemer',
  'native-script',
  'plutus-script',
  'datum',
  'vote',
  'proposal',
  'collateral-return',
  'required-signers',
  'network-id',
  'validity-interval',
  'transaction-validity',
  'auxiliary-data',
  'auxiliary-data-hash',
  'script-data-hash',
  'treasury-value',
  'donation',
  'maximum-collateral-loss',
  'maximum-collateral-loss-unresolved',
] as const;

export type Cip30ReviewEffectKind = typeof CIP30_REVIEW_EFFECTS[number];
export type Cip30TransactionReview = Readonly<{
  mode: 'sign' | 'submit';
  transactionId: string;
  bodyCbor: string;
  fullCbor: string;
  fullCborDigest: string;
  witnessSetCbor: string;
  auxiliaryDataCbor: string;
  isValid: boolean;
  effects: readonly Readonly<{
    index: number;
    kind: string;
    value: string;
  }>[];
  maximumCollateralLoss?: string;
  existingVkeyWitnesses: readonly string[];
  existingBootstrapWitnesses: readonly string[];
  auxiliaryDataHash?: string;
  scriptDataHash?: string;
  commitmentsVerified: boolean;
  approvable: boolean;
  refusalReasons: readonly string[];
}>;

const stable = (value: unknown): unknown => {
  if (typeof value === 'bigint') return value.toString();
  if (Buffer.isBuffer(value)) return value.toString('hex');
  if (Array.isArray(value)) return value.map(stable);
  if (value && typeof value === 'object')
    return Object.keys(value)
      .sort()
      .reduce<Record<string, unknown>>((result, key) => {
        const item = (value as Record<string, unknown>)[key];
        if (item !== undefined) result[key] = stable(item);
        return result;
      }, {});
  return value;
};

const display = (value: unknown): string =>
  JSON.stringify(stable(value), null, 2);

export const createCip30TransactionReview = (
  transaction: SemanticTransaction,
  mode: 'sign' | 'submit'
): Cip30TransactionReview => {
  const { envelope } = transaction;
  const bodyCbor = bytesForSpan(envelope.cbor, envelope.spans.body).toString(
    'hex'
  );
  const fullCbor = envelope.cbor.toString('hex');
  const refusalReasons = transaction.review.complete
    ? []
    : transaction.review.requirements.map(
        ({ kind, target, reason }) => `${kind}:${target}:${reason}`
      );
  transaction.effects.forEach(({ kind }) => {
    if (!(CIP30_REVIEW_EFFECTS as readonly string[]).includes(kind))
      refusalReasons.push(`unsupported-effect:${kind}`);
    if (kind === 'maximum-collateral-loss-unresolved')
      refusalReasons.push('maximum-collateral-loss-unresolved');
  });
  if (transaction.collateral.maximumLossRequirement)
    refusalReasons.push('maximum-collateral-loss-unresolved');

  return Object.freeze({
    mode,
    transactionId: transaction.transactionId,
    bodyCbor,
    fullCbor,
    fullCborDigest: Buffer.from(blake2b(envelope.cbor, undefined, 32)).toString(
      'hex'
    ),
    witnessSetCbor: bytesForSpan(
      envelope.cbor,
      envelope.spans.witnessSet
    ).toString('hex'),
    auxiliaryDataCbor: bytesForSpan(
      envelope.cbor,
      envelope.spans.auxiliaryData
    ).toString('hex'),
    isValid: envelope.isValid,
    effects: Object.freeze(
      transaction.effects.map(({ kind, value }, index) =>
        Object.freeze({ index, kind, value: display(value) })
      )
    ),
    ...(transaction.collateral.maximumLoss
      ? { maximumCollateralLoss: display(transaction.collateral.maximumLoss) }
      : {}),
    existingVkeyWitnesses: Object.freeze([...transaction.witnesses.vkeys]),
    existingBootstrapWitnesses: Object.freeze([
      ...transaction.witnesses.bootstrap,
    ]),
    ...(transaction.commitments.auxiliaryDataHash
      ? { auxiliaryDataHash: transaction.commitments.auxiliaryDataHash }
      : {}),
    ...(transaction.commitments.scriptDataHash
      ? { scriptDataHash: transaction.commitments.scriptDataHash }
      : {}),
    commitmentsVerified: transaction.review.complete,
    approvable:
      transaction.review.complete &&
      transaction.review.signable &&
      refusalReasons.length === 0,
    refusalReasons: Object.freeze([...new Set(refusalReasons)]),
  });
};

const own = (
  value: unknown,
  keys: readonly string[]
): value is Record<string, unknown> => {
  if (!value || typeof value !== 'object' || Array.isArray(value)) return false;
  const prototype = Object.getPrototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) return false;
  if (Object.getOwnPropertySymbols(value).length) return false;
  const descriptors = Object.getOwnPropertyDescriptors(value);
  return (
    Object.keys(descriptors).sort().join(',') === [...keys].sort().join(',') &&
    keys.every((key) =>
      Object.prototype.hasOwnProperty.call(descriptors[key], 'value')
    )
  );
};
const text = (value: unknown): value is string =>
  typeof value === 'string' && value.length > 0;
const texts = (value: unknown): value is string[] =>
  Array.isArray(value) && value.every((item) => typeof item === 'string');

export const parseCip30TransactionReview = (
  value: unknown
): Cip30TransactionReview => {
  if (
    !own(
      value,
      [
        'mode',
        'transactionId',
        'bodyCbor',
        'fullCbor',
        'fullCborDigest',
        'witnessSetCbor',
        'auxiliaryDataCbor',
        'isValid',
        'effects',
        'maximumCollateralLoss',
        'existingVkeyWitnesses',
        'existingBootstrapWitnesses',
        'auxiliaryDataHash',
        'scriptDataHash',
        'commitmentsVerified',
        'approvable',
        'refusalReasons',
      ].filter((key) => Object.prototype.hasOwnProperty.call(value, key))
    ) ||
    (value.mode !== 'sign' && value.mode !== 'submit') ||
    !/^[0-9a-f]{64}$/u.test(value.transactionId as string) ||
    !text(value.bodyCbor) ||
    !text(value.fullCbor) ||
    !/^[0-9a-f]{64}$/u.test(value.fullCborDigest as string) ||
    !text(value.witnessSetCbor) ||
    !text(value.auxiliaryDataCbor) ||
    typeof value.isValid !== 'boolean' ||
    !Array.isArray(value.effects) ||
    !value.effects.every(
      (effect, index) =>
        own(effect, ['index', 'kind', 'value']) &&
        effect.index === index &&
        text(effect.kind) &&
        text(effect.value)
    ) ||
    (value.maximumCollateralLoss !== undefined &&
      !text(value.maximumCollateralLoss)) ||
    !texts(value.existingVkeyWitnesses) ||
    !texts(value.existingBootstrapWitnesses) ||
    (value.auxiliaryDataHash !== undefined && !text(value.auxiliaryDataHash)) ||
    (value.scriptDataHash !== undefined && !text(value.scriptDataHash)) ||
    typeof value.commitmentsVerified !== 'boolean' ||
    typeof value.approvable !== 'boolean' ||
    !texts(value.refusalReasons)
  )
    throw new Error('Invalid CIP-30 transaction review');
  return Object.freeze(value as Cip30TransactionReview);
};
