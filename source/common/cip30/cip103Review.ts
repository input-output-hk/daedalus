import type {
  Cip103InputRole,
  Cip103Resolution,
} from '../cardano/transactionOverlay';
import type { SemanticTransaction } from '../cardano/transaction';
import type { Cip103PreflightBatch } from '../types/cip103.types';
import {
  createCip30TransactionReview,
  parseCip30TransactionReview,
} from './review';
import type { Cip30TransactionReview } from './review';

export type Cip103ReviewDependency = Readonly<{
  source: 'current-batch' | 'pending-submission';
  inputRole: Cip103InputRole;
  outpoint: Readonly<{ transactionId: string; index: number }>;
  sourceTransactionIndex?: number;
}>;

export type Cip103BatchItemReview = Readonly<{
  index: number;
  dependencies: readonly Cip103ReviewDependency[];
  conflicts: readonly Readonly<{
    inputRole: Cip103InputRole;
    outpoint: Readonly<{ transactionId: string; index: number }>;
    earlierTransactionIndex: number;
  }>[];
  transaction: Cip30TransactionReview;
}>;

export type Cip103BatchReview = Readonly<{
  mode: 'sign' | 'submit';
  approvable: boolean;
  refusalIndex?: number;
  items: readonly Cip103BatchItemReview[];
}>;

const invalid = (): never => {
  throw new Error('Invalid CIP-103 batch review');
};

export const createCip103BatchReview = (
  batch: Cip103PreflightBatch,
  resolution: Cip103Resolution,
  transactions: readonly SemanticTransaction[]
): Cip103BatchReview => {
  if (
    batch.items.length !== resolution.items.length ||
    batch.items.length !== transactions.length
  )
    return invalid();
  const mode = batch.operation;
  const items = batch.items.map((item, index) => {
    const resolved = resolution.items[index];
    if (
      item.index !== index ||
      resolved.transactionIndex !== index ||
      item.bodyHash !== resolved.bodyHash ||
      item.fullCborDigest !== resolved.fullCborDigest
    )
      return invalid();
    const transaction = createCip30TransactionReview(transactions[index], mode);
    if (
      transaction.transactionId !== item.bodyHash ||
      transaction.fullCborDigest !== item.fullCborDigest
    )
      return invalid();
    const dependencies = [
      ...resolved.inputs.normal,
      ...resolved.inputs.collateral,
      ...resolved.inputs.reference,
    ]
      .filter(({ source }) => source === 'earlier' || source === 'pending')
      .map(({ source, inputRole, outpoint, sourceTransactionIndex }) =>
        Object.freeze({
          source: source === 'earlier' ? 'current-batch' : 'pending-submission',
          inputRole,
          outpoint,
          ...(sourceTransactionIndex === undefined
            ? {}
            : { sourceTransactionIndex }),
        })
      );
    return Object.freeze({
      index,
      dependencies: Object.freeze(dependencies),
      conflicts: Object.freeze(
        resolved.conflicts.map(
          ({ inputRole, outpoint, earlierTransactionIndex }) =>
            Object.freeze({ inputRole, outpoint, earlierTransactionIndex })
        )
      ),
      transaction,
    });
  });
  const refusalIndex = items.findIndex(
    ({ transaction: item }) => !item.approvable
  );
  return Object.freeze({
    mode,
    approvable: refusalIndex === -1,
    ...(refusalIndex === -1 ? {} : { refusalIndex }),
    items: Object.freeze(items),
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

const inputRole = (value: unknown): value is Cip103InputRole =>
  value === 'normal' || value === 'collateral' || value === 'reference';

const parseOutpoint = (
  value: unknown
): Readonly<{ transactionId: string; index: number }> => {
  if (
    !own(value, ['transactionId', 'index']) ||
    typeof value.transactionId !== 'string' ||
    !/^[0-9a-f]{64}$/u.test(value.transactionId) ||
    typeof value.index !== 'number' ||
    !Number.isSafeInteger(value.index) ||
    value.index < 0
  )
    return invalid();
  return Object.freeze({
    transactionId: value.transactionId,
    index: value.index,
  });
};

export const parseCip103BatchReview = (value: unknown): Cip103BatchReview => {
  if (
    !own(
      value,
      ['mode', 'approvable', 'refusalIndex', 'items'].filter((key) =>
        Object.prototype.hasOwnProperty.call(value, key)
      )
    ) ||
    (value.mode !== 'sign' && value.mode !== 'submit') ||
    typeof value.approvable !== 'boolean' ||
    !Array.isArray(value.items) ||
    value.items.length === 0 ||
    value.items.length > 50
  )
    return invalid();
  const mode = value.mode;
  const items = value.items.map((item, index) => {
    if (
      !own(item, ['index', 'dependencies', 'conflicts', 'transaction']) ||
      item.index !== index ||
      !Array.isArray(item.dependencies) ||
      !Array.isArray(item.conflicts)
    )
      return invalid();
    const transaction = parseCip30TransactionReview(item.transaction);
    if (transaction.mode !== mode) return invalid();
    const dependencies = item.dependencies.map((dependency) => {
      if (
        !own(
          dependency,
          [
            'source',
            'inputRole',
            'outpoint',
            'sourceTransactionIndex',
          ].filter((key) =>
            Object.prototype.hasOwnProperty.call(dependency, key)
          )
        ) ||
        !inputRole(dependency.inputRole) ||
        (dependency.source !== 'current-batch' &&
          dependency.source !== 'pending-submission')
      )
        return invalid();
      const sourceTransactionIndex = dependency.sourceTransactionIndex;
      if (dependency.source === 'current-batch') {
        if (
          typeof sourceTransactionIndex !== 'number' ||
          !Number.isSafeInteger(sourceTransactionIndex) ||
          sourceTransactionIndex < 0 ||
          sourceTransactionIndex >= index
        )
          return invalid();
        return Object.freeze({
          source: 'current-batch',
          inputRole: dependency.inputRole,
          outpoint: parseOutpoint(dependency.outpoint),
          sourceTransactionIndex,
        });
      }
      if (sourceTransactionIndex !== undefined) return invalid();
      return Object.freeze({
        source: 'pending-submission',
        inputRole: dependency.inputRole,
        outpoint: parseOutpoint(dependency.outpoint),
      });
    });
    const conflicts = item.conflicts.map((conflict) => {
      if (
        !own(conflict, ['inputRole', 'outpoint', 'earlierTransactionIndex']) ||
        !inputRole(conflict.inputRole) ||
        typeof conflict.earlierTransactionIndex !== 'number' ||
        !Number.isSafeInteger(conflict.earlierTransactionIndex) ||
        conflict.earlierTransactionIndex < 0 ||
        conflict.earlierTransactionIndex >= index
      )
        return invalid();
      return Object.freeze({
        inputRole: conflict.inputRole,
        outpoint: parseOutpoint(conflict.outpoint),
        earlierTransactionIndex: conflict.earlierTransactionIndex,
      });
    });
    return Object.freeze({
      index,
      dependencies: Object.freeze(dependencies),
      conflicts: Object.freeze(conflicts),
      transaction,
    });
  });
  const refusalIndex = items.findIndex(
    ({ transaction }) => !transaction.approvable
  );
  if (
    value.approvable !== (refusalIndex === -1) ||
    (refusalIndex === -1
      ? value.refusalIndex !== undefined
      : value.refusalIndex !== refusalIndex)
  )
    return invalid();
  return Object.freeze({
    mode,
    approvable: value.approvable,
    ...(refusalIndex === -1 ? {} : { refusalIndex }),
    items: Object.freeze(items),
  });
};
