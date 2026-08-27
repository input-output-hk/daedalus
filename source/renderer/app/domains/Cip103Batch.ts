import { blake2b } from 'blakejs';
import {
  decodeConwayTransaction,
  SemanticTransaction,
} from '../../../common/cardano/transaction';
import {
  ExactTransactionEnvelope,
  parseConwayTransactionEnvelope,
} from '../../../common/cardano/transactionEnvelope';
import { invalidRequest } from '../../../common/cip30/errors';
import {
  parseCip103SignRequest,
  parseCip103SubmitRequest,
} from '../../../common/cip30/schemas/cip103';
import type {
  Cip103Operation,
  Cip103PreflightBatch,
  Cip103PreflightItem,
} from '../../../common/types/cip103.types';

const inspect = (
  cbor: string,
  index: number,
  networkId: 0 | 1,
  partialSign?: boolean
): Cip103PreflightItem => {
  try {
    const bytes = Buffer.from(cbor, 'hex');
    const envelope: ExactTransactionEnvelope = parseConwayTransactionEnvelope(
      bytes
    );
    const transaction: SemanticTransaction = decodeConwayTransaction(envelope);
    if (
      transaction.networkId !== undefined &&
      transaction.networkId !== networkId
    ) {
      throw invalidRequest();
    }
    return Object.freeze({
      index,
      cbor,
      fullCborDigest: Buffer.from(blake2b(bytes, undefined, 32)).toString(
        'hex'
      ),
      bodyHash: envelope.transactionId,
      partialSign,
      envelope,
      transaction,
    });
  } catch (_) {
    throw invalidRequest();
  }
};

const batch = (
  operation: Cip103Operation,
  items: readonly Cip103PreflightItem[]
): Cip103PreflightBatch =>
  Object.freeze({
    state: 'preflighted',
    operation,
    items: Object.freeze(items),
  });

export const preflightCip103Sign = (
  value: unknown,
  networkId: 0 | 1
): Cip103PreflightBatch =>
  batch(
    'sign',
    parseCip103SignRequest(value).map(({ cbor, partialSign }, index) =>
      inspect(cbor, index, networkId, partialSign)
    )
  );

export const preflightCip103Submit = (
  value: unknown,
  networkId: 0 | 1
): Cip103PreflightBatch =>
  batch(
    'submit',
    parseCip103SubmitRequest(value).map((cbor, index) =>
      inspect(cbor, index, networkId)
    )
  );
