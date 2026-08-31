import fs from 'fs';
import cbor from 'cbor';

import { decodeConwayTransaction } from '../../../source/common/cardano/transaction';
import { parseConwayTransactionEnvelope } from '../../../source/common/cardano/transactionEnvelope';
import type {
  Cip103PreflightBatch,
  Cip103PreflightItem,
} from '../../../source/common/types/cip103.types';
import { submitCip103Batch } from '../../../source/main/cip30/extensions/cip103';

const logPath = process.argv[2];
if (!logPath) throw new Error('Missing process-exit log path');

const item = (index: number): Cip103PreflightItem => {
  const envelope = parseConwayTransactionEnvelope(
    cbor.encodeCanonical([
      new Map<number, unknown>([
        [0, []],
        [1, []],
        [2, index],
      ]),
      new Map(),
      true,
      null,
    ])
  );
  return Object.freeze({
    index,
    cbor: envelope.cbor.toString('hex'),
    fullCborDigest: `${index + 1}`.repeat(64),
    bodyHash: envelope.transactionId,
    envelope,
    transaction: decodeConwayTransaction(envelope),
  });
};

const batch: Cip103PreflightBatch = Object.freeze({
  state: 'preflighted',
  operation: 'submit',
  items: Object.freeze([item(0), item(1), item(2)]),
});
const review = Object.freeze({
  mode: 'submit' as const,
  approvable: true,
  items: Object.freeze(
    batch.items.map(({ index, bodyHash, fullCborDigest, cbor: fullCbor }) =>
      Object.freeze({
        index,
        transaction: Object.freeze({
          transactionId: bodyHash,
          fullCborDigest,
          fullCbor,
        }),
      })
    )
  ),
});

void submitCip103Batch({
  batch,
  review,
  submitTransaction: async (_cbor, index) => {
    fs.appendFileSync(logPath, `${index}\n`);
    if (index === 0) process.exit(73);
    return {
      revision: 1,
      transaction_id: batch.items[index].bodyHash,
      status: 'submitted',
    };
  },
});
