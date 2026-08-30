import cbor from 'cbor';

import { decodeConwayTransaction } from '../../../common/cardano/transaction';
import { parseConwayTransactionEnvelope } from '../../../common/cardano/transactionEnvelope';
import type { Cip30WalletSubmissionResponse } from '../../../common/cip30/executor';
import type {
  Cip103PreflightBatch,
  Cip103PreflightItem,
} from '../../../common/types/cip103.types';
import {
  Cip103BatchSubmissionRequest,
  Cip103ExecutionReview,
  submitCip103Batch,
} from './cip103';

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
const review: Cip103ExecutionReview = Object.freeze({
  mode: 'submit',
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

const success = (
  index: number,
  status: Cip30WalletSubmissionResponse['status'] = 'submitted'
): Cip30WalletSubmissionResponse => ({
  revision: 1,
  transaction_id: batch.items[index].bodyHash,
  status,
});

const request = (
  submitTransaction: Cip103BatchSubmissionRequest['submitTransaction'],
  selectedReview = review
): Cip103BatchSubmissionRequest => ({
  batch,
  review: selectedReview,
  submitTransaction,
});

describe('CIP-103 batch submission', () => {
  it('resolves exact aligned hashes after every ordered attempt succeeds', async () => {
    const statuses: Cip30WalletSubmissionResponse['status'][] = [
      'authorized',
      'outcome_unknown',
      'in_ledger',
    ];
    const submitTransaction = jest.fn(async (_cbor: string, index: number) =>
      success(index, statuses[index])
    );

    const result = await submitCip103Batch(request(submitTransaction));

    expect(submitTransaction.mock.calls.map(([, index]) => index)).toEqual([
      0,
      1,
      2,
    ]);
    expect(
      submitTransaction.mock.calls.map(([transaction]) => transaction)
    ).toEqual(batch.items.map(({ cbor: transaction }) => transaction));
    expect(result).toEqual(batch.items.map(({ bodyHash }) => bodyHash));
    expect(Object.isFrozen(result)).toBe(true);
  });

  it.each([0, 1, 2])(
    'attempts every later item and rejects directly with the aligned array when index %i fails',
    async (failedIndex) => {
      const submitTransaction = jest.fn(
        async (
          _cbor: string,
          index: number
        ): Promise<Cip30WalletSubmissionResponse> => {
          if (index === failedIndex)
            return Promise.reject(
              Object.freeze({ code: 2, info: `Failed at ${index}` })
            );
          return success(index);
        }
      );

      const rejection = await submitCip103Batch(
        request(submitTransaction)
      ).catch((error) => error);

      expect(submitTransaction.mock.calls.map(([, index]) => index)).toEqual([
        0,
        1,
        2,
      ]);
      expect(rejection).not.toBeInstanceOf(Error);
      expect(rejection).toEqual(
        batch.items.map(({ bodyHash }, index) =>
          index === failedIndex
            ? { code: 2, info: `Failed at ${index}` }
            : bodyHash
        )
      );
    }
  );

  it('normalizes malformed, mismatched, rejected, and expired results without stopping', async () => {
    const submitTransaction = jest.fn(
      async (
        _cbor: string,
        index: number
      ): Promise<Cip30WalletSubmissionResponse> => {
        if (index === 0)
          return { ...success(index), transaction_id: 'ff'.repeat(32) };
        if (index === 1) return success(index, 'rejected');
        return success(index, 'expired');
      }
    );

    const rejection = await submitCip103Batch(request(submitTransaction)).catch(
      (error) => error
    );

    expect(submitTransaction).toHaveBeenCalledTimes(3);
    expect(rejection).toEqual([
      { code: 2, info: 'Transaction submission failed' },
      { code: 2, info: 'Transaction submission failed' },
      { code: 2, info: 'Transaction submission failed' },
    ]);
  });

  it('continues the authorized loop after the guest route disappears', async () => {
    let guestRouteAlive = true;
    const observedRouteState: boolean[] = [];
    const submitTransaction = jest.fn(async (_cbor: string, index: number) => {
      observedRouteState.push(guestRouteAlive);
      if (index === 0) guestRouteAlive = false;
      return success(index);
    });

    await expect(
      submitCip103Batch(request(submitTransaction))
    ).resolves.toEqual(batch.items.map(({ bodyHash }) => bodyHash));
    expect(observedRouteState).toEqual([true, false, false]);
  });

  it('relies on backend idempotence and retains no retry journal', async () => {
    let pass = 0;
    const submitTransaction = jest.fn(async (_cbor: string, index: number) =>
      success(index, pass === 0 ? 'submitted' : 'in_ledger')
    );

    const first = await submitCip103Batch(request(submitTransaction));
    pass = 1;
    const retry = await submitCip103Batch(request(submitTransaction));

    expect(retry).toEqual(first);
    expect(submitTransaction.mock.calls.map(([, index]) => index)).toEqual([
      0,
      1,
      2,
      0,
      1,
      2,
    ]);
  });

  it('rejects a mismatched review before any submission attempt', async () => {
    const submitTransaction = jest.fn(async (_cbor: string, index: number) =>
      success(index)
    );

    const rejection = await submitCip103Batch(
      request(submitTransaction, { ...review, approvable: false })
    ).catch((error) => error);

    expect(submitTransaction).not.toHaveBeenCalled();
    expect(rejection).toEqual([
      { code: 2, info: 'Transaction submission failed' },
      { code: 2, info: 'Transaction submission failed' },
      { code: 2, info: 'Transaction submission failed' },
    ]);
  });
});
