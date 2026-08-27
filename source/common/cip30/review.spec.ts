import semanticFixture from '../cardano/fixtures/exact-cbor/semantic-conway-v1.json';
import { decodeConwayTransaction } from '../cardano/transaction';
import { parseConwayTransactionEnvelope } from '../cardano/transactionEnvelope';
import {
  createCip30TransactionReview,
  parseCip30TransactionReview,
} from './review';

const transaction = decodeConwayTransaction(
  parseConwayTransactionEnvelope(Buffer.from(semanticFixture.cborHex, 'hex'))
);

describe('CIP-30 transaction review', () => {
  it('binds sign review to exact body bytes and submit review to the full envelope', () => {
    const signing = createCip30TransactionReview(transaction, 'sign');
    const submission = createCip30TransactionReview(transaction, 'submit');

    expect(signing.transactionId).toBe(transaction.transactionId);
    expect(signing.bodyCbor).toBe(
      transaction.envelope.cbor
        .subarray(
          transaction.envelope.spans.body.start,
          transaction.envelope.spans.body.end
        )
        .toString('hex')
    );
    expect(submission.fullCbor).toBe(semanticFixture.cborHex);
    expect(submission.witnessSetCbor).toBe('a0');
    expect(submission.auxiliaryDataCbor).toBe('f6');
    expect(parseCip30TransactionReview(submission)).toEqual(submission);
  });

  it('refuses incomplete, unknown, and unresolved collateral semantics', () => {
    const incomplete = createCip30TransactionReview(
      {
        ...transaction,
        effects: [
          ...transaction.effects,
          { kind: 'future-effect', value: true },
          { kind: 'maximum-collateral-loss-unresolved', value: {} },
        ],
        review: {
          complete: false,
          signable: false,
          requirements: [
            { kind: 'datum', target: 'missing', reason: 'unavailable' },
          ],
        },
      },
      'sign'
    );
    expect(incomplete.approvable).toBe(false);
    expect(incomplete.refusalReasons).toEqual(
      expect.arrayContaining([
        'datum:missing:unavailable',
        'unsupported-effect:future-effect',
        'maximum-collateral-loss-unresolved',
      ])
    );
  });

  it('rejects malformed renderer review values', () => {
    const value = createCip30TransactionReview(transaction, 'submit');
    expect(() =>
      parseCip30TransactionReview({ ...value, fullCborDigest: '00' })
    ).toThrow('Invalid CIP-30 transaction review');
  });
});
