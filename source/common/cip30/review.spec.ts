import alonzoFixture from '../cardano/fixtures/exact-cbor/alonzo-compatible.json';
import babbageFixture from '../cardano/fixtures/exact-cbor/babbage-compatible.json';
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
const context = {
  outputs: [],
  ownership: [],
  network: {
    networkId: 0 as const,
    networkMagic: 42,
    genesisHash: '00'.repeat(32),
  },
};

describe('CIP-30 transaction review', () => {
  it('binds sign review to exact body bytes and submit review to the full envelope', () => {
    const signing = createCip30TransactionReview(transaction, 'sign', context);
    const submission = createCip30TransactionReview(
      transaction,
      'submit',
      context
    );

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

  it.each([
    ['Alonzo', alonzoFixture, true],
    ['Babbage', babbageFixture, true],
    ['Conway', semanticFixture, false],
  ] as const)(
    'displays exact %s-family effects and approves only authenticated semantics',
    (family, fixture, mint) => {
      const envelope = parseConwayTransactionEnvelope(
        Buffer.from(fixture.cborHex, 'hex')
      );
      const unresolved = decodeConwayTransaction(envelope);
      const tx = decodeConwayTransaction(envelope, {
        resolvedInputs: unresolved.inputs.normal.map((outpoint) => ({
          outpoint,
          value: { coin: BigInt(10_000_000), assets: [] },
        })),
      });
      const review = createCip30TransactionReview(tx, 'submit', context);
      expect(review.approvable).toBe(true);
      expect(review.fullCbor).toBe(fixture.cborHex);
      expect(review.bodyCbor).toBe(
        envelope.cbor.subarray(envelope.spans.body.start, envelope.spans.body.end).toString('hex')
      );
      expect(review.effects.map(({ kind }) => kind)).toEqual(
        expect.arrayContaining(mint
          ? ['output', 'fee', 'mint', 'native-script', 'auxiliary-data']
          : ['output', 'fee'])
      );
      expect(review.effects.some(({ kind }) => kind === 'mint')).toBe(mint);
      expect(review.display.fee).toBe(mint ? '3' : '0');
      expect(review.display.entries.find(({ role }) => role === 'output')?.value?.coin)
        .toBe(family === 'Alonzo' ? '100000' : family === 'Babbage' ? '100' : '0');
      expect(review.display.mint.map(({ assetName, quantity }) => [assetName, quantity]))
        .toEqual(mint ? [['6173736574', '1']] : []);
      if (family === 'Babbage')
        expect(review.display.entries.find(({ role }) => role === 'output')).toMatchObject({
          hasDatum: true,
          hasReferenceScript: true,
        });
      expect(parseCip30TransactionReview(review)).toEqual(review);
    }
  );

  it('warns about preferred collateral spending without blocking approval', () => {
    const review = createCip30TransactionReview(transaction, 'sign', context, [
      {
        kind: 'preferred-collateral-spend',
        value: { state: 'will-be-spent' },
      },
    ]);

    expect(review.effects).toContainEqual(
      expect.objectContaining({ kind: 'preferred-collateral-spend' })
    );
    expect(review.approvable).toBe(true);
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
      'sign',
      context
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
    const value = createCip30TransactionReview(transaction, 'submit', context);
    expect(() =>
      parseCip30TransactionReview({ ...value, fullCborDigest: '00' })
    ).toThrow('Invalid CIP-30 transaction review');
  });
});
