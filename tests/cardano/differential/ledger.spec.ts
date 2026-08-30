import { blake2b } from 'blakejs';

import babbageFixture from '../../../source/common/cardano/fixtures/exact-cbor/conway-babbage-outputs.json';
import regressionFixture from '../../../source/common/cardano/fixtures/exact-cbor/conway-regression.json';
import untaggedFixture from '../../../source/common/cardano/fixtures/exact-cbor/conway-untagged-sets.json';
import manifest from '../../../source/common/cardano/fixtures/exact-cbor/manifest.json';
import { decodeConwayTransaction } from '../../../source/common/cardano/transaction';
import { parseConwayTransactionEnvelope } from '../../../source/common/cardano/transactionEnvelope';

const fixtures = [regressionFixture, babbageFixture, untaggedFixture];

const ledgerExpectation = (id: string) => {
  const fixture = manifest.fixtures.find((candidate) => candidate.id === id);
  if (!fixture) throw new Error(`Missing ledger fixture expectation: ${id}`);
  return fixture.expected;
};

const semanticEffects = (cborHex: string) => {
  const transaction = decodeConwayTransaction(
    parseConwayTransactionEnvelope(Buffer.from(cborHex, 'hex'))
  );
  const outputs = transaction.outputs.map(
    ({ address, datum, referenceScript, value }) => ({
      address,
      datum,
      referenceScript,
      value,
    })
  );
  const collateralReturn = transaction.collateral.return
    ? {
        address: transaction.collateral.return.address,
        datum: transaction.collateral.return.datum,
        referenceScript: transaction.collateral.return.referenceScript,
        value: transaction.collateral.return.value,
      }
    : undefined;
  const outpoints = (
    values: readonly Readonly<{ transactionId: string; index: bigint }>[]
  ) => values.map(({ transactionId, index }) => ({ transactionId, index }));
  return {
    inputs: {
      normal: outpoints(transaction.inputs.normal),
      collateral: outpoints(transaction.inputs.collateral),
      reference: outpoints(transaction.inputs.reference),
    },
    outputs,
    fee: transaction.fee,
    collateral: {
      return: collateralReturn,
      total: transaction.collateral.total,
      maximumLoss: transaction.collateral.maximumLoss,
      maximumLossRequirement: transaction.collateral.maximumLossRequirement,
    },
    witnessCounts: {
      vkeys: transaction.witnesses.vkeys.length,
      bootstrap: transaction.witnesses.bootstrap.length,
      nativeScripts: transaction.witnesses.nativeScripts.length,
      plutusScripts: transaction.witnesses.plutusScripts.length,
      datums: transaction.witnesses.datums.length,
      redeemers: transaction.witnesses.redeemers.length,
    },
    isValid: transaction.envelope.isValid,
    effectKinds: transaction.effects.map(({ kind }) => kind),
  };
};

describe('Cardano ledger differential fixtures', () => {
  it.each(fixtures)(
    'matches the pinned ledger body hash for $id',
    (fixture) => {
      const source = Buffer.from(fixture.cborHex, 'hex');
      const envelope = parseConwayTransactionEnvelope(source);
      const expected = ledgerExpectation(fixture.id);
      const independentlyHashedBody = Buffer.from(
        blake2b(
          source.subarray(envelope.spans.body.start, envelope.spans.body.end),
          undefined,
          32
        )
      ).toString('hex');

      expect(envelope.transactionId).toBe(expected.bodyHashBlake2b256);
      expect(independentlyHashedBody).toBe(expected.bodyHashBlake2b256);
      expect(envelope.spans).toMatchObject({
        body: expected.body,
        witnessSet: expected.witnessSet,
        isValid: expected.isValid,
        auxiliaryData: expected.auxiliaryData,
        outputs: expected.outputs,
        collateralReturn: expected.collateralReturn,
      });
    }
  );

  it('normalizes tagged, untagged, array, and map ledger encodings to identical effects', () => {
    const [expected, ...variants] = fixtures.map(({ cborHex }) =>
      semanticEffects(cborHex)
    );

    variants.forEach((variant) => expect(variant).toEqual(expected));
    expect(expected).toMatchObject({
      fee: BigInt(180021),
      inputs: {
        normal: expect.any(Array),
        collateral: expect.any(Array),
        reference: [],
      },
      outputs: [
        { value: { coin: BigInt(2000000), assets: [] } },
        { value: { coin: BigInt(995651046), assets: [] } },
      ],
      collateral: {
        return: { value: { coin: BigInt(1729968), assets: [] } },
        total: BigInt(270032),
      },
      witnessCounts: {
        vkeys: 0,
        bootstrap: 0,
        nativeScripts: 0,
        plutusScripts: 1,
        datums: 1,
        redeemers: 1,
      },
      isValid: false,
    });
  });
});
