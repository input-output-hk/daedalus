import cbor from 'cbor';

import { invalidRequest } from '../cip30/errors';
import wireFixtures from '../cip30/contracts/fixtures/wire-fixtures.json';
import type { ContextOutput } from './transactionContext';
import {
  decodeCip30Value,
  getCip30Balance,
  getCip30Utxos,
  normalizeCip30Address,
  serializeCip30Value,
} from './cip30Serialization';

const address = wireFixtures.addresses.find(
  ({ name }) => name === 'mainnet-enterprise-matching-drep'
);
if (!address) throw new Error('Missing address fixture');

const makeOutput = (
  transactionByte: number,
  index: number,
  coin: number,
  assets: Array<[string, string, number]> = [],
  walletMember = true,
  rawAddress = address.raw
): ContextOutput => {
  const input = [Buffer.alloc(32, transactionByte), index];
  const policies = new Map<Buffer, Map<Buffer, number>>();
  assets.forEach(([policyId, assetName, quantity]) => {
    const policy = [...policies.keys()].find(
      (candidate) => candidate.toString('hex') === policyId
    );
    const values = policy ? policies.get(policy)! : new Map<Buffer, number>();
    values.set(Buffer.from(assetName, 'hex'), quantity);
    if (!policy) policies.set(Buffer.from(policyId, 'hex'), values);
  });
  const value = assets.length ? [coin, policies] : coin;
  const output = cbor.encodeCanonical([Buffer.from(rawAddress, 'hex'), value]);
  return {
    outpoint: {
      transactionId: transactionByte.toString(16).padStart(2, '0').repeat(32),
      index,
    },
    sourceCbor: output.toString('hex'),
    inputCbor: cbor.encodeCanonical(input).toString('hex'),
    canonicalCbor: output.toString('hex'),
    unspentCbor: cbor
      .encodeCanonical([input, cbor.decodeFirstSync(output)])
      .toString('hex'),
    provenance: ['node'],
    roles: ['wallet_snapshot'],
    walletMember,
    pendingState: 'none',
  };
};

const expectInvalid = (action: () => unknown): void => {
  try {
    action();
    throw new Error('Expected InvalidRequest');
  } catch (error) {
    expect(error).toEqual(invalidRequest());
  }
};

describe('CIP-30 address and value serialization', () => {
  test.each(wireFixtures.addresses)(
    'normalizes $name raw and Bech32 addresses',
    (fixture) => {
      expect(
        normalizeCip30Address(fixture.raw, fixture.networkId as 0 | 1)
      ).toBe(fixture.raw);
      expect(
        normalizeCip30Address(fixture.bech32, fixture.networkId as 0 | 1)
      ).toBe(fixture.raw);
    }
  );

  test.each(wireFixtures.addressNegatives)('rejects $name', ({ value }) => {
    expectInvalid(() => normalizeCip30Address(value, 1));
  });
  test('decodes and canonically serializes ledger values', () => {
    const policyId = '11'.repeat(28);
    const value = {
      coin: BigInt(24),
      assets: [
        { policyId, assetName: 'ff', quantity: BigInt(2) },
        { policyId, assetName: '', quantity: BigInt(1) },
      ],
    };
    const encoded = serializeCip30Value(value);
    expect(encoded).toBe(
      cbor
        .encodeCanonical([
          24,
          new Map([
            [
              Buffer.from(policyId, 'hex'),
              new Map([
                [Buffer.alloc(0), 1],
                [Buffer.from('ff', 'hex'), 2],
              ]),
            ],
          ]),
        ])
        .toString('hex')
    );
    expect(decodeCip30Value(encoded)).toEqual({
      coin: value.coin,
      assets: [value.assets[1], value.assets[0]],
    });
    expect(
      serializeCip30Value({ coin: BigInt('18446744073709551615'), assets: [] })
    ).toBe('1bffffffffffffffff');
    expectInvalid(() => decodeCip30Value('00ff'));
  });
});

describe('CIP-30 UTxO reads', () => {
  const policyId = '22'.repeat(28);
  const outputs = [
    makeOutput(3, 0, 7),
    makeOutput(1, 1, 2, [[policyId, 'aa', 3]]),
    makeOutput(2, 0, 5),
    makeOutput(0, 0, 99, [], false),
  ];
  const snapshot = { outputs };

  test('returns exact pairs in outpoint order and aggregates every controlled value', () => {
    expect(getCip30Utxos(snapshot)).toEqual({
      kind: 'page',
      items: [
        outputs[1].unspentCbor,
        outputs[2].unspentCbor,
        outputs[0].unspentCbor,
      ],
    });
    expect(decodeCip30Value(getCip30Balance(snapshot))).toEqual({
      coin: BigInt(14),
      assets: [{ policyId, assetName: 'aa', quantity: BigInt(3) }],
    });
  });

  test('selects a deterministic covering prefix or null', () => {
    expect(
      getCip30Utxos(snapshot, cbor.encodeCanonical(6).toString('hex'))
    ).toEqual({
      kind: 'page',
      items: [outputs[1].unspentCbor, outputs[2].unspentCbor],
    });
    expect(
      getCip30Utxos(
        snapshot,
        cbor
          .encodeCanonical([
            0,
            new Map([
              [
                Buffer.from(policyId, 'hex'),
                new Map([[Buffer.from('aa', 'hex'), 3]]),
              ],
            ]),
          ])
          .toString('hex')
      )
    ).toEqual({ kind: 'page', items: [outputs[1].unspentCbor] });
    expect(
      getCip30Utxos(snapshot, cbor.encodeCanonical(15).toString('hex'))
    ).toBeNull();
    expect(getCip30Utxos(snapshot, '00')).toEqual({ kind: 'page', items: [] });
  });

  test('paginates after selection without capping the result set', () => {
    expect(getCip30Utxos(snapshot, undefined, { page: 1, limit: 2 })).toEqual({
      kind: 'page',
      items: [outputs[0].unspentCbor],
    });
    expect(getCip30Utxos(snapshot, undefined, { page: 3, limit: 2 })).toEqual({
      kind: 'paginate-error',
      maxSize: 3,
    });
    expect(getCip30Utxos(snapshot, undefined, { page: 1, limit: 3 })).toEqual({
      kind: 'page',
      items: [],
    });
    expectInvalid(() =>
      getCip30Utxos(snapshot, undefined, { page: 0, limit: 101 })
    );

    const many = {
      outputs: Array.from({ length: 101 }, (_, index) =>
        makeOutput(index, 0, 1)
      ),
    };
    expect(getCip30Utxos(many)).toMatchObject({
      kind: 'page',
      items: { length: 101 },
    });
  });
});
