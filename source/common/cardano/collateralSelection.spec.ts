import cbor from 'cbor';

import { invalidRequest } from '../cip30/errors';
import wireFixtures from '../cip30/contracts/fixtures/wire-fixtures.json';
import type { Cip30Utxo } from './cip30Serialization';
import { decodeConwayOutput } from './transaction';
import type { ContextOutput } from './transactionContext';
import { decodeCip30Coin, selectCip30Collateral } from './collateralSelection';

const paymentAddress = wireFixtures.addresses.find(
  ({ name }) => name === 'mainnet-enterprise-matching-drep'
);
const scriptAddress = wireFixtures.addresses.find(
  ({ name }) => name === 'mainnet-enterprise-script'
);
if (!paymentAddress || !scriptAddress)
  throw new Error('Missing address fixtures');

const makeUtxo = (
  byte: number,
  coin: number,
  rawAddress = paymentAddress.raw,
  assets = false
): Cip30Utxo => {
  const input = [Buffer.alloc(32, byte), 0];
  const value = assets
    ? [coin, new Map([[Buffer.alloc(28, 1), new Map([[Buffer.alloc(0), 1]])]])]
    : coin;
  const output = cbor.encodeCanonical([Buffer.from(rawAddress, 'hex'), value]);
  const context: ContextOutput = {
    outpoint: {
      transactionId: byte.toString(16).padStart(2, '0').repeat(32),
      index: 0,
    },
    sourceCbor: output.toString('hex'),
    inputCbor: cbor.encodeCanonical(input).toString('hex'),
    canonicalCbor: output.toString('hex'),
    unspentCbor: cbor
      .encodeCanonical([input, cbor.decodeFirstSync(output)])
      .toString('hex'),
    provenance: ['node'],
    roles: ['wallet_snapshot'],
    walletMember: true,
    pendingState: 'none',
  };
  const decoded = decodeConwayOutput(output);
  return { context, address: decoded.address, value: decoded.value };
};

const expectInvalid = (action: () => unknown): void => {
  try {
    action();
    throw new Error('Expected InvalidRequest');
  } catch (error) {
    expect(error).toEqual(invalidRequest());
  }
};

test('accepts only the frozen canonical Coin encodings', () => {
  expect(wireFixtures.coin.positive.map(decodeCip30Coin).map(String)).toEqual([
    '0',
    '23',
    '24',
    '18446744073709551615',
  ]);
  wireFixtures.coin.negative.forEach((coin) =>
    expectInvalid(() => decodeCip30Coin(coin))
  );
});

test('chooses the smallest sufficient pure-ADA payment-key combination', () => {
  const four = makeUtxo(4, 4_000_000);
  const six = makeUtxo(6, 6_000_000);
  const ten = makeUtxo(10, 10_000_000);
  const script = makeUtxo(1, 1_000_000, scriptAddress.raw);
  const token = makeUtxo(2, 1_000_000, paymentAddress.raw, true);
  const utxos = [ten, token, six, script, four];
  const before = JSON.stringify(utxos, (_, value) =>
    typeof value === 'bigint' ? value.toString() : value
  );

  expect(
    selectCip30Collateral(
      utxos,
      cbor.encodeCanonical(10_000_000).toString('hex'),
      3
    )
  ).toEqual([ten.context.unspentCbor]);
  expect(
    selectCip30Collateral(
      [token, six, script, four],
      cbor.encodeCanonical(9_000_000).toString('hex'),
      3
    )
  ).toEqual([four.context.unspentCbor, six.context.unspentCbor]);
  expect(
    selectCip30Collateral(
      utxos,
      cbor.encodeCanonical(6_000_001).toString('hex'),
      1
    )
  ).toEqual([ten.context.unspentCbor]);
  expect(
    JSON.stringify(utxos, (_, value) =>
      typeof value === 'bigint' ? value.toString() : value
    )
  ).toBe(before);
});

test('has no five-ADA cap and returns null without side effects when insufficient', () => {
  const six = makeUtxo(6, 6_000_000);
  expect(
    selectCip30Collateral(
      [six],
      cbor.encodeCanonical(6_000_000).toString('hex'),
      1
    )
  ).toEqual([six.context.unspentCbor]);
  expect(
    selectCip30Collateral(
      [six],
      cbor.encodeCanonical(6_000_001).toString('hex'),
      1
    )
  ).toBeNull();
  expectInvalid(() => selectCip30Collateral([six], '1800', 1));
});
