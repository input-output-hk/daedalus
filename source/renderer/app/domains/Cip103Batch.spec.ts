import cbor from 'cbor';

import semanticFixture from '../../../common/cardano/fixtures/exact-cbor/semantic-conway-v1.json';
import { formatCip103FailureInfo } from '../../../common/types/cip103.types';
import { preflightCip103Sign, preflightCip103Submit } from './Cip103Batch';

const invalidRequest = { code: -1, info: 'Invalid request' };
const expectInvalid = (callback: () => unknown): void => {
  let caught: unknown;
  try {
    callback();
  } catch (error) {
    caught = error;
  }
  expect(caught).toEqual(invalidRequest);
};

const withNetworkId = (networkId: 0 | 1): string => {
  const transaction = cbor.decodeFirstSync(
    Buffer.from(semanticFixture.cborHex, 'hex')
  ) as [Map<number, unknown>, Map<number, unknown>, boolean, null];
  transaction[0].set(15, networkId);
  return cbor.encodeCanonical(transaction).toString('hex');
};

describe('CIP-103 batch preflight', () => {
  it('preserves ordered exact identities and independent duplicate items', () => {
    const source = [
      { cbor: semanticFixture.cborHex },
      { cbor: semanticFixture.cborHex, partialSign: true },
    ];
    const result = preflightCip103Sign(source, 0);

    source[0].cbor = '00';
    source.pop();

    expect(result.state).toBe('preflighted');
    expect(result.operation).toBe('sign');
    expect(result.items).toHaveLength(2);
    expect(result.items.map(({ index }) => index)).toEqual([0, 1]);
    expect(result.items.map(({ partialSign }) => partialSign)).toEqual([
      false,
      true,
    ]);
    expect(result.items.map((item) => item.cbor)).toEqual([
      semanticFixture.cborHex,
      semanticFixture.cborHex,
    ]);
    expect(result.items.map(({ fullCborDigest }) => fullCborDigest)).toEqual([
      'aa6d69fe05b455788296999818f61560ff6770623fbe8be7159ef9c9db673ebb',
      'aa6d69fe05b455788296999818f61560ff6770623fbe8be7159ef9c9db673ebb',
    ]);
    expect(result.items.map(({ bodyHash }) => bodyHash)).toEqual([
      '7533f1e5701f514e1f0a4853596dac0e3c4db9c1669535e7273588aaae7060e6',
      '7533f1e5701f514e1f0a4853596dac0e3c4db9c1669535e7273588aaae7060e6',
    ]);
    expect(result.items[0]).not.toBe(result.items[1]);
    expect(Object.isFrozen(result)).toBe(true);
    expect(Object.isFrozen(result.items)).toBe(true);
    expect(result.items.every(Object.isFrozen)).toBe(true);
  });

  it('rejects every invalid transaction and explicit network mismatch', () => {
    ['00', `${semanticFixture.cborHex}00`, withNetworkId(1)].forEach((value) =>
      expectInvalid(() => preflightCip103Submit([value], 0))
    );
    expect(preflightCip103Submit([withNetworkId(1)], 1).items).toHaveLength(1);
  });

  it('freezes the indexed failure text', () => {
    expect(formatCip103FailureInfo(0)).toBe('Transaction at index 0 failed');
    expect(formatCip103FailureInfo(12)).toBe('Transaction at index 12 failed');
  });
});
