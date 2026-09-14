import { assetFingerprint } from './assetFingerprint';

// The eight CIP-14 golden vectors, transcribed from cardano-wallet at
// 3e623efdd3b2652288607536b58b3959717a9608,
// lib/primitive/test/spec/Cardano/Wallet/Primitive/Types/TokenFingerprintSpec.hs:40-78.
const goldenVectors: Array<[string, string, string]> = [
  [
    '7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373',
    '',
    'asset1rjklcrnsdzqp65wjgrg55sy9723kw09mlgvlc3',
  ],
  [
    '7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc37e',
    '',
    'asset1nl0puwxmhas8fawxp8nx4e2q3wekg969n2auw3',
  ],
  [
    '1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209',
    '',
    'asset1uyuxku60yqe57nusqzjx38aan3f2wq6s93f6ea',
  ],
  [
    '7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373',
    '504154415445',
    'asset13n25uv0yaf5kus35fm2k86cqy60z58d9xmde92',
  ],
  [
    '1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209',
    '504154415445',
    'asset1hv4p5tv2a837mzqrst04d0dcptdjmluqvdx9k3',
  ],
  [
    '1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209',
    '7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373',
    'asset1aqrdypg669jgazruv5ah07nuyqe0wxjhe2el6f',
  ],
  [
    '7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373',
    '1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209',
    'asset17jd78wukhtrnmjh3fngzasxm8rck0l2r4hhyyt',
  ],
  [
    '7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373',
    '0000000000000000000000000000000000000000000000000000000000000000',
    'asset1pkpwyknlvul7az0xx8czhl60pyel45rpje4z8w',
  ],
];

const policyId = goldenVectors[0][0];

describe('assetFingerprint', () => {
  goldenVectors.forEach(([vectorPolicyId, vectorAssetName, expected]) => {
    it(`produces the published fingerprint ${expected}`, () => {
      expect(assetFingerprint(vectorPolicyId, vectorAssetName)).toEqual(
        expected
      );
    });
  });

  it('produces a different fingerprint when the two arguments are transposed', () => {
    const [first, second] = [goldenVectors[5], goldenVectors[6]];
    expect(assetFingerprint(first[0], first[1])).not.toEqual(
      assetFingerprint(second[0], second[1])
    );
  });

  it('accepts an upper-case hex policy id', () => {
    const [vectorPolicyId, vectorAssetName, expected] = goldenVectors[3];
    expect(
      assetFingerprint(vectorPolicyId.toUpperCase(), vectorAssetName)
    ).toEqual(expected);
  });

  it('rejects an odd-length policy id', () => {
    expect(() => assetFingerprint(policyId.slice(1), '')).toThrow(
      'policyId is not a hex string'
    );
  });

  it('rejects a policy id containing a non-hex character', () => {
    // Buffer.from stops at the first non-hex pair, so this would otherwise
    // hash a silently truncated prefix.
    expect(() => assetFingerprint(`${policyId.slice(0, 54)}zz`, '')).toThrow(
      'policyId is not a hex string'
    );
  });

  it('rejects an asset name containing a non-hex character', () => {
    expect(() => assetFingerprint(policyId, '5041zz')).toThrow(
      'assetName is not a hex string'
    );
  });

  it('rejects an empty policy id', () => {
    expect(() => assetFingerprint('', '')).toThrow(
      'policyId must be 28 bytes, was 0'
    );
  });

  it('rejects a policy id one byte short', () => {
    expect(() => assetFingerprint(policyId.slice(0, 54), '')).toThrow(
      'policyId must be 28 bytes, was 27'
    );
  });

  it('rejects a policy id one byte long', () => {
    expect(() => assetFingerprint(`${policyId}ab`, '')).toThrow(
      'policyId must be 28 bytes, was 29'
    );
  });

  it('accepts an asset name of exactly 32 bytes', () => {
    expect(assetFingerprint(policyId, '00'.repeat(32))).toEqual(
      'asset1pkpwyknlvul7az0xx8czhl60pyel45rpje4z8w'
    );
  });

  it('rejects an asset name of 33 bytes', () => {
    expect(() => assetFingerprint(policyId, '00'.repeat(33))).toThrow(
      'assetName must be at most 32 bytes, was 33'
    );
  });

  it('rejects the two arguments given in the wrong order', () => {
    expect(() => assetFingerprint('00'.repeat(32), policyId)).toThrow(
      'policyId must be 28 bytes, was 32'
    );
  });
});
