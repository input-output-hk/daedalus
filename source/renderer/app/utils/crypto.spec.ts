import * as bip39 from 'bip39';
import { Buffer } from 'safe-buffer';
import bip39Vectors from './__fixtures__/bip39-vectors.json';
import {
  blake2b224,
  decodeBech32,
  encodeBech32,
  generateMnemonic,
  getStakeAddressFromStakeKey,
  mnemonicToSeedHex,
} from './crypto';
import validWords from '../../../common/config/crypto/valid-words.en';

// `webpack.config.js` is `target: 'web'`, so the shipped renderer resolves
// `pbkdf2` through its `browser` field to `browser.js`, a pure-JS
// implementation. `jest.config.js` sets no `browser: true`, so Jest resolves
// `main` and exercises the Node implementation, which delegates to
// `crypto.pbkdf2Sync`. Asserting only what Jest resolves would say nothing
// about what ships, so both are asserted here.
const pbkdf2Node = require('pbkdf2');
const pbkdf2Browser = require('pbkdf2/browser.js');
const unorm = require('unorm');

const { passphrase: VECTOR_PASSPHRASE, vectors } = bip39Vectors;

// The derivation `mnemonicToSeedHex` performs, with the implementation injected.
const seedHexVia = (impl, mnemonic: string, password: string): string =>
  impl
    .pbkdf2Sync(
      Buffer.from(unorm.nfkd(mnemonic), 'utf8'),
      Buffer.from(`mnemonic${unorm.nfkd(password) || ''}`, 'utf8'),
      2048,
      32,
      'sha512'
    )
    .toString('hex');

// BIP39 seeds are 64 bytes. `mnemonicToSeedHex` derives 32, so a published seed
// is compared against its leading half.
const leading32Bytes = (seedHex: string): string => seedHex.slice(0, 64);

const toHex = (bytes): string => Buffer.from(bytes).toString('hex');

describe('bip39 vector fixture', () => {
  // Guards the suite against being weakened by deleting vectors rather than by
  // changing an expectation, which is the quieter of the two.
  it('carries the whole published English set', () => {
    expect(vectors).toHaveLength(24);
  });

  it('covers every mnemonic length the published set defines', () => {
    const lengths = [
      ...new Set(vectors.map(([, mnemonic]) => mnemonic.split(' ').length)),
    ].sort((a, b) => a - b);
    expect(lengths).toEqual([12, 18, 24]);
  });

  it('names the passphrase the published seeds were derived with', () => {
    expect(VECTOR_PASSPHRASE).toBe('TREZOR');
  });
});

describe('mnemonicToSeedHex', () => {
  it.each(vectors.map(([, mnemonic, seedHex]) => [mnemonic, seedHex]))(
    'derives the published seed for "%s"',
    (mnemonic: string, seedHex: string) => {
      expect(mnemonicToSeedHex(mnemonic, VECTOR_PASSPHRASE)).toBe(
        leading32Bytes(seedHex)
      );
    }
  );

  // Expectations below are not in the published set; they were derived with an
  // independent PBKDF2-HMAC-SHA512 implementation rather than captured from
  // this one.
  it.each([
    [
      'abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about',
      '5eb00bbddcf069084889a8ab9155568165f5c453ccb85e70811aaed6f6da5fc1',
    ],
    [
      'legal winner thank year wave sausage worth useful legal winner thank yellow',
      '878386efb78845b3355bd15ea4d39ef97d179cb712b77d5c12b6be415fffeffe',
    ],
  ])(
    'salts with "mnemonic" alone when no password is given',
    (mnemonic, expected) => {
      // `getScrambledInput` calls this with one argument. `unorm.nfkd(undefined)`
      // returns the empty string rather than the string "undefined", so the salt
      // is exactly `mnemonic`.
      expect(mnemonicToSeedHex(mnemonic, undefined)).toBe(expected);
      expect(mnemonicToSeedHex(mnemonic, '')).toBe(expected);
    }
  );
});

describe('pbkdf2 resolution', () => {
  it.each(vectors.map(([, mnemonic, seedHex]) => [mnemonic, seedHex]))(
    'agrees between the Node and browser implementations for "%s"',
    (mnemonic: string, seedHex: string) => {
      const expected = leading32Bytes(seedHex);
      expect(seedHexVia(pbkdf2Node, mnemonic, VECTOR_PASSPHRASE)).toBe(
        expected
      );
      expect(seedHexVia(pbkdf2Browser, mnemonic, VECTOR_PASSPHRASE)).toBe(
        expected
      );
    }
  );
});

describe('bip39 entropy encoding', () => {
  // `generateMnemonic` delegates the entropy-to-words mapping, and its
  // checksum, to bip39. `bip39` 3.1.0 reimplements that path on top of
  // `@noble/hashes`, and this branch takes that bump. The mapping is asserted
  // against bip39 directly because `generateMnemonic` is random by
  // construction, so no vector can pin it from the outside.
  it.each(vectors.map(([entropyHex, mnemonic]) => [entropyHex, mnemonic]))(
    'maps entropy %s to the published mnemonic and back',
    (entropyHex: string, mnemonic: string) => {
      expect(bip39.entropyToMnemonic(entropyHex, validWords)).toBe(mnemonic);
      expect(bip39.mnemonicToEntropy(mnemonic, validWords)).toBe(entropyHex);
    }
  );
});

describe('generateMnemonic', () => {
  it.each([12, 15, 18, 21, 24])(
    'returns %i words when asked for them',
    (words) => {
      expect(generateMnemonic(words).split(' ')).toHaveLength(words);
    }
  );

  it('draws every word from the English wordlist', () => {
    const words = generateMnemonic(24).split(' ');
    const unknown = words.filter((word) => !validWords.includes(word));
    expect(unknown).toEqual([]);
  });

  it('does not repeat itself across calls', () => {
    expect(generateMnemonic(24)).not.toBe(generateMnemonic(24));
  });
});

describe('blake2b224', () => {
  // Expectations produced by an independent BLAKE2b implementation, not by
  // blakejs. `blakejs` moves from 1.1.0 to 1.2.1 on this branch and this is
  // what would notice if the digest changed.
  it.each([
    ['', '836cc68931c2e4e3e838602eca1902591d216837bafddfe6f0c8cb07'],
    ['abc', '9bd237b02a29e43bdd6738afa5b53ff0eee178d6210b618e4511aec8'],
  ])('digests %p to its known 28-byte value', (input, expected) => {
    expect(toHex(blake2b224(Buffer.from(input, 'utf8')))).toBe(expected);
  });
});

describe('decodeBech32 and encodeBech32', () => {
  const key = Buffer.from(
    '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f',
    'hex'
  );
  const encoded =
    'stake_vk1qqqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0suf0pev';

  it('encodes known bytes to the known bech32 string', () => {
    expect(encodeBech32('stake_vk', key)).toBe(encoded);
  });

  it('decodes the known bech32 string back to the same bytes', () => {
    expect(toHex(decodeBech32(encoded))).toBe(toHex(key));
  });
});

describe('getStakeAddressFromStakeKey', () => {
  const stakeKey =
    'stake_vk1qqqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0suf0pev';

  // Both expectations were produced independently: BLAKE2b-224 of the decoded
  // key, prefixed with the network byte, bech32-encoded with a reference
  // implementation.
  it('derives the mainnet stake address', () => {
    const original = global.environment.isMainnet;
    global.environment.isMainnet = true;
    try {
      expect(getStakeAddressFromStakeKey(stakeKey)).toBe(
        'stake1u9y3zykaqy24cp76kjzlwx6h9cx2uav79nfck8qwja2599ce4f2gx'
      );
    } finally {
      global.environment.isMainnet = original;
    }
  });

  it('derives the testnet stake address', () => {
    const original = global.environment.isMainnet;
    global.environment.isMainnet = false;
    try {
      expect(getStakeAddressFromStakeKey(stakeKey)).toBe(
        'stake_test1upy3zykaqy24cp76kjzlwx6h9cx2uav79nfck8qwja2599c7lrgvm'
      );
    } finally {
      global.environment.isMainnet = original;
    }
  });
});
