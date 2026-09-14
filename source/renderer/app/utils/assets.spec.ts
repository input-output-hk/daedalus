import BigNumber from 'bignumber.js';
import { getAssetTokenFromToken, getNonZeroAssetTokens } from './assets';
import type { AssetMetadata, Token } from '../api/assets/types';

const policyId = '6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7';
const otherPolicyId =
  '2f9e3d4a1b8c7e6f5a4b3c2d1e0f9a8b7c6d5e4f3a2b1c0d9e8f7a6b';
// 'Cointest' and 'USDC' respectively.
const assetName = '436f696e74657374';
const otherAssetName = '55534443';
// A 32-byte name whose bytes are not text.
const opaqueAssetName =
  '787c09a71b2eacdc2a7644591bd32426ed996387470bc6ec9574167ccf6af8cf';

const metadata: AssetMetadata = {
  name: 'Test Coin',
  description: 'A test coin',
  ticker: 'TEST',
};

// Shaped as the wallet balance mapping builds a token: identity already present.
const walletToken = (overrides: Partial<Token> = {}): Token => ({
  policyId,
  assetName,
  assetNameASCII: 'Cointest',
  quantity: new BigNumber(42),
  uniqueId: `${policyId}${assetName}`,
  ...overrides,
});

// Shaped as the transaction mapping builds one: no uniqueId, no assetNameASCII.
const transactionToken = (overrides: Record<string, any> = {}) =>
  ({
    policyId,
    assetName,
    quantity: new BigNumber(7),
    address: 'addr_test1qq',
    ...overrides,
  }) as Token;

const lookupOf =
  (assets: Record<string, Record<string, any>>) =>
  (lookupPolicyId: string, lookupAssetName: string) =>
    assets[`${lookupPolicyId}${lookupAssetName}`];

const emptyLookup = () => undefined;

describe('getAssetTokenFromToken', () => {
  it('keeps the identity of a token whose subject has no cached row', () => {
    const merged = getAssetTokenFromToken(walletToken(), emptyLookup);
    expect(merged.uniqueId).toEqual(`${policyId}${assetName}`);
    expect(merged.policyId).toEqual(policyId);
    expect(merged.assetName).toEqual(assetName);
    expect(merged.assetNameASCII).toEqual('Cointest');
    expect(merged.quantity).toEqual(new BigNumber(42));
  });

  it('leaves the registry fields undefined when the subject has no cached row', () => {
    const merged = getAssetTokenFromToken(walletToken(), emptyLookup);
    expect(merged.metadata).toBeUndefined();
    expect(merged.decimals).toBeUndefined();
    expect(merged.recommendedDecimals).toBeUndefined();
    expect(merged.fingerprint).toBeUndefined();
  });

  it('merges a fully populated token and lookup into exactly the fields both carry', () => {
    const token = walletToken({ address: 'addr_test1qq' });
    const merged = getAssetTokenFromToken(
      token,
      lookupOf({
        [`${policyId}${assetName}`]: {
          uniqueId: `${policyId}${assetName}`,
          policyId,
          assetName,
          fingerprint: 'asset1cvmyrfrc7lpsnjhhz9l4rzqmc6nlp4kw2xkvpa',
          metadata,
          decimals: 6,
          recommendedDecimals: 6,
        },
      })
    );
    expect(merged).toEqual({
      policyId,
      assetName,
      assetNameASCII: 'Cointest',
      quantity: new BigNumber(42),
      address: 'addr_test1qq',
      uniqueId: `${policyId}${assetName}`,
      fingerprint: 'asset1cvmyrfrc7lpsnjhhz9l4rzqmc6nlp4kw2xkvpa',
      metadata,
      decimals: 6,
      recommendedDecimals: 6,
    });
  });

  it('derives the identity of a token that originated in a transaction', () => {
    const merged = getAssetTokenFromToken(transactionToken(), emptyLookup);
    expect(merged.uniqueId).toEqual(`${policyId}${assetName}`);
    expect(merged.assetNameASCII).toEqual('Cointest');
    expect(merged.quantity).toEqual(new BigNumber(7));
    expect(merged.address).toEqual('addr_test1qq');
  });

  it('derives an asset name that is not text without throwing', () => {
    const merged = getAssetTokenFromToken(
      transactionToken({ assetName: opaqueAssetName }),
      emptyLookup
    );
    expect(merged.uniqueId).toEqual(`${policyId}${opaqueAssetName}`);
    expect(typeof merged.assetNameASCII).toEqual('string');
  });

  it('derives the identity of a transaction token whose subject is cached', () => {
    const merged = getAssetTokenFromToken(
      transactionToken(),
      lookupOf({
        [`${policyId}${assetName}`]: {
          uniqueId: 'a value the merge must not read',
          fingerprint: 'asset1cvmyrfrc7lpsnjhhz9l4rzqmc6nlp4kw2xkvpa',
          metadata,
          decimals: 6,
        },
      })
    );
    expect(merged.uniqueId).toEqual(`${policyId}${assetName}`);
    expect(merged.metadata).toEqual(metadata);
    expect(merged.decimals).toEqual(6);
  });

  it('never takes identity from the lookup', () => {
    const merged = getAssetTokenFromToken(
      walletToken(),
      lookupOf({
        [`${policyId}${assetName}`]: {
          uniqueId: `${otherPolicyId}${otherAssetName}`,
          policyId: otherPolicyId,
          assetName: otherAssetName,
          assetNameASCII: 'USDC',
          metadata,
        },
      })
    );
    expect(merged.uniqueId).toEqual(`${policyId}${assetName}`);
    expect(merged.policyId).toEqual(policyId);
    expect(merged.assetName).toEqual(assetName);
    expect(merged.assetNameASCII).toEqual('Cointest');
  });
});

describe('getNonZeroAssetTokens', () => {
  it('keeps every token when no subject has a cached row', () => {
    const tokens = [
      walletToken(),
      walletToken({
        assetName: otherAssetName,
        assetNameASCII: 'USDC',
        uniqueId: `${policyId}${otherAssetName}`,
        quantity: new BigNumber(1),
      }),
    ];
    const merged = getNonZeroAssetTokens(tokens, emptyLookup);
    expect(merged).toHaveLength(2);
    expect(merged.map(({ uniqueId }) => uniqueId)).toEqual([
      `${policyId}${assetName}`,
      `${policyId}${otherAssetName}`,
    ]);
  });

  it('keeps every token when only some subjects have a cached row', () => {
    const tokens = [
      walletToken(),
      walletToken({
        assetName: otherAssetName,
        assetNameASCII: 'USDC',
        uniqueId: `${policyId}${otherAssetName}`,
        quantity: new BigNumber(1),
      }),
    ];
    const merged = getNonZeroAssetTokens(
      tokens,
      lookupOf({
        [`${policyId}${otherAssetName}`]: {
          fingerprint: 'asset1zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz',
          metadata,
        },
      })
    );
    expect(merged).toHaveLength(2);
    expect(
      merged.find(({ uniqueId }) => uniqueId === `${policyId}${otherAssetName}`)
        ?.metadata
    ).toEqual(metadata);
  });

  it('orders the tokens whose subject resolved by fingerprint', () => {
    const tokens = [
      walletToken({
        assetName: otherAssetName,
        uniqueId: `${policyId}${otherAssetName}`,
      }),
      walletToken(),
    ];
    const merged = getNonZeroAssetTokens(
      tokens,
      lookupOf({
        [`${policyId}${assetName}`]: { fingerprint: 'asset1aaa' },
        [`${policyId}${otherAssetName}`]: { fingerprint: 'asset1bbb' },
      })
    );
    expect(merged.map(({ fingerprint }) => fingerprint)).toEqual([
      'asset1aaa',
      'asset1bbb',
    ]);
  });
});
