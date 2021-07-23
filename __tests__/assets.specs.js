import { hasTokenLeftAfterTransaction } from '../source/renderer/app/utils/assets';

const allAssets = [{
  policyId: '6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7',
  assetName: '',
  quantity: 6,
  fingerprint: 'asset1cvmyrfrc7lpht2hcjwr9lulzyyjv27uxh3kcz0',
  metadata: {
    url: 'https://developers.cardano.org/',
    name: 'Testcoin',
    ticker: 'TEST',
    description: 'Testcoin crypto powered by Cardano testnet.',
  },
  recommendedDecimals: null,
  uniqueId: '6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7',
}, {
  policyId: '94d4cdbcffb09ebd4780d94f932a657dc4852530fa8013df66c72d4c',
  assetName: '676f6f64636f696e',
  quantity: 1,

  fingerprint: 'asset13x2a44r5cp3kuys2x7sgz5062r6q9hl8htkpup',
  metadata: {
    url: 'https://good.io',
    name: 'GoodCoin',
    description: 'Coin that is good!!! 👍',
  },
  recommendedDecimals: null,
  uniqueId: '94d4cdbcffb09ebd4780d94f932a657dc4852530fa8013df66c72d4c676f6f64636f696e',
}, {
  policyId: '789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1',
  assetName: '',
  quantity: 2,
  fingerprint: 'asset1656gm7zkherdvxkn52mhaxkkw343qtkqgv0h8c',
  metadata: {
    url: 'https://sad.io',
    name: 'SadCoin',
    ticker: 'SAD',
    description: 'Coin with no asset name',
  },
  recommendedDecimals: null,
  uniqueId: '789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1',
}];

describe('Asset helper functions', () => {
  it('Function hasTokenLeftAfterTransaction returns true in case after the transaction no tokes are left in the wallet', () => {
    const assetAmounts = ["3","1","1"];
    const result = hasTokenLeftAfterTransaction(allAssets, assetAmounts);
    expect(result).toBe(true);
  });
  it('Function hasTokenLeftAfterTransaction returns false in case after the transaction tokes are left in the wallet', () => {
    const assetAmounts = ["6","1","2"];
    const result = hasTokenLeftAfterTransaction(allAssets, assetAmounts);
    expect(result).toBe(false);
  });

  it('Function hasTokenLeftAfterTransaction returns true in case after the transaction no tokes are left in the wallet in case of one token', () => {
    const assetAmounts = ["1"];
    const result = hasTokenLeftAfterTransaction([{...allAssets[1]}], assetAmounts);
    expect(result).toBe(false);
  });

});
