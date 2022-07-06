import BigNumber from 'bignumber.js';
import { hasAssetsAfterTransaction } from './helpers';

const assetWithOneQuantity = {
  policyId: 'policyId1',
  assetName: '54657374636f696e',
  quantity: new BigNumber(1),
  fingerprint: 'policyId154657374636f696e',
  metadata: {
    name: 'Testcoin',
    description: 'Test coin',
  },
  uniqueId: 'uniqueId1',
  decimals: 1,
  recommendedDecimals: null,
};

const assetWithTenQuantity = {
  policyId: 'policyId2',
  assetName: '436f696e74657374',
  quantity: new BigNumber(10),
  fingerprint: 'policyId2436f696e74657374',
  uniqueId: 'uniqueId2',
  decimals: 1,
  recommendedDecimals: null,
};

describe('hasAssetsAfterTransaction', () => {
  test('Should be true when wallet has assets and tx sending no assets', () => {
    expect(
      hasAssetsAfterTransaction({
        assetTokens: [assetWithOneQuantity, assetWithTenQuantity],
        selectedAssets: [],
        assetsAmounts: [],
      })
    ).toEqual(true);
  });

  test('Should be true when wallet has assets and tx is not sending all asset types', () => {
    expect(
      hasAssetsAfterTransaction({
        assetTokens: [assetWithOneQuantity, assetWithTenQuantity],
        selectedAssets: [assetWithTenQuantity],
        assetsAmounts: ['5'],
      })
    ).toEqual(true);
  });

  test('Should be true when wallet has assets and tx is not sending all assets', () => {
    expect(
      hasAssetsAfterTransaction({
        assetTokens: [assetWithOneQuantity, assetWithTenQuantity],
        selectedAssets: [assetWithOneQuantity, assetWithTenQuantity],
        assetsAmounts: ['1', '5'],
      })
    ).toEqual(true);
  });

  test('Should be false when wallet has no assets', () => {
    expect(
      hasAssetsAfterTransaction({
        assetTokens: [],
        selectedAssets: [],
        assetsAmounts: [],
      })
    ).toEqual(false);
  });

  test('Should be false when tx is sending all assets (1 asset)', () => {
    expect(
      hasAssetsAfterTransaction({
        assetTokens: [assetWithOneQuantity],
        selectedAssets: [assetWithOneQuantity],
        assetsAmounts: ['1'],
      })
    ).toEqual(false);
  });

  test('Should be false when tx is sending all assets (2 assets)', () => {
    expect(
      hasAssetsAfterTransaction({
        assetTokens: [assetWithOneQuantity, assetWithTenQuantity],
        selectedAssets: [assetWithOneQuantity, assetWithTenQuantity],
        assetsAmounts: ['1', '10'],
      })
    ).toEqual(false);
  });
});
