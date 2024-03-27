'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const helpers_1 = require('./helpers');
const assetWithOneQuantity = {
  policyId: 'policyId1',
  assetName: '54657374636f696e',
  quantity: new bignumber_js_1.default(1),
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
  quantity: new bignumber_js_1.default(10),
  fingerprint: 'policyId2436f696e74657374',
  uniqueId: 'uniqueId2',
  decimals: 1,
  recommendedDecimals: null,
};
describe('hasAssetsAfterTransaction', () => {
  test('Should be true when wallet has assets and tx sending no assets', () => {
    expect(
      (0, helpers_1.hasAssetsAfterTransaction)({
        assetTokens: [assetWithOneQuantity, assetWithTenQuantity],
        selectedAssets: [],
        assetsAmounts: [],
      })
    ).toEqual(true);
  });
  test('Should be true when wallet has assets and tx is not sending all asset types', () => {
    expect(
      (0, helpers_1.hasAssetsAfterTransaction)({
        assetTokens: [assetWithOneQuantity, assetWithTenQuantity],
        selectedAssets: [assetWithTenQuantity],
        assetsAmounts: ['5'],
      })
    ).toEqual(true);
  });
  test('Should be true when wallet has assets and tx is not sending all assets', () => {
    expect(
      (0, helpers_1.hasAssetsAfterTransaction)({
        assetTokens: [assetWithOneQuantity, assetWithTenQuantity],
        selectedAssets: [assetWithOneQuantity, assetWithTenQuantity],
        assetsAmounts: ['1', '5'],
      })
    ).toEqual(true);
  });
  test('Should be false when wallet has no assets', () => {
    expect(
      (0, helpers_1.hasAssetsAfterTransaction)({
        assetTokens: [],
        selectedAssets: [],
        assetsAmounts: [],
      })
    ).toEqual(false);
  });
  test('Should be false when tx is sending all assets (1 asset)', () => {
    expect(
      (0, helpers_1.hasAssetsAfterTransaction)({
        assetTokens: [assetWithOneQuantity],
        selectedAssets: [assetWithOneQuantity],
        assetsAmounts: ['1'],
      })
    ).toEqual(false);
  });
  test('Should be false when tx is sending all assets (2 assets)', () => {
    expect(
      (0, helpers_1.hasAssetsAfterTransaction)({
        assetTokens: [assetWithOneQuantity, assetWithTenQuantity],
        selectedAssets: [assetWithOneQuantity, assetWithTenQuantity],
        assetsAmounts: ['1', '10'],
      })
    ).toEqual(false);
  });
});
//# sourceMappingURL=helpers.spec.js.map
