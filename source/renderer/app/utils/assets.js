'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.getUniqueId = exports.tokenHasBalance = exports.isTokenMissingInWallet = exports.searchAssets = exports.hasTokensLeftAfterTransaction = exports.sortAssets = exports.getNonZeroAssetTokens = exports.getAssetTokenFromToken = exports.getAssetTokens = exports.getAssetToken = exports.getToken = exports.getZeroToken = exports.filterAssets = void 0;
const find_1 = __importDefault(require('lodash/find'));
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const lodash_1 = require('lodash');
const WalletTransaction_1 = require('../domains/WalletTransaction');
const formatters_1 = require('./formatters');
/**
 * A few functions here use Assets and Tokens, so here is a brief difference:
 * Asset - has the asset details (fingerprint, metadata, decimals, recommendedDecimals)
 * Token - has a transaction's details (quantity, address)
 * Both have policyId and assetName
 */
/**
 * Removes the `change` assets
 * that are included in the TX API response
 */
const filterAssets = (assets, transactionType, isInternalAddress) =>
  assets.filter(
    ({ address }) =>
      (transactionType === WalletTransaction_1.TransactionTypes.INCOME &&
        isInternalAddress(address)) ||
      (transactionType === WalletTransaction_1.TransactionTypes.EXPEND &&
        !isInternalAddress(address))
  );
exports.filterAssets = filterAssets;
const getZeroToken = ({ policyId, assetName, assetNameASCII, uniqueId }) => ({
  policyId,
  assetName,
  assetNameASCII,
  uniqueId,
  quantity: new bignumber_js_1.default(0),
});
exports.getZeroToken = getZeroToken;
/**
 * Receives an asset and a list of tokens
 * Then retrieves the token with the same uniqueId
 * @param asset - asset details
 * @param tokens - list of Tokens
 * See Asset/Token differences at the beginning of this doc
 */
const getToken = (asset, tokens) => {
  let token = tokens.find(({ uniqueId }) => uniqueId === asset.uniqueId);
  if (!token) {
    token = (0, exports.getZeroToken)(asset);
  }
  return token;
};
exports.getToken = getToken;
/**
 * Receives a Token and an Asset
 * then merges them into an AssetToken
 * @param asset - asset details
 * @param token - token details
 * See Asset/Token differences at the beginning of this doc
 */
const getAssetToken = (
  {
    policyId,
    assetName,
    assetNameASCII,
    fingerprint,
    metadata,
    decimals,
    recommendedDecimals,
    uniqueId,
  },
  { quantity, address }
) => ({
  policyId,
  assetName,
  assetNameASCII,
  quantity,
  address,
  fingerprint,
  metadata,
  decimals,
  recommendedDecimals,
  uniqueId,
});
exports.getAssetToken = getAssetToken;
/**
 * Receives both the Assets and the Tokens from a wallet
 * then merges them into AssetTokens
 * @param assets - list of asset details
 * @param tokens - list of token details
 * See Asset/Token differences at the beginning of this doc
 */
const getAssetTokens = (assets, tokens) =>
  assets
    .map((asset) =>
      (0, exports.getAssetToken)(asset, (0, exports.getToken)(asset, tokens))
    )
    .filter((token) => !!token.uniqueId) // @TOKEN TODO - Remove this filter once we can list zero tokens
    .filter((token) => !token.quantity.isZero());
exports.getAssetTokens = getAssetTokens;
/**
 * Receives a Token
 * and combines with the data from the Asset
 * @param asset - asset details
 * @param getAsset - function that returns an asset
 * See Asset/Token differences at the beginning of this doc
 */
const getAssetTokenFromToken = (asset, getAsset) => {
  const { policyId, assetName, assetNameASCII, quantity, address } = asset;
  const { fingerprint, metadata, decimals, recommendedDecimals, uniqueId } =
    getAsset(policyId, assetName) || {};
  return {
    policyId,
    assetName,
    assetNameASCII,
    quantity,
    address,
    fingerprint,
    metadata,
    decimals,
    recommendedDecimals,
    uniqueId,
  };
};
exports.getAssetTokenFromToken = getAssetTokenFromToken;
const getNonZeroAssetTokens = (tokens, getAsset) =>
  tokens
    .map((token) => (0, exports.getAssetTokenFromToken)(token, getAsset))
    .filter((token) => !!token.uniqueId)
    .sort((0, exports.sortAssets)('fingerprint', 'asc'));
exports.getNonZeroAssetTokens = getNonZeroAssetTokens;
/**
 * High-order function for sorting assetTokens
 * @param sortBy - sorting parameter
 * @param sortDirection - should it sort in ascending or descending direction
 */
const sortAssets = (sortBy, sortDirection) => (asset1, asset2) => {
  const {
    quantity: unformattedQuantity1,
    fingerprint: fingerprint1,
    metadata: metadata1,
    decimals: decimals1,
  } = asset1;
  const quantity1 = (0, formatters_1.formattedTokenDecimals)(
    unformattedQuantity1,
    decimals1
  );
  const { name: name1 } = metadata1 || {};
  const {
    quantity: unformattedQuantity2,
    fingerprint: fingerprint2,
    metadata: metadata2,
    decimals: decimals2,
  } = asset2;
  const quantity2 = (0, formatters_1.formattedTokenDecimals)(
    unformattedQuantity2,
    decimals2
  );
  const { name: name2 } = metadata2 || {};
  if (sortBy === 'token') {
    if (name1 && !name2) return -1;
    if (!name1 && name2) return 1;
    if (name1 && name2) {
      if (sortDirection === 'asc') {
        return name1.localeCompare(name2);
      }
      return name2.localeCompare(name1);
    }
    if (sortDirection === 'asc') {
      return fingerprint1.localeCompare(fingerprint2);
    }
    return fingerprint2.localeCompare(fingerprint1);
  }
  if (sortBy === 'fingerprint') {
    if (sortDirection === 'asc') {
      return fingerprint1.localeCompare(fingerprint2);
    }
    return fingerprint2.localeCompare(fingerprint1);
  }
  if (sortBy === 'quantity') {
    if (sortDirection === 'asc') {
      return quantity1.isLessThan(quantity2) ? -1 : 1;
    }
    return quantity1.isLessThan(quantity2) ? 1 : -1;
  }
  return 0;
};
exports.sortAssets = sortAssets;
/**
 * Check if after the transactions your wallet has some assets left
 * @param allAvailableTokens Collection of assets in your wallet
 * @param initialSelectedAssets Collection of assets initially preselected
 * @param selectedAssets Selected assets to be send in the transaction
 * @returns {boolean}
 */
const hasTokensLeftAfterTransaction = (
  allAvailableTokens,
  initialSelectedAssets,
  selectedAssets
) => {
  if (
    !!selectedAssets &&
    selectedAssets.length &&
    selectedAssets.length > 0 &&
    !!initialSelectedAssets &&
    initialSelectedAssets?.length &&
    initialSelectedAssets?.length > 0
  ) {
    // If there is a minimal difference between the assets selected and the
    // ones available in your wallet means you left assets in your wallet
    if (
      initialSelectedAssets.length < allAvailableTokens.length ||
      selectedAssets.length < initialSelectedAssets.length
    ) {
      return true;
    }
    return !!(0, find_1.default)(
      selectedAssets,
      (selectedAsset, index) =>
        !initialSelectedAssets[index]?.quantity?.isEqualTo(selectedAsset)
    );
  }
  return false;
};
exports.hasTokensLeftAfterTransaction = hasTokensLeftAfterTransaction;
/**
 * Generic function for filtering AssetTokens
 * @param rawSearchValue - search value
 * @param assets - AssetTokens to operate the search
 */
const searchAssets = (rawSearchValue, assets) => {
  const searchValue = rawSearchValue.trim();
  if (searchValue.length < 3) {
    return assets;
  }
  return (0, lodash_1.filter)(assets, (asset) => {
    const {
      policyId,
      assetName,
      assetNameASCII,
      fingerprint,
      metadata,
    } = asset;
    const { name, ticker, description } = metadata || {};
    const checkList = [
      policyId,
      assetName,
      assetNameASCII,
      fingerprint,
      metadata,
      name,
      ticker,
      description,
    ];
    const regex = new RegExp((0, lodash_1.escapeRegExp)(searchValue), 'i');
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string | AssetMetadata' is not a... Remove this comment to see the full error message
    return checkList.some((item) => regex.test(item));
  });
};
exports.searchAssets = searchAssets;
const isTokenMissingInWallet = (wallet, token) => {
  if (!wallet || !token || !token.uniqueId) {
    return false;
  }
  const { available } = wallet.assets;
  const { uniqueId } = token;
  return !available.find((walletToken) => walletToken.uniqueId === uniqueId);
};
exports.isTokenMissingInWallet = isTokenMissingInWallet;
const tokenHasBalance = (token, amount) =>
  token.quantity.isGreaterThanOrEqualTo(amount);
exports.tokenHasBalance = tokenHasBalance;
const getUniqueId = ({ assetName, policyId }) => `${assetName}${policyId}`;
exports.getUniqueId = getUniqueId;
//# sourceMappingURL=assets.js.map
