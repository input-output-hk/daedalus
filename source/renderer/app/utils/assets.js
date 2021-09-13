// @flow
import find from 'lodash/find';
import BigNumber from 'bignumber.js';
import Wallet from '../domains/Wallet';
import type { Token, Tokens, AssetToken } from '../api/assets/types';
import { TransactionTypes } from '../domains/WalletTransaction';
import type { TransactionType } from '../api/transactions/types';

/**
 *
 * This function removes the `change` assets
 * that are included in the TX API response
 *
 */
export const filterAssets = (
  assets: Array<any>,
  transactionType: TransactionType,
  isInternalAddress: Function
): Array<any> =>
  assets.filter(
    ({ address }) =>
      (transactionType === TransactionTypes.INCOME &&
        isInternalAddress(address)) ||
      (transactionType === TransactionTypes.EXPEND &&
        !isInternalAddress(address))
  );

/**
 *
 * This function receives a Token (the asset included in a wallet or transaction)
 * and combines with the data from the Asset
 *
 * Data from the Token: policyId, assetName, quantity, address
 * Data from the Asset: fingerprint, metadata, decimals, recommendedDecimals, uniqueId
 */
export const getAssetToken = (asset: Token, getAsset: Function): AssetToken => {
  const { policyId, assetName, quantity, address } = asset;
  const { fingerprint, metadata, decimals, recommendedDecimals, uniqueId } =
    getAsset(policyId, assetName) || {};
  return {
    policyId,
    assetName,
    quantity,
    address,
    fingerprint,
    metadata,
    decimals,
    recommendedDecimals,
    uniqueId,
  };
};

/**
 *
 * This function receives a list of Tokens (the assets included in a wallet or transaction)
 * then retrieves the Assets
 * and sort them accordingly
 *
 */
export const getAssetTokens = (
  tokens: Tokens,
  getAsset: Function
): Array<AssetToken> =>
  tokens
    .map((token) => getAssetToken(token, getAsset))
    .filter((token) => !!token.uniqueId)
    .sort(sortAssets);

export const sortAssets = (asset1: AssetToken, asset2: AssetToken) => {
  if (asset1 && asset2) {
    if (asset1.fingerprint < asset2.fingerprint) {
      return -1;
    }
    if (asset1.fingerprint > asset2.fingerprint) {
      return 1;
    }
  }
  return 0;
};

/**
 * Check if after the transactions your wallet has some assets left
 *
 * @param allAvailableTokens Collection of assets in your wallet
 * @param initialSelectedAssets Collection of assets initially preselected
 * @param selectedAssets Selected assets to be send in the transaction
 * @returns {boolean}
 */
export const hasTokensLeftAfterTransaction = (
  allAvailableTokens: AssetToken[],
  initialSelectedAssets: AssetToken[],
  selectedAssets?: string[]
): boolean => {
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
    return !!find(
      selectedAssets,
      (selectedAsset, index) =>
        !initialSelectedAssets[index]?.quantity?.isEqualTo(selectedAsset)
    );
  }
  return false;
};

export const isTokenMissingInWallet = (wallet?: ?Wallet, token?: Token) => {
  if (!wallet || !token || !token.uniqueId) {
    return false;
  }
  const { available } = wallet.assets;
  const { uniqueId } = token;
  return !available.find((walletToken) => walletToken.uniqueId === uniqueId);
};

export const tokenHasBalance = (token: Token, amount: BigNumber) =>
  token.quantity.isGreaterThanOrEqualTo(amount);
