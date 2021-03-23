// @flow
import type {
  WalletAssetItem,
  WalletAssetItems,
  WalletTransactionAsset,
} from '../api/assets/types';
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
 * This function receives a list os Wallet Assets
 * which don't contain fingerprint nor metadata
 *
 * And a function which gathers this missing items
 * from the `details` in the Assets Store
 *
 */
export const getTransactionAsset = (
  asset: WalletAssetItem,
  getAssetDetails: Function
): WalletTransactionAsset => {
  const { policyId, assetName, quantity, address } = asset;
  const { fingerprint, metadata } = getAssetDetails(policyId, assetName) || {};
  const txAsset = {
    policyId,
    assetName,
    quantity,
    address,
    fingerprint,
    metadata,
  };
  return txAsset;
};

/**
 *
 * This function gets the complete Tx Asset from `getTransactionAsset`
 * and sorts it accordingly
 *
 */
export const getTransactionAssets = (
  assets: WalletAssetItems,
  getAssetDetails: Function
): Array<WalletTransactionAsset> =>
  assets
    .map((rawAsset) => getTransactionAsset(rawAsset, getAssetDetails))
    .sort((asset1, asset2) => {
      if (asset1 && asset2) {
        if (asset1.fingerprint < asset2.fingerprint) {
          return -1;
        }
        if (asset1.fingerprint > asset2.fingerprint) {
          return 1;
        }
      }
      return 0;
    });
