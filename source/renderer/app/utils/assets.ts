import find from 'lodash/find';
import BigNumber from 'bignumber.js';
import { filter, escapeRegExp, reduce } from 'lodash';
import Wallet from '../domains/Wallet';
import type { Token, Tokens, AssetToken } from '../api/assets/types';
import { TransactionTypes } from '../domains/WalletTransaction';
import type { TransactionType } from '../api/transactions/types';
import { formattedTokenDecimals } from './formatters';
import { hexToString } from './strings';
import {
  ASSET_METADATA_SERVERS_LIST,
  ASSET_METADATA_SOURCE_MAX_TIP_LAG_SLOTS,
  ASSET_METADATA_SOURCE_TYPES,
} from '../config/assetsConfig';
import type { AssetMetadataSourceType } from '../types/assetTypes';

export type SortBy = 'token' | 'fingerprint' | 'quantity';
export type SortDirection = 'asc' | 'desc';

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
export const filterAssets = (
  assets: Array<any>,
  transactionType: TransactionType,
  isInternalAddress: (...args: Array<any>) => any
): Array<any> =>
  assets.filter(
    ({ address }) =>
      (transactionType === TransactionTypes.INCOME &&
        isInternalAddress(address)) ||
      (transactionType === TransactionTypes.EXPEND &&
        !isInternalAddress(address))
  );
/**
 * Receives a Token and combines it with the registry data for the same subject.
 *
 * Identity comes from the token and never from the lookup: a token the wallet
 * holds exists whether or not anything has been cached about it, so taking
 * `uniqueId` from the lookup would make an unresolved asset disappear from the
 * send form, the send confirmation and the transaction list. Only `metadata`,
 * `decimals`, `recommendedDecimals`, `recommendedDecimalsVerified`, `hasImage`,
 * `source` and `fingerprint` come from the lookup.
 *
 * A token built from a transaction response carries neither `uniqueId` nor
 * `assetNameASCII`, so both are derived here in the same shape the
 * wallet-balance mapping gives them: the policy id followed by the asset name,
 * and the asset name decoded with `hexToString`.
 *
 * @param token - token details
 * @param getAsset - function that returns an asset
 * See Asset/Token differences at the beginning of this doc
 */
export const getAssetTokenFromToken = (
  token: Token,
  getAsset: (...args: Array<any>) => any
): AssetToken => {
  const { policyId, assetName, assetNameASCII, uniqueId } = token;
  const {
    fingerprint,
    metadata,
    decimals,
    recommendedDecimals,
    recommendedDecimalsVerified,
    hasImage,
    source,
  } = getAsset(policyId, assetName) || {};
  return {
    ...token,
    uniqueId: uniqueId || `${policyId}${assetName}`,
    assetNameASCII: assetNameASCII || hexToString(assetName || ''),
    fingerprint,
    metadata,
    decimals,
    recommendedDecimals,
    recommendedDecimalsVerified,
    hasImage,
    source,
  };
};
export const getNonZeroAssetTokens = (
  tokens: Tokens,
  getAsset: (...args: Array<any>) => any
): Array<AssetToken> =>
  tokens
    .map((token) => getAssetTokenFromToken(token, getAsset))
    .sort(sortAssets('fingerprint', 'asc'));

/**
 * High-order function for sorting assetTokens
 * @param sortBy - sorting parameter
 * @param sortDirection - should it sort in ascending or descending direction
 */
export const sortAssets =
  (sortBy: SortBy, sortDirection: SortDirection) =>
  (asset1: AssetToken, asset2: AssetToken) => {
    const {
      quantity: unformattedQuantity1,
      fingerprint: fingerprint1,
      metadata: metadata1,
      decimals: decimals1,
    } = asset1;
    const quantity1 = formattedTokenDecimals(unformattedQuantity1, decimals1);
    const { name: name1 } = metadata1 || {};
    // A token the wallet holds is rendered whether or not anything has been
    // cached about it, and a fingerprint arrives with the cached row, so the
    // comparator has to order rows that do not have one yet. Rows without a
    // fingerprint sort together, ahead of the rest, and keep the order they
    // arrived in.
    const sortableFingerprint1 = fingerprint1 || '';
    const {
      quantity: unformattedQuantity2,
      fingerprint: fingerprint2,
      metadata: metadata2,
      decimals: decimals2,
    } = asset2;
    const quantity2 = formattedTokenDecimals(unformattedQuantity2, decimals2);
    const { name: name2 } = metadata2 || {};
    const sortableFingerprint2 = fingerprint2 || '';

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
        return sortableFingerprint1.localeCompare(sortableFingerprint2);
      }

      return sortableFingerprint2.localeCompare(sortableFingerprint1);
    }

    if (sortBy === 'fingerprint') {
      if (sortDirection === 'asc') {
        return sortableFingerprint1.localeCompare(sortableFingerprint2);
      }

      return sortableFingerprint2.localeCompare(sortableFingerprint1);
    }

    if (sortBy === 'quantity') {
      if (sortDirection === 'asc') {
        return quantity1.isLessThan(quantity2) ? -1 : 1;
      }

      return quantity1.isLessThan(quantity2) ? 1 : -1;
    }

    return 0;
  };

/**
 * Check if after the transactions your wallet has some assets left
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

/**
 * Generic function for filtering AssetTokens
 * @param rawSearchValue - search value
 * @param assets - AssetTokens to operate the search
 */
export const searchAssets = (
  rawSearchValue: string,
  assets: Array<AssetToken>
) => {
  const searchValue = rawSearchValue.trim();

  if (searchValue.length < 3) {
    return assets;
  }

  return filter(assets, (asset) => {
    const { policyId, assetName, assetNameASCII, fingerprint, metadata } =
      asset;
    const { name, ticker, description } = metadata || {};
    // Only the fields that are text, and only where there is any. `test`
    // coerces its argument, so an absent field would be searched as the literal
    // "undefined" and a three-letter search for `und` would match every row the
    // cache has not resolved. The metadata object itself was in this list and
    // coerced to "[object Object]"; its three text properties are here in their
    // own right, so nothing is lost by dropping it.
    const checkList = [
      policyId,
      assetName,
      assetNameASCII,
      fingerprint,
      name,
      ticker,
      description,
    ].filter((item) => typeof item === 'string');
    const regex = new RegExp(escapeRegExp(searchValue), 'i');
    return checkList.some((item) => regex.test(item));
  });
};
export const isTokenMissingInWallet = (
  wallet?: Wallet | null | undefined,
  token?: Token
) => {
  if (!wallet || !token || !token.uniqueId) {
    return false;
  }

  const { available } = wallet.assets;
  const { uniqueId } = token;
  return !available.find((walletToken) => walletToken.uniqueId === uniqueId);
};
export const tokenHasBalance = (token: Token, amount: BigNumber) =>
  token.quantity.isGreaterThanOrEqualTo(amount);

/**
 * The preset a stored URL belongs to, or `custom`.
 *
 * The same reduction as `getSmashServerIdFromUrl` at `utils/staking.ts:17-28`,
 * and it exists for the same reason: a user who pastes the default URL should
 * see the default selected rather than a custom entry holding the same string.
 * It carries no suppression where that one does, because the preset list's keys
 * and the fallback are both typed.
 */
export const getAssetMetadataSourceIdFromUrl = (
  sourceUrl: string
): AssetMetadataSourceType =>
  reduce(
    ASSET_METADATA_SERVERS_LIST,
    (result: AssetMetadataSourceType, entry, id) => {
      if (entry && entry.url === sourceUrl) {
        return id as AssetMetadataSourceType;
      }
      return result;
    },
    ASSET_METADATA_SOURCE_TYPES.CUSTOM
  );

/**
 * Whether a candidate source is current enough to read pointers from.
 *
 * One-directional. A source ahead of the local tip is fine, because a node that
 * is still syncing is behind everything; only a source that lags the user's own
 * node by more than the bound is refused. A null local tip is the state before
 * the first network status arrives, and it accepts: refusing every source for
 * the length of a first sync would make the setting unusable exactly when
 * someone is most likely to open it.
 *
 * Lives here rather than in `api.ts` so the boundary can be driven without
 * building an `AdaApi`.
 */
export const assetMetadataSourceTipIsFresh = (
  sourceTipSlot: number,
  localTipSlot: number | null | undefined
): boolean => {
  if (typeof localTipSlot !== 'number' || !Number.isFinite(localTipSlot)) {
    return true;
  }
  return (
    localTipSlot - sourceTipSlot <= ASSET_METADATA_SOURCE_MAX_TIP_LAG_SLOTS
  );
};
