// @flow
import BigNumber from 'bignumber.js';

import AssetDomain from '../../domains/Asset';

/**
 *
 * ASSET
 * Fetched from the Assets API endpoint
 * It's not attached to a particular wallet or transaction
 * Therefore, it doesn't have `quantity` nor `address`
 *
 * Exclusive data: fingerprint, metadata
 * Missing data: quantity, address
 *
 */
export type ApiAsset = {
  policy_id: string,
  asset_name: string,
  fingerprint: string,
  metadata?: ?AssetMetadata,
};
export type ApiAssets = Array<ApiAsset>;
export type Asset = {
  assetName: string,
  assetNameASCII: string,
  decimals: ?number,
  fingerprint: string,
  metadata?: ?AssetMetadata,
  policyId: string,
  recommendedDecimals: ?number,
  uniqueId: string,
};

/**
 *
 * TOKEN
 * Asset that is attached to a particular wallet and/or transaction
 * It doesn't have the Asset details (fingerprint, metadata)
 *
 * Exclusive data: quantity, address
 * Missing data: fingerprint, metadata
 *
 */
export type ApiToken = {
  policy_id: string,
  asset_name: string,
  quantity: number,
  address?: ?string,
};
export type ApiTokens = Array<ApiToken>;
export type Token = {
  policyId: string,
  assetName: string,
  quantity: BigNumber,
  address?: ?string,
  uniqueId: string,
};

export type Tokens = Array<Token>;
export type WalletTokens = {
  available: Tokens,
  total: Tokens,
};

/**
 *
 * ASSET TOKEN
 * Merged object from a Token and its relative Asset details
 *
 * It has all the data combined: quantity, address, fingerprint, metadata, etc.
 *
 */
export type AssetToken = {
  ...$Exact<Token>,
  ...$Exact<Asset>,
};

export type AssetMetadata = {
  name: string,
  description: string,
  ticker?: string,
  decimals?: number, // [0 .. 255]
  url?: string,
  logo?: string,
};

export type StoredAssetMetadata = {
  [uniqueId: string]: AssetMetadata,
};

export type GetUnknownAssetRequest = {
  walletId: string,
  policyId: string,
};

export type GetAssetsRequest = {
  walletId: string,
};

export type GetAssetsResponse = {
  assets: Array<AssetDomain>,
  total: number,
};
