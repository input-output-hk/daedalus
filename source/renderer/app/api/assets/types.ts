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
  policy_id: string;
  asset_name: string;
  fingerprint: string;
  metadata?: AssetMetadata | null | undefined;
};
export type ApiAssets = Array<ApiAsset>;
export type Asset = {
  assetName: string;
  decimals: number | null | undefined;
  fingerprint: string;
  metadata?: AssetMetadata | null | undefined;
  policyId: string;
  recommendedDecimals: number | null | undefined;
  uniqueId: string;
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
  policy_id: string;
  asset_name: string;
  quantity: number;
  address?: string | null | undefined;
};
export type ApiTokens = Array<ApiToken>;
export type Token = {
  policyId: string;
  assetName: string;
  quantity: BigNumber;
  address?: string | null | undefined;
  uniqueId: string;
};
export type Tokens = Array<Token>;
export type WalletTokens = {
  available: Tokens;
  total: Tokens;
};

/**
 *
 * ASSET TOKEN
 * Merged object from a Token and its relative Asset details
 *
 * It has all the data combined: quantity, address, fingerprint, metadata, etc.
 *
 */
export type AssetToken = Token & Asset;
export type AssetMetadata = {
  name: string;
  description: string;
  ticker?: string;
  decimals?: number;
  // [0 .. 255]
  url?: string;
  logo?: string;
};
export type StoredAssetMetadata = Record<string, AssetMetadata>;
export type GetUnknownAssetRequest = {
  walletId: string;
  policyId: string;
};
export type GetAssetsRequest = {
  walletId: string;
};
export type GetAssetsResponse = {
  assets: Array<AssetDomain>;
  total: number;
};
