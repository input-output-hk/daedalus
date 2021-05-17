// @flow
import BigNumber from 'bignumber.js';

import Asset from '../../domains/Asset';
import type { AssetDomainProps } from '../../domains/Asset';

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
export type Assets = Array<ApiAsset>;
export type ApiAsset = {
  policy_id: string,
  asset_name: string,
  fingerprint: string,
  metadata?: ?AssetMetadata,
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
  ...$Exact<AssetDomainProps>,
};

export type AssetMetadata = {
  name: string,
  ticker: string,
  description: string,
  unit?: {
    decimals: number,
    name: string,
  },
  url?: string,
  logo?: string,
};

export type GetUnknownAssetRequest = {
  walletId: string,
  policyId: string,
};

export type GetAssetsRequest = {
  walletId: string,
};

export type GetAssetsResponse = {
  assets: Array<Asset>,
  total: number,
};
