import BigNumber from 'bignumber.js';

/**
 *
 * ASSET
 * The metadata for a subject, resolved from the local cache.
 * It's not attached to a particular wallet or transaction
 * Therefore, it doesn't have `quantity` nor `address`
 *
 * Exclusive data: fingerprint, metadata
 * Missing data: quantity, address
 *
 */
export type Asset = {
  assetName: string;
  decimals?: number | null;
  fingerprint: string;
  metadata?: AssetMetadata | null;
  policyId: string;
  recommendedDecimals?: number | null;
  /**
   * Whether `recommendedDecimals` was cryptographically bound to the token's
   * minting policy. Only a verified value is applied automatically; an
   * unverified one is offered in the settings dialog and formats nothing.
   */
  recommendedDecimalsVerified?: boolean;
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
  assetNameASCII?: string;
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
  decimals?: number; // [0 .. 255]
  url?: string;
  logo?: string;
};
