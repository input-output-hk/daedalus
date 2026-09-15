import BigNumber from 'bignumber.js';
import type { AssetMetadataSource } from '../../../../common/types/asset-metadata.types';

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
  /**
   * Whether the cache holds a logo for this subject. It is not the logo: the
   * bytes travel on their own channel, one subject at a time, so that a picture
   * never sits on the path of a name or an amount.
   */
  hasImage?: boolean;
  /**
   * Which channel the cached row came from, or null when there is no row.
   *
   * It decides how a name is labelled rather than whether it is shown. A name
   * on a chain row is in the transaction that minted the asset, which had to
   * satisfy the minting policy, so it is bound to that policy; a decoded asset
   * name is bound to nothing.
   */
  source?: AssetMetadataSource | null;
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

/** The current tip of a candidate metadata source, as its `/tip` reports it. */
export type AssetMetadataSourceTip = {
  absoluteSlot: number;
};

/**
 * Whether a candidate metadata source may be stored, and when not, why.
 *
 * Two-valued would be enough to refuse. It is three-valued because the two
 * refusals mean different things to the person who typed the URL: one says this
 * is not an instance, the other says this instance is behind.
 */
export type AssetMetadataSourceCheck =
  | { valid: true }
  | { valid: false; reason: 'unreachable' | 'stale' };
