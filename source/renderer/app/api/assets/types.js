// @flow
import BigNumber from 'bignumber.js';

import Asset from '../../domains/Asset';
import type { AssetDomainProps } from '../../domains/Asset';

export type Assets = Array<ApiAsset>;
export type ApiAsset = {
  policy_id: string,
  asset_name: string,
  fingerprint: string,
  metadata?: ?AssetMetadata,
};

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

export type AssetTokenProps = {
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
