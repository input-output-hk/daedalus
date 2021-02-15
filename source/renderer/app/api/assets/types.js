// @flow

import Asset from '../../domains/Asset';

export type SingleAsset = {
  id: string,
  policy_id: string,
  asset_name: string,
  metadata?: ?AssetMetadata,
};

export type ApiAsset = {
  policy_id: string,
  asset_name: string,
  fingerprint: string,
  metadata?: ?AssetMetadata,
};

export type AssetMetadata = {
  name: string,
  acronym: string,
  description: string,
  unit?: {
    decimals: number,
    name: string,
  },
  url?: string,
  logo?: string,
};

export type AssetItem = {
  id: string,
  policy_id: string,
  asset_name: string,
  quantity: number,
};

export type WalletAssetItem = {
  id: string,
  policyId: string,
  assetName: string,
  quantity: number,
};

export type Assets = Array<SingleAsset>;

export type AssetItems = Array<AssetItem>;

export type WalletAssetItems = Array<WalletAssetItem>;

export type WalletAssets = {
  available: WalletAssetItems,
  total: WalletAssetItems,
};

export type WalletSummaryAsset = {
  id: string,
  metadata: ?AssetMetadata,
  total: WalletAssetItem,
};

export type WalletTransactionAsset = {
  id: string,
  policyId: string,
  assetName: string,
  quantity: number,
  metadata: ?AssetMetadata,
};

export type GetAssetRequest = {
  walletId: string,
  policyId: string,
  assetName: string,
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

export type GetAssetResponse = {
  asset: Asset,
};
