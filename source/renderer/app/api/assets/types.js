// @flow

export type Asset = {
  id: string,
  policy_id: string,
  asset_name: string,
  metadata?: ?AssetMetadata,
};

export type AssetMetadata = {
  name: string,
  acronym: string,
  description: string,
  unit: {
    decimals: number,
    name: string,
  },
  url: string,
  logo: string,
};

export type AssetItem = {
  id: string,
  policy_id: string,
  asset_name: string,
  quantity: number,
};

export type Assets = Array<Asset>;

export type AssetItems = Array<AssetItem>;

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
