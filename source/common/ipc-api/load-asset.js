// @flow
export const LOAD_ASSET_CHANNEL = 'LOAD_ASSET';
export type LoadAssetRequest = { fileName: string };
export type LoadAssetResponse = string;

const ASSETS_LOADED_CHANNEL = 'assets-loaded';
export const ASSETS_LOADED = {
  SUCCESS: `${ASSETS_LOADED_CHANNEL}-success`,
};
