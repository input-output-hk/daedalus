// @flow
import Action from './lib/Action';
import type { WalletSummaryAsset } from '../api/assets/types';

// ======= ASSETS ACTIONS =======

export default class AssetsActions {
  onAssetSettingsOpen: Action<{ asset: WalletSummaryAsset }> = new Action();
  onAssetSettingsSubmit: Action<{
    asset: WalletSummaryAsset,
    decimals: number,
  }> = new Action();
  onAssetSettingsCancel: Action<any> = new Action();
}
