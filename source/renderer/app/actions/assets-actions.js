// @flow
import Action from './lib/Action';
import type { AssetTokenProps } from '../api/assets/types';

// ======= ASSETS ACTIONS =======

export default class AssetsActions {
  onAssetSettingsOpen: Action<{ asset: AssetTokenProps }> = new Action();
  onAssetSettingsSubmit: Action<{
    asset: AssetTokenProps,
    decimals: number,
  }> = new Action();
  onAssetSettingsCancel: Action<any> = new Action();
}
