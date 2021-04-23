// @flow
import Action from './lib/Action';
import type { WalletSummaryAsset } from '../api/assets/types';

// ======= ADDRESSES ACTIONS =======

export default class AddressesActions {
  onAssetSettingsOpen: Action<{ asset: WalletSummaryAsset }> = new Action();
  onAssetSettingsSubmit: Action<{
    asset: WalletSummaryAsset,
    decimals: number,
  }> = new Action();
  onAssetSettingsCancel: Action<any> = new Action();
}
