// @flow
import Action from './lib/Action';
import type { WalletSummaryAsset } from '../api/assets/types';

// ======= ADDRESSES ACTIONS =======

export default class AddressesActions {
  onEditAssetOpen: Action<{ asset: WalletSummaryAsset }> = new Action();
  onEditAssetSubmit: Action<{
    asset: WalletSummaryAsset,
    decimalPrecision: number,
  }> = new Action();
  onEditAssetCancel: Action<any> = new Action();
}
