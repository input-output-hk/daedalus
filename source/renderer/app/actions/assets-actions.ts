import Action from './lib/Action';
import type { AssetToken } from '../api/assets/types'; // ======= ASSETS ACTIONS =======

export default class AssetsActions {
  onAssetSettingsOpen: Action<{
    asset: AssetToken;
  }> = new Action();
  onAssetSettingsSubmit: Action<{
    asset: AssetToken;
    decimals: number;
  }> = new Action();
  onAssetSettingsCancel: Action<any> = new Action();
  onOpenAssetSend: Action<{
    uniqueId: string;
  }> = new Action();
  onCopyAssetParam: Action<{
    param: string;
    fullValue: string;
  }> = new Action();
  onToggleFavorite: Action<{
    uniqueId: string;
    isFavorite: boolean;
  }> = new Action();
  copyAssetParamNotification: Action<{
    param: string;
    shortValue: string;
  }> = new Action();
}
