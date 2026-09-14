import React, { useCallback } from 'react';
import { observer, inject } from 'mobx-react';
import type { AssetToken } from '../../api/assets/types';
import AssetSettingsDialog from '../../components/assets/AssetSettingsDialog';
import WalletTokens from '../../components/wallet/tokens/wallet-tokens/WalletTokens';
import type { InjectedProps } from '../../types/injectedPropsType';
import { getNonZeroAssetTokens } from '../../utils/assets';

type Props = InjectedProps;
type OpenAssetSettingsDialogArgs = {
  asset: AssetToken;
};
const WalletTokensPage = inject(
  'stores',
  'actions'
)(
  observer((props: Props) => {
    const { actions, stores } = props;
    const { assets, profile, wallets, app } = stores;
    const {
      getAsset,
      favorites,
      insertingAssetUniqueId,
      removingAssetUniqueId,
    } = assets;
    const { setEditedAsset, onOpenAssetSend, onToggleFavorite } =
      actions.assets;
    const { open } = actions.dialogs;
    const { active: activeWallet } = wallets;
    const { currentLocale, isDecimalPlacesNoticeAcknowledged } = profile;
    const { acknowledgeDecimalPlacesNotice } = actions.profile;
    const openAssetSettingsDialog = useCallback(
      ({ asset }: OpenAssetSettingsDialogArgs) => {
        setEditedAsset.trigger({
          asset,
        });
        open.trigger({
          dialog: AssetSettingsDialog,
        });
      },
      [open, setEditedAsset]
    );
    // Guard against potential null values
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSummaryPage.');
    const walletTokens = activeWallet.assets.total;
    const assetTokens = getNonZeroAssetTokens(walletTokens, getAsset);
    return (
      <WalletTokens
        assets={assetTokens}
        currentLocale={currentLocale}
        insertingAssetUniqueId={insertingAssetUniqueId}
        onAssetSettings={openAssetSettingsDialog}
        onCopyAssetParam={() => {}}
        onOpenAssetSend={onOpenAssetSend.trigger}
        onToggleFavorite={onToggleFavorite.trigger}
        removingAssetUniqueId={removingAssetUniqueId}
        onExternalLinkClick={app.openExternalLink}
        tokenFavorites={favorites}
        wallet={activeWallet}
        isDecimalPlacesNoticeAcknowledged={isDecimalPlacesNoticeAcknowledged}
        onAcknowledgeDecimalPlacesNotice={
          acknowledgeDecimalPlacesNotice.trigger
        }
      />
    );
  })
);
export default WalletTokensPage;
