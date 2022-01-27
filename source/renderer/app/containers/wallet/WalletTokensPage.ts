// @flow
import React from 'react';
import { observer, inject } from 'mobx-react';
import WalletTokens from '../../components/wallet/tokens/wallet-tokens/WalletTokens';
import type { InjectedProps } from '../../types/injectedPropsType';
import { getAssetTokens } from '../../utils/assets';

type Props = InjectedProps;

const WalletTokensPage = inject(
  'stores',
  'actions'
)(
  observer((props: Props) => {
    const { actions, stores } = props;
    const { assets, profile, wallets, app } = stores;
    const {
      all,
      assetSettingsDialogWasOpened,
      favorites,
      insertingAssetUniqueId,
      removingAssetUniqueId,
    } = assets;
    const {
      onAssetSettingsOpen,
      onOpenAssetSend,
      onToggleFavorite,
    } = actions.assets;
    const { active: activeWallet } = wallets;
    const { currentLocale } = profile;

    // Guard against potential null values
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSummaryPage.');

    const walletTokens = activeWallet.assets.total;
    const assetTokens = getAssetTokens(all, walletTokens);
    const totalRawAssets = activeWallet.assets.total.length;
    const totalAssets = assetTokens.length;
    const hasRawAssets = activeWallet.assets.total.length > 0;
    const isLoadingAssets = hasRawAssets && totalAssets < totalRawAssets;

    return (
      <WalletTokens
        assets={assetTokens}
        assetSettingsDialogWasOpened={assetSettingsDialogWasOpened}
        currentLocale={currentLocale}
        insertingAssetUniqueId={insertingAssetUniqueId}
        isLoadingAssets={isLoadingAssets}
        onAssetSettings={onAssetSettingsOpen.trigger}
        onCopyAssetParam={() => {}}
        onOpenAssetSend={onOpenAssetSend.trigger}
        onToggleFavorite={onToggleFavorite.trigger}
        removingAssetUniqueId={removingAssetUniqueId}
        onExternalLinkClick={app.openExternalLink}
        tokenFavorites={favorites}
        wallet={activeWallet}
      />
    );
  })
);

export default WalletTokensPage;
