// @flow
import React from 'react';
import { observer, inject } from 'mobx-react';
import WalletTokens from '../../components/wallet/tokens/WalletTokens';
import type { InjectedProps } from '../../types/injectedPropsType';
import { getAssetTokens } from '../../utils/assets';

type Props = InjectedProps;

const WalletTokensPage = inject(
  'stores',
  'actions'
)(
  observer((props: Props) => {
    const { actions, stores } = props;
    const { assets, profile, wallets } = stores;
    const { getAsset, assetSettingsDialogWasOpened } = assets;
    const { onAssetSettingsOpen, onOpenAssetSend } = actions.assets;
    const { active: activeWallet } = wallets;
    const { currentLocale } = profile;

    // Guard against potential null values
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSummaryPage.');

    const walletTokens = activeWallet.assets.total;
    const assetTokens = getAssetTokens(walletTokens, getAsset);
    const totalRawAssets = activeWallet.assets.total.length;
    const totalAssets = assetTokens.length;
    const hasRawAssets = activeWallet.assets.total.length > 0;
    const isLoadingAssets = hasRawAssets && totalAssets < totalRawAssets;

    return (
      <WalletTokens
        assets={assetTokens}
        assetSettingsDialogWasOpened={assetSettingsDialogWasOpened}
        currentLocale={currentLocale}
        isLoadingAssets={isLoadingAssets}
        onAssetSettings={onAssetSettingsOpen.trigger}
        onCopyAssetParam={() => {}}
        onOpenAssetSend={onOpenAssetSend.trigger}
        wallet={activeWallet}
      />
    );
  })
);

export default WalletTokensPage;
