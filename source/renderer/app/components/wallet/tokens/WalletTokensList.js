// @flow
import React from 'react';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import { observer } from 'mobx-react';
import { searchAssets } from '../../../utils/assets';

import styles from './WalletTokens.scss';
import Wallet from '../../../domains/Wallet';
import BorderedBox from '../../widgets/BorderedBox';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import WalletToken from './WalletToken';
import type { AssetToken } from '../../../api/assets/types';

const messages = defineMessages({
  noResults: {
    id: 'wallet.tokens.search.noResults',
    defaultMessage: '!!!No results matching your query',
    description: 'No results on Wallet summary assets page',
  },
});

type Props = {
  assets: Array<AssetToken>,
  assetSettingsDialogWasOpened: boolean,
  currentLocale: string,
  intl: intlShape.isRequired,
  isLoadingAssets: boolean,
  onAssetSettings: Function,
  onCopyAssetParam: Function,
  onOpenAssetSend: Function,
  searchValue?: string,
  title: string,
  wallet: Wallet,
};

const WalletTokensList = observer((props: Props) => {
  const {
    // anyAssetWasHovered,
    assets,
    assetSettingsDialogWasOpened,
    intl,
    isLoadingAssets,
    onAssetSettings,
    onCopyAssetParam,
    onOpenAssetSend,
    searchValue = '',
    title,
    wallet,
  } = props;
  const isRestoreActive = wallet.isRestoring;

  const filteredAssets = searchAssets(searchValue, assets);

  const noResults =
    !filteredAssets.length && searchValue && searchValue.trim().length >= 3;

  if (isLoadingAssets) {
    return (
      <div className={styles.syncingWrapper}>
        <LoadingSpinner big />
      </div>
    );
  }

  if (noResults) {
    return (
      <p className={styles.noResults}>
        {intl.formatMessage(messages.noResults)}
      </p>
    );
  }

  return (
    <div className={styles.component}>
      <h3>{title}</h3>
      <BorderedBox>
        {filteredAssets.map((asset) => (
          <WalletToken
            key={asset.uniqueId}
            asset={asset}
            onOpenAssetSend={onOpenAssetSend}
            onCopyAssetParam={onCopyAssetParam}
            onAssetSettings={onAssetSettings}
            anyAssetWasHovered
            isLoading={isRestoreActive}
            assetSettingsDialogWasOpened={assetSettingsDialogWasOpened}
          />
        ))}
      </BorderedBox>
    </div>
  );
});

export default injectIntl(WalletTokensList);
