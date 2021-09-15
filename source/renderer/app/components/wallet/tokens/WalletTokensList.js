// @flow
import React from 'react';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import { observer } from 'mobx-react';
import { searchAssets } from '../../../utils/assets';

import styles from './WalletTokensList.scss';
import Wallet from '../../../domains/Wallet';
import BorderedBox from '../../widgets/BorderedBox';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import WalletToken from './WalletToken';
import type { AssetToken } from '../../../api/assets/types';

const messages = defineMessages({
  noResults: {
    id: 'wallet.tokens.search.noResults',
    defaultMessage: '!!!No results matching your query',
    description: 'No results on the WalletTokensList',
  },
  searchResults: {
    id: 'wallet.tokens.search.searchResults',
    defaultMessage: '!!!Search Results',
    description: 'Search Results on the WalletTokensList',
  },
});

type Props = {
  assets: Array<AssetToken>,
  assetSettingsDialogWasOpened: boolean,
  currentLocale: string,
  onViewAllButtonClick?: Function,
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
    assets,
    assetSettingsDialogWasOpened,
    onViewAllButtonClick,
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
  const hasSearch =
    !isLoadingAssets && !!searchValue && searchValue.trim().length >= 3;
  const noResults = hasSearch && !filteredAssets.length;

  let content;

  if (isLoadingAssets) {
    content = (
      <div className={styles.syncingWrapper}>
        <LoadingSpinner big />
      </div>
    );
  } else if (noResults) {
    content = (
      <p className={styles.noResults}>
        {intl.formatMessage(messages.noResults)}
      </p>
    );
  } else {
    content = filteredAssets.map((asset) => (
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
    ));
  }

  return (
    <div className={styles.component}>
      <div className={styles.outerHeader}>
        <div className={styles.title}>
          {title}
          {hasSearch && !noResults && (
            <>
              {intl.formatMessage(messages.searchResults)} (
              {filteredAssets.length})
            </>
          )}
        </div>
      </div>
      <BorderedBox>
        {content}
        {onViewAllButtonClick && (
          <button onClick={onViewAllButtonClick}>VIEW ALL</button>
        )}
      </BorderedBox>
    </div>
  );
});

export default injectIntl(WalletTokensList);
