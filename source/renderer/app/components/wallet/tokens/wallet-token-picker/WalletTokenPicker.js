// @flow
import React from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import classNames from 'classnames';
import Dialog from '../../../widgets/Dialog';
import WalletToken from '../wallet-token/WalletToken';
import WalletTokensSearch from '../wallet-tokens-search/WalletTokensSearch';
import { ScrollPositionEnum } from './helpers';
import { useSearch, useScrollPosition } from './hooks';
import styles from './WalletTokenPicker.scss';
import { messages } from './WalletTokenPicker.messages';
import type { Intl } from '../../../../types/i18nTypes';
import type { AssetToken } from '../../../../api/assets/types';

type Props = {
  intl: Intl,
  assets: Array<AssetToken>,
  tokenFavorites: Object,
};

const WalletTokenPicker = ({ intl, assets, tokenFavorites }: Props) => {
  const { searchValue, setSearchValue } = useSearch();
  const { scrollableRef, scrollPosition } = useScrollPosition();
  const toolbarStyles = classNames(
    styles.toolbar,
    scrollPosition !== ScrollPositionEnum.TOP && styles.scrollTop
  );

  return (
    <Dialog
      className={styles.dialog}
      title={intl.formatMessage(messages.title)}
      closeOnOverlayClick
      theme
    >
      <div className={styles.root}>
        <WalletTokensSearch
          searchValue={searchValue}
          onSearch={setSearchValue}
        />
        <div className={toolbarStyles}>
          <span className={styles.count}>2 of 30 max tokens</span>
        </div>
        <div className={styles.tokens} ref={scrollableRef}>
          {assets.map((asset) => (
            <WalletToken
              key={asset.uniqueId}
              asset={asset}
              fullFingerprint={false}
              isFavorite={tokenFavorites[asset.uniqueId]}
            />
          ))}
        </div>
      </div>
    </Dialog>
  );
};

export default injectIntl(observer(WalletTokenPicker));
