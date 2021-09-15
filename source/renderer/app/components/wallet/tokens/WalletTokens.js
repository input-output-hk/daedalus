// @flow
import React, { useState } from 'react';
import { intlShape, injectIntl, defineMessages } from 'react-intl';
import { omit } from 'lodash';
import { observer } from 'mobx-react';
import styles from './WalletTokens.scss';
import Wallet from '../../../domains/Wallet';
import WalletTokensList from './WalletTokensList';
import WalletTokensSearch from './WalletTokensSearch';
import type { AssetToken } from '../../../api/assets/types';

const messages = defineMessages({
  favoritesListTitle: {
    id: 'wallet.tokens.list.favorites.title',
    defaultMessage: '!!!Favorites',
    description: 'Favorites list title label',
  },
  tokensListTitle: {
    id: 'wallet.tokens.list.tokens.title',
    defaultMessage: '!!!Tokens',
    description: 'Favorites list title label',
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
  wallet: Wallet,
  intl: intlShape.isRequired,
};
type SearchValue = string;
const WalletTokens = observer((props: Props) => {
  const [searchValue, setSearchValue] = useState<SearchValue>('');

  const { assets, intl } = props;
  const listProps = { ...omit(props, 'assets', 'intl'), searchValue };
  const favoriteTokens = [...assets.slice(0, 1)];

  console.log(intl.formatMessage(messages.favoritesListTitle));
  return (
    <div className={styles.component}>
      <WalletTokensSearch searchValue={searchValue} onSearch={setSearchValue} />
      <WalletTokensList
        assets={favoriteTokens}
        title={intl.formatMessage(messages.favoritesListTitle)}
        {...listProps}
      />
      <WalletTokensList
        assets={assets}
        title={intl.formatMessage(messages.tokensListTitle)}
        {...listProps}
      />
    </div>
  );
});

export default injectIntl(WalletTokens);
