// @flow
import React from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import styles from './WalletSummaryAssets.scss';
import Wallet from '../../../domains/Wallet';
import type { AssetToken } from '../../../api/assets/types';

import WalletTokensList from '../tokens/WalletTokensList';

const messages = defineMessages({
  tokensTitle: {
    id: 'wallet.summary.assets.tokensTitle',
    defaultMessage: '!!!Tokens',
    description: 'Number of tokens title on Wallet summary assets page',
  },
});

type Props = {
  assets: Array<AssetToken>,
  assetSettingsDialogWasOpened: boolean,
  currentLocale: string,
  isLoadingAssets: boolean,
  onAssetSettings: Function,
  onCopyAssetParam: Function,
  onOpenAssetSend: Function,
  intl: intlShape.isRequired,
  wallet: Wallet,
};

const WalletSummaryAssets = observer((props: Props) => {
  return <WalletTokensList title="Recently used tokens" {...props} />;
});

export default injectIntl(WalletSummaryAssets);
