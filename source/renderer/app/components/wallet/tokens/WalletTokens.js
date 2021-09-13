// @flow
import React from 'react';
import classnames from 'classnames';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import { observer } from 'mobx-react';
import styles from './WalletTokens.scss';
import Wallet from '../../../domains/Wallet';
import type { AssetToken } from '../../../api/assets/types';

// const messages = defineMessages({
//   fingerprintItem: {
//     id: 'assets.assetToken.item.fingerprint',
//     defaultMessage: '!!!Fingerprint',
//     description: '"fingerprint" item.',
//   },
// });

type Props = {
  wallet: Wallet,
  assets: Array<AssetToken>,
  onOpenAssetSend: Function,
  onCopyAssetParam: Function,
  onAssetSettings: Function,
  currentLocale: string,
  isLoadingAssets: boolean,
  assetSettingsDialogWasOpened: boolean,
  intl: intlShape.isRequired,
};

const WalletTokens = observer((props: Props) => {
  console.log('props', props);
  const { assets } = props;

  return (
    <div className={styles.component}>
      <pre>{JSON.stringify(assets, null, 2)}</pre>
    </div>
  );
});

export default injectIntl(WalletTokens);
