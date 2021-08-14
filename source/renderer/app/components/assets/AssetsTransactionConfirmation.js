// @flow
import React from 'react';
import classnames from 'classnames';
import { intlShape, injectIntl } from 'react-intl';
import BigNumber from 'bignumber.js';
import { observer } from 'mobx-react';
import styles from './AssetsTransactionConfirmation.scss';
import AssetTransactionConfirmation from './AssetTransactionConfirmation';
import Wallet from '../../domains/Wallet';
import globalMessages from '../../i18n/global-messages';
import { formattedWalletAmount } from '../../utils/formatters';
import type { AssetToken } from '../../api/assets/types';
import { isTokenMissingInWallet, tokenHasBalance } from '../../utils/assets';

type Props = {
  assets: Array<AssetToken>,
  assetsAmounts: Array<BigNumber>,
  className?: string,
  adaAmount?: BigNumber,
  intl: intlShape.isRequired,
  wallet?: ?Wallet,
  getAssetByUniqueId: Function,
};

const AssetsTransactionConfirmation = observer((props: Props) => {
  const { adaAmount, assets, assetsAmounts, className, intl, wallet } = props;
  const componentStyles = classnames([styles.component, className]);
  return (
    <div className={componentStyles}>
      <div className={styles.adaAmount}>
        <p>{intl.formatMessage(globalMessages.unitAda)}</p>
        <div className={styles.amount}>
          {formattedWalletAmount(adaAmount, false)}
        </div>
      </div>
      {assets.map((asset, index) => (
        <AssetTransactionConfirmation
          key={asset.uniqueId}
          index={index}
          isHardwareWallet={false}
          asset={asset}
          amount={assetsAmounts[index]}
          tokenIsMissing={isTokenMissingInWallet(wallet, asset)}
          insufficientBalance={
            !!wallet && !tokenHasBalance(asset, assetsAmounts[index])
          }
        />
      ))}
    </div>
  );
});

export default injectIntl(AssetsTransactionConfirmation);
