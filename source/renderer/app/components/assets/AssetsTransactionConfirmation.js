// @flow
import React from 'react';
import classnames from 'classnames';
import { intlShape, injectIntl } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
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
  adaError?: string,
};

const AssetsTransactionConfirmation = observer((props: Props) => {
  const {
    adaAmount,
    assets,
    assetsAmounts,
    className,
    intl,
    wallet,
    adaError,
  } = props;
  const insufficientAdaAmount = wallet?.amount.isLessThan(adaAmount);
  const componentStyles = classnames([styles.component, className]);
  const adaAmountStyles = classnames([
    styles.adaAmount,
    insufficientAdaAmount ? styles.adaAmountError : null,
  ]);

  return (
    <div className={componentStyles}>
      <div className={adaAmountStyles}>
        <p>{intl.formatMessage(globalMessages.adaName)}</p>
        <div className={styles.amount}>
          {adaError ? (
            <PopOver
              content={adaError}
              className={styles.adaErrorPopOver}
              appendTo="parent"
              visible
              placement="left-start"
              offset={[-8, 23]}
            >
              {formattedWalletAmount(adaAmount)}
            </PopOver>
          ) : (
            formattedWalletAmount(adaAmount)
          )}
        </div>
      </div>
      {assets.map((asset, index) => (
        <AssetTransactionConfirmation
          key={asset.uniqueId}
          assetNumber={index + 1}
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
