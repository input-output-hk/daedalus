import React from 'react';
import classnames from 'classnames';
import { intlShape, injectIntl } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import BigNumber from 'bignumber.js';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './AssetsTransactionConfirmatio... Remove this comment to see the full error message
import styles from './AssetsTransactionConfirmation.scss';
import AssetTransactionConfirmation from './AssetTransactionConfirmation';
import Wallet from '../../domains/Wallet';
import globalMessages from '../../i18n/global-messages';
import { formattedWalletAmount } from '../../utils/formatters';
import type { AssetToken } from '../../api/assets/types';
import { isTokenMissingInWallet, tokenHasBalance } from '../../utils/assets';

type Props = {
  assets: Array<AssetToken>;
  assetsAmounts: Array<BigNumber>;
  className?: string;
  adaAmount?: BigNumber;
  intl: intlShape.isRequired;
  wallet?: Wallet | null | undefined;
  getAssetByUniqueId: (...args: Array<any>) => any;
  adaError?: string;
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
  const adaAmountContent = (
    <div className={adaAmountStyles}>
      <p>{intl.formatMessage(globalMessages.adaName)}</p>
      <div className={styles.amount}>{formattedWalletAmount(adaAmount)}</div>
    </div>
  );
  return (
    <div className={componentStyles}>
      {adaError ? (
        <PopOver
          content={adaError}
          className={styles.adaErrorPopOver}
          appendTo="parent"
        >
          {adaAmountContent}
        </PopOver>
      ) : (
        adaAmountContent
      )}
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
