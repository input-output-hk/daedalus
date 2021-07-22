// @flow
import React from 'react';
import classnames from 'classnames';
import { intlShape, injectIntl } from 'react-intl';
import BigNumber from 'bignumber.js';
import { observer } from 'mobx-react';
import styles from './AssetsTransactionConfirmation.scss';
import AssetTransactionConfirmation from './AssetTransactionConfirmation';
import type { AssetToken } from '../../api/assets/types';
import globalMessages from '../../i18n/global-messages';
import { DECIMAL_PLACES_IN_ADA } from '../../config/numbersConfig';
import AssetAmount from './AssetAmount';

type Props = {
  assets: Array<AssetToken>,
  feesAmount?: BigNumber,
  feesUnit?: string,
  intl: intlShape.isRequired,
};

const AssetsTransactionConfirmation = observer((props: Props) => {
  const {
    assets,
    feesAmount,
    intl,
    feesUnit = intl.formatMessage(globalMessages.unitAda),
  } = props;
  const componentStyles = classnames([styles.component]);
  return (
    <div className={componentStyles}>
      <div className={styles.fees}>
        <p>{feesUnit}</p>
        <AssetAmount amount={feesAmount} decimals={DECIMAL_PLACES_IN_ADA} />
      </div>
      {assets.map((asset, index) => (
        <AssetTransactionConfirmation
          key={asset.uniqueId}
          amount="1.000000"
          index={index}
          isHardwareWallet={false}
          asset={asset}
        />
      ))}
    </div>
  );
});

export default injectIntl(AssetsTransactionConfirmation);
