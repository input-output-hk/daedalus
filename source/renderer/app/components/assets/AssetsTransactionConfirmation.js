// @flow
import React from 'react';
import classnames from 'classnames';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import BigNumber from 'bignumber.js';
import { observer } from 'mobx-react';
import styles from './AssetsTransactionConfirmation.scss';
import AssetTransactionConfirmation from './AssetTransactionConfirmation';
import type { AssetToken } from '../../api/assets/types';

const messages = defineMessages({
  title: {
    id: 'ID.TITLE',
    defaultMessage: 'CONTENT',
    description: '"" item on.',
  },
});

type Props = {
  intl: intlShape.isRequired,
  assets: Array<AssetToken>,
  feesAmount?: BigNumber,
  feesUnit?: string,
};

const AssetsTransactionConfirmation = observer((props: Props) => {
  const { assets, intl } = props;
  const componentStyles = classnames([styles.component]);
  return (
    <div className={componentStyles}>
      {intl.formatMessage(messages.title)}
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
