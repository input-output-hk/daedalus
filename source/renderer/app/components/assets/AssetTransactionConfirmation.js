// @flow
import React from 'react';
import classnames from 'classnames';
import {
  defineMessages,
  intlShape,
  injectIntl,
  FormattedHTMLMessage,
} from 'react-intl';
import BigNumber from 'bignumber.js';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import questionMarkIcon from '../../assets/images/question-mark.inline.svg';
import styles from './AssetTransactionConfirmation.scss';
import type { AssetToken } from '../../api/assets/types';
import Asset from './Asset';

const messages = defineMessages({
  title: {
    id: 'asset.transactionConfirmation.title',
    defaultMessage: 'Token {index}',
    description: '"title" item on AssetTransactionConfirmation.',
  },
  unformattedAmountMessageForHardwareWallets: {
    id:
      'asset.transactionConfirmation.unformattedAmountMessageForHardwareWallets',
    defaultMessage: '!!!unformattedAmountMessageForHardwareWallets',
    description:
      '"unformattedAmountMessageForHardwareWallets" item on AssetTransactionConfirmation.',
  },
  unformattedAmountMessageForSoftwareWallets: {
    id:
      'asset.transactionConfirmation.unformattedAmountMessageForSoftwareWallets',
    defaultMessage: '!!!unformattedAmountMessageForSoftwareWallets',
    description:
      '"unformattedAmountMessageForSoftwareWallets" item on AssetTransactionConfirmation.',
  },
});

type Props = {
  amount: string,
  asset: AssetToken,
  index: number,
  intl: intlShape.isRequired,
  isHardwareWallet: boolean,
};

const onCopyAssetItem = () => {};

const AssetTransactionConfirmation = observer((props: Props) => {
  const { index, asset, intl, amount, isHardwareWallet } = props;
  const componentStyles = classnames([styles.component]);
  return (
    <div className={componentStyles}>
      <div className={styles.assetsContainer}>
        <h3>
          <span>
            {/* intl.formatMessage(messages.assetLabel) */}
            assetLabel &nbsp;#{index + 1}
          </span>
          <Asset
            asset={asset}
            onCopyAssetItem={onCopyAssetItem}
            className={styles.assetToken}
          />
        </h3>
        <div className={styles.amountFeesWrapper}>
          <div className={styles.amount}>{amount}</div>
        </div>
      </div>
      <div className={styles.assetsContainer}>
        <div className={styles.unformattedAmountLine} />
        <div className={styles.unformattedAmountLabel}>
          {/* intl.formatMessage(messages.unformattedAmountLabel) */}
          <PopOver
            content={
              <div className="UnformattedAmountTooltip">
                <FormattedHTMLMessage
                  {...messages[
                    isHardwareWallet
                      ? 'unformattedAmountMessageForHardwareWallets'
                      : 'unformattedAmountMessageForSoftwareWallets'
                  ]}
                  tagName="div"
                />
              </div>
            }
            key="tooltip"
          >
            <div className={styles.questionMark}>
              <SVGInline svg={questionMarkIcon} />
            </div>
          </PopOver>
          {':'}
        </div>
        <div className={styles.unformattedAmount}>{amount}</div>
      </div>
    </div>
  );
});

export default injectIntl(AssetTransactionConfirmation);
