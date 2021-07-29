// @flow
import React from 'react';
import classnames from 'classnames';
import {
  defineMessages,
  intlShape,
  injectIntl,
  FormattedHTMLMessage,
} from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import questionMarkIcon from '../../assets/images/question-mark.inline.svg';
import styles from './AssetTransactionConfirmation.scss';
import type { AssetToken } from '../../api/assets/types';
import Asset from './Asset';
import { formattedTokenWalletAmount } from '../../utils/formatters';

const messages = defineMessages({
  assetLabel: {
    id: 'asset.transactionConfirmation.assetLabel',
    defaultMessage: '!!!Token #{index}',
    description: '"assetLabel" item on AssetTransactionConfirmation.',
  },
  unformattedAmountLabel: {
    id: 'asset.transactionConfirmation.unformattedAmountLabel',
    defaultMessage: '!!!unformatted amount',
    description:
      '"unformattedAmountLabel" item on AssetTransactionConfirmation.',
  },
  unformattedAmountMessageForHardwareWallets: {
    id:
      'asset.transactionConfirmation.unformattedAmountMessageForHardwareWallets',
    defaultMessage:
      '!!!Native assets may specify a number of decimal places, as defined in the Cardano token registry. Daedalus uses this information to format the amount that is being sent in the transaction.<br /><br />The native token unformatted amount is the amount without these decimal places. Please ensure that you verify both amounts, as some wallet software may not yet use the Cardano token registry.',
    description:
      '"unformattedAmountMessageForHardwareWallets" item on AssetTransactionConfirmation.',
  },
  unformattedAmountMessageForSoftwareWallets: {
    id:
      'asset.transactionConfirmation.unformattedAmountMessageForSoftwareWallets',
    defaultMessage:
      '!!!Native assets may specify a number of decimal places, as defined in the Cardano token registry. Daedalus uses this information to format the amount that is being sent in the transaction.<br /><br />The native token unformatted amount is the amount without these decimal places. Please ensure that you verify both amounts, as some wallet software may not yet use the Cardano token registry.<br /><br />The native token unformatted amount will be displayed on the hardware wallet device during transaction confirmation.',
    description:
      '"unformattedAmountMessageForSoftwareWallets" item on AssetTransactionConfirmation.',
  },
});

type Props = {
  asset: AssetToken,
  index: number,
  intl: intlShape.isRequired,
  isHardwareWallet: boolean,
  tokenIsMissing?: boolean,
};

const onCopyAssetItem = () => {};

const AssetTransactionConfirmation = observer((props: Props) => {
  const { index, asset, intl, isHardwareWallet, tokenIsMissing } = props;
  const { quantity, metadata, decimals } = asset;
  const amount = formattedTokenWalletAmount(quantity, metadata, decimals);
  const componentStyles = classnames([styles.component]);
  return (
    <div className={componentStyles}>
      <div className={styles.assetsContainer}>
        <h3>
          <span>
            {intl.formatMessage(messages.assetLabel, { index })}{' '}
            {tokenIsMissing ? 'MISSING' : ''}
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
          {intl.formatMessage(messages.unformattedAmountLabel)}
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
