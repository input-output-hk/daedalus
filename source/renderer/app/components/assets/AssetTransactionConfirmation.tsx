import React from 'react';
import classnames from 'classnames';
import BigNumber from 'bignumber.js';
import {
  defineMessages,
  intlShape,
  injectIntl,
  FormattedHTMLMessage,
} from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/question-m... Remove this comment to see the full error message
import questionMarkIcon from '../../assets/images/question-mark.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './AssetTransactionConfirmation... Remove this comment to see the full error message
import styles from './AssetTransactionConfirmation.scss';
import type { AssetToken } from '../../api/assets/types';
import Asset from './Asset';
import { formattedTokenWalletAmount } from '../../utils/formatters';

const messages = defineMessages({
  assetLabel: {
    id: 'asset.transactionConfirmation.assetLabel',
    defaultMessage: '!!!Token #{assetNumber}',
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
  missingToken: {
    id: 'asset.transactionConfirmation.missingToken',
    defaultMessage: '!!!There is no such token in this wallet',
    description: '"missingToken" item on AssetTransactionConfirmation.',
  },
  insufficientBalance: {
    id: 'asset.transactionConfirmation.insufficientBalance',
    defaultMessage:
      '!!!Insufficient funds. The balance of the token in this wallet is  {formattedBalance} (Unformatted: {unformattedBalance})',
    description: '"insufficientBalance" item on AssetTransactionConfirmation.',
  },
});
type Props = {
  asset: AssetToken;
  assetNumber: number;
  intl: intlShape.isRequired;
  isHardwareWallet: boolean;
  tokenIsMissing?: boolean;
  insufficientBalance?: boolean;
  amount: BigNumber;
};

const onCopyAssetParam = () => {};

const AssetTransactionConfirmation = observer((props: Props) => {
  const {
    assetNumber,
    asset,
    intl,
    isHardwareWallet,
    tokenIsMissing,
    insufficientBalance,
    amount,
  } = props;
  const hasError = tokenIsMissing || insufficientBalance;
  const { metadata, decimals } = asset;
  const formattedAmount = formattedTokenWalletAmount(
    amount,
    metadata,
    decimals
  );
  const unformattedAmount = formattedTokenWalletAmount(amount, null, 0);
  const formattedBalance = formattedTokenWalletAmount(
    asset.quantity,
    metadata,
    decimals
  );
  const unformattedBalance = formattedTokenWalletAmount(
    asset.quantity,
    null,
    0
  );
  const componentStyles = classnames(styles.component, {
    [styles.error]: hasError,
  });
  const content = (
    <>
      <div className={styles.assetsContainer}>
        <h3>
          <span className={styles.assetLabel}>
            {intl.formatMessage(messages.assetLabel, {
              assetNumber,
            })}{' '}
          </span>
          <Asset
            asset={asset}
            onCopyAssetParam={onCopyAssetParam}
            hasError={hasError}
          />
        </h3>
        <div className={styles.amountFeesWrapper}>
          <div className={styles.amount}>{formattedAmount}</div>
        </div>
      </div>
      <div className={styles.assetsContainer}>
        <div className={styles.unformattedAmountLine} />
        <div className={styles.unformattedAmountLabel}>
          {intl.formatMessage(messages.unformattedAmountLabel)}
          <PopOver
            content={
              <FormattedHTMLMessage
                {...messages[
                  isHardwareWallet
                    ? 'unformattedAmountMessageForHardwareWallets'
                    : 'unformattedAmountMessageForSoftwareWallets'
                ]}
                tagName="div"
              />
            }
          >
            <div className={styles.questionMark}>
              <SVGInline svg={questionMarkIcon} />
            </div>
          </PopOver>
          {':'}
        </div>
        <div className={styles.unformattedAmount}>{unformattedAmount}</div>
      </div>
    </>
  );

  if (tokenIsMissing) {
    return (
      <div className={componentStyles}>
        <PopOver
          content={intl.formatMessage(messages.missingToken)}
          appendTo="parent"
          placement="bottom"
        >
          {content}
        </PopOver>
      </div>
    );
  }

  if (insufficientBalance) {
    return (
      <div className={componentStyles}>
        <PopOver
          content={intl.formatMessage(messages.insufficientBalance, {
            formattedBalance,
            unformattedBalance,
          })}
          appendTo="parent"
          placement="bottom"
        >
          {content}
        </PopOver>
      </div>
    );
  }

  return <div className={componentStyles}>{content}</div>;
});
export default injectIntl(AssetTransactionConfirmation);
