// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './Step3FailureDialog.scss';
import redeemDialogOverride from './RedeemDialogOverride.scss';
import sadWalletImage from '../../../assets/images/sad-wallet.inline.svg';

const messages = defineMessages({
  description: {
    id: 'staking.redeemItnRewards.step3.failure.description',
    defaultMessage:
      '!!!No rewards were found in your Incentivized Testnet Rewards wallet. Please make sure that you have entered the correct wallet recovery phrase.',
    description: 'description for Redeem Incentivized Testnet - Step 3',
  },
  backButtonLabel: {
    id: 'staking.redeemItnRewards.step3.failure.backButtonLabel',
    defaultMessage: '!!!Back',
    description: 'backButtonLabel for Redeem Incentivized Testnet - Step 3',
  },
  closeWindowLinkLabel: {
    id: 'staking.redeemItnRewards.step3.failure.closeWindowLinkLabel',
    defaultMessage: '!!!Close window',
    description:
      'closeWindowLinkLabel for Redeem Incentivized Testnet - Step 3',
  },
});

type Props = {
  onClose: Function,
  onBack: Function,
};

@observer
export default class Step3FailureDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onClose, onBack } = this.props;

    const actions = {
      direction: 'column',
      items: [
        {
          primary: true,
          label: intl.formatMessage(messages.backButtonLabel),
          onClick: onBack,
        },
        {
          onClick: onClose,
          label: intl.formatMessage(messages.closeWindowLinkLabel),
          isLink: true,
          hasIconAfter: false,
        },
      ],
    };

    return (
      <>
        <DialogCloseButton
          className={redeemDialogOverride.closeButton}
          onClose={onClose}
        />
        <Dialog
          actions={actions}
          onClose={onClose}
          customThemeOverrides={redeemDialogOverride}
          closeOnOverlayClick={false}
        >
          <SVGInline svg={sadWalletImage} className={styles.sadWalletImage} />
          <div className={styles.description}>
            {intl.formatMessage(messages.description)}
          </div>
        </Dialog>
      </>
    );
  }
}
