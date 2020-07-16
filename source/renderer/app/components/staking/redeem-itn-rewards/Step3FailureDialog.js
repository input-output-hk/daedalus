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
  description1NoRewards: {
    id: 'staking.redeemItnRewards.step3.failure.description1NoRewards',
    defaultMessage:
      '!!!No rewards were found in your Incentivized Testnet Rewards wallet. Please make sure that you have entered the correct wallet recovery phrase.',
    description: 'description for Redeem Incentivized Testnet - Step 3',
  },
  description2InvalidWallet: {
    id: 'staking.redeemItnRewards.step3.failure.description2InvalidWallet',
    defaultMessage:
      '!!!Rewards from the wallet corresponding to the recovery phrase you have provided have already been redeemed.',
    description: 'description for Redeem Incentivized Testnet - Step 3',
  },
  description3Generic: {
    id: 'staking.redeemItnRewards.step3.failure.description3Generic',
    defaultMessage:
      '!!!No rewards were found in your Incentivized Testnet Rewards wallet. Please make sure that you have entered the correct wallet recovery phrase and that rewards have not already been redeemed.',
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
  // @REDEEM TODO: Remove when the API endpoint is implemented
  stakingFailure: number,
};

@observer
export default class Step3FailureDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onClose, onBack, stakingFailure } = this.props;

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

    // #REDEEM TODO: remove when the API is ready
    let description = messages.description1NoRewards;
    if (stakingFailure === 2) description = messages.description2InvalidWallet;
    if (stakingFailure === 3) description = messages.description3Generic;

    const closeButton = (
      <DialogCloseButton
        className={redeemDialogOverride.closeButton}
        onClose={onClose}
      />
    );

    return (
      <Dialog
        actions={actions}
        onClose={onClose}
        closeButton={closeButton}
        customThemeOverrides={redeemDialogOverride}
        closeOnOverlayClick={false}
      >
        <SVGInline svg={sadWalletImage} className={styles.sadWalletImage} />
        <div className={styles.description}>
          {intl.formatMessage(description)}
        </div>
      </Dialog>
    );
  }
}
