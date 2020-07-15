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
    id: 'staking.redeemItnRewards.noWallets.description',
    defaultMessage:
      '!!!Redemption of Incentivized Testnet rewards is not available as you currently do not have any Shelley-compatible wallets.',
    description: 'description for Redeem Incentivized Testnet - Step 3',
  },
  addWalletButtonLabel: {
    id: 'staking.redeemItnRewards.noWallets.addWalletButtonLabel',
    defaultMessage: '!!!Add wallet',
    description:
      'addWalletButtonLabel for Redeem Incentivized Testnet - Step 3',
  },
});

type Props = {
  onClose: Function,
  onAddWallet: Function,
};

@observer
export default class NoWalletsDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onClose, onAddWallet } = this.props;

    const closeButton = (
      <DialogCloseButton
        className={redeemDialogOverride.closeButton}
        onClose={onClose}
      />
    );

    return (
      <Dialog
        actions={[
          {
            primary: true,
            label: intl.formatMessage(messages.addWalletButtonLabel),
            onClick: onAddWallet,
          },
        ]}
        closeButton={closeButton}
        onClose={onClose}
        customThemeOverrides={redeemDialogOverride}
        closeOnOverlayClick={false}
      >
        <SVGInline svg={sadWalletImage} className={styles.sadWalletImage} />
        <div className={styles.description}>
          {intl.formatMessage(messages.description)}
        </div>
      </Dialog>
    );
  }
}
