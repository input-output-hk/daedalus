import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './NoWalletsDialog.scss';
import sadWalletImage from '../../../assets/images/sad-wallet.inline.svg';
import closeCrossThin from '../../../assets/images/close-cross-thin.inline.svg';
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
  onClose: (...args: Array<any>) => any;
  onAddWallet: (...args: Array<any>) => any;
};

@observer
class NoWalletsDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onClose, onAddWallet } = this.props;
    const closeButton = (
      <DialogCloseButton
        icon={closeCrossThin}
        className={styles.closeButton}
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
        closeOnOverlayClick={false}
        fullSize
      >
        <div className={styles.component}>
          <SVGInline svg={sadWalletImage} className={styles.sadWalletImage} />
          <div className={styles.description}>
            {intl.formatMessage(messages.description)}
          </div>
        </div>
      </Dialog>
    );
  }
}

export default NoWalletsDialog;
