// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './ResyncWallet.scss';
import ResyncWalletButton from './ResyncWalletButton';

export const messages = defineMessages({
  resyncWalletHeader: {
    id: 'wallet.settings.resyncWallet.header',
    defaultMessage: '!!!Resync wallet with the blockchain',
    description: 'Resync wallet header on the wallet settings page.',
  },
  resyncWalletDescription: {
    id: 'wallet.settings.resyncWallet.description',
    defaultMessage:
      '!!!If you are experiencing issues with your wallet, or think you have an incorrect balance or transaction history, you can delete the local data stored by Daedalus and resync with the blockchain.',
    description: 'Resync wallet description.',
  },
});

type Props = {
  isForcedWalletResyncStarting: boolean,
  onResyncWallet: Function,
};

type State = {
  isFormBlocked: boolean,
};

@observer
export default class ResyncWallet extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { isForcedWalletResyncStarting, onResyncWallet } = this.props;
    return (
      <div className={styles.component}>
        <span>{intl.formatMessage(messages.resyncWalletHeader)}</span>
        <div className={styles.contentBox}>
          <p>{intl.formatMessage(messages.resyncWalletDescription)}</p>
          <ResyncWalletButton
            isSubmitting={isForcedWalletResyncStarting}
            onClick={onResyncWallet}
          />
        </div>
      </div>
    );
  }
}
