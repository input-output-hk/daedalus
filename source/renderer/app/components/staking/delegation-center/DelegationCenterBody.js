// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import Wallet from '../../../domains/Wallet';
import WalletRow from './WalletRow';
import styles from './DelegationCenterBody.scss';
import { DelegationActions } from '../../../api/staking/types';
import type { DelegationAction } from '../../../api/staking/types';

const messages = defineMessages({
  bodyTitle: {
    id: 'staking.delegationCenter.bodyTitle',
    defaultMessage: '!!!Wallets',
    description: 'Title for the Delegation center body section.',
  },
});

type Props = {
  wallets: Array<Wallet>,
  onDelegate: Function,
  numberOfStakePools: number,
};

@observer
export default class DelegationCenterBody extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  handleMenuItemClick = (item: DelegationAction, walletId: string) => {
    const { onDelegate } = this.props;

    switch (item) {
      case DelegationActions.CHANGE_DELEGATION:
        onDelegate(walletId);
        break;
      case DelegationActions.REMOVE_DELEGATION:
        // @TODO - Add once [DDW-1096] is done
        break;
      default:
        break;
    }
  };

  render() {
    const { intl } = this.context;
    const { wallets, onDelegate, numberOfStakePools } = this.props;

    const title = intl.formatMessage(messages.bodyTitle);

    return (
      <div className={styles.component}>
        <div className={styles.bodyTitle}>
          <span>{title}</span>
        </div>
        <div className={styles.mainContent}>
          {wallets.map(wallet => (
            <WalletRow
              key={wallet.id}
              wallet={wallet}
              onDelegate={onDelegate}
              numberOfStakePools={numberOfStakePools}
              onMenuItemClick={this.handleMenuItemClick}
            />
          ))}
        </div>
      </div>
    );
  }
}
