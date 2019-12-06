// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import Wallet from '../../../domains/Wallet';
import WalletRow from './WalletRow';
import styles from './DelegationCenterBody.scss';
import { DelegationActions } from '../../../domains/StakePool';
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
  numberOfStakePools: number,
  onDelegate: Function,
  onUndelegate: Function,
  getStakePoolById: Function,
};

@observer
export default class DelegationCenterBody extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  handleMenuItemClick = (item: DelegationAction, walletId: string) => {
    const { onDelegate, onUndelegate } = this.props;

    console.debug('TEST: ', item, walletId);
    switch (item) {
      case DelegationActions.CHANGE_DELEGATION:
        onDelegate(walletId);
        break;
      case DelegationActions.REMOVE_DELEGATION:
        onUndelegate(walletId);
        break;
      default:
        break;
    }
  };

  render() {
    const { intl } = this.context;
    const {
      wallets,
      numberOfStakePools,
      onDelegate,
      getStakePoolById,
    } = this.props;

    const title = intl.formatMessage(messages.bodyTitle);

    return (
      <div className={styles.component}>
        <div className={styles.bodyTitle}>
          <span>{title}</span>
        </div>
        <div className={styles.mainContent}>
          {wallets.map((wallet: Wallet) => (
            <WalletRow
              key={wallet.id}
              wallet={wallet}
              numberOfStakePools={numberOfStakePools}
              onDelegate={onDelegate}
              onMenuItemClick={this.handleMenuItemClick}
              delegatedStakePool={getStakePoolById(wallet.delegatedStakePoolId)}
            />
          ))}
        </div>
      </div>
    );
  }
}
