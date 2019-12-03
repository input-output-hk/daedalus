// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import Wallet from '../../../domains/Wallet';
import WalletRow from './WalletRow';
import styles from './DelegationCenterBody.scss';

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
};

@observer
export default class DelegationCenterBody extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      wallets,
      numberOfStakePools,
      onDelegate,
      onUndelegate,
    } = this.props;

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
              numberOfStakePools={numberOfStakePools}
              onDelegate={onDelegate}
              onUndelegate={onUndelegate}
            />
          ))}
        </div>
      </div>
    );
  }
}
