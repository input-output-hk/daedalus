// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { rangeMap } from '../../../utils/rangeMap';
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
  onDelegate: Function,
};

@observer
export default class DelegationCenterBody extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  getIndex = (ranking: number) => {
    const { wallets } = this.props;

    return rangeMap(
      ranking,
      1,
      wallets.filter(wallet => wallet.isDelegated).length,
      0,
      99
    );
  };

  render() {
    const { intl } = this.context;
    const { wallets, onDelegate } = this.props;

    const title = intl.formatMessage(messages.bodyTitle);

    return (
      <div className={styles.component}>
        <div className={styles.bodyTitle}>
          <span>{title}</span>
        </div>
        <div className={styles.mainContent}>
          {wallets.map(wallet => {
            if (wallet.isDelegated && wallet.delegatedStakePool) {
              const index = this.getIndex(wallet.delegatedStakePool.ranking);
              return (
                <WalletRow
                  key={wallet.id}
                  wallet={wallet}
                  index={index}
                  onDelegate={onDelegate}
                />
              );
            }
            return (
              <WalletRow
                key={wallet.id}
                wallet={wallet}
                onDelegate={onDelegate}
              />
            );
          })}
        </div>
      </div>
    );
  }
}
