// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Wallet from '../../../domains/Wallet';
import WalletRow from './WalletRow';
import styles from './DelegationCenterBody.scss';
import { DelegationActions } from '../../../domains/StakePool';
import type { DelegationAction } from '../../../api/staking/types';
import LoadingSpinner from '../../widgets/LoadingSpinner';

const messages = defineMessages({
  bodyTitle: {
    id: 'staking.delegationCenter.bodyTitle',
    defaultMessage: '!!!Wallets',
    description: 'Title for the Delegation center body section.',
  },
  loadingStakePoolsMessage: {
    id: 'staking.delegationCenter.loadingStakePoolsMessage',
    defaultMessage: '!!!Loading stake pools',
    description:
      'Loading stake pool message for the Delegation center body section.',
  },
});

type Props = {
  wallets: Array<Wallet>,
  numberOfStakePools: number,
  onDelegate: Function,
  onUndelegate: Function,
  getStakePoolById: Function,
  isLoading: boolean,
};

@observer
export default class DelegationCenterBody extends Component<Props> {
  loadingSpinner: ?LoadingSpinner;

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  handleMenuItemClick = (item: DelegationAction, walletId: string) => {
    const { onDelegate, onUndelegate } = this.props;

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
      isLoading,
    } = this.props;

    const title = intl.formatMessage(messages.bodyTitle);

    const loadingSpinner = (
      <LoadingSpinner
        big
        ref={component => {
          this.loadingSpinner = component;
        }}
      />
    );

    const componentClasses = classnames([
      styles.component,
      isLoading ? styles.isLoading : null,
    ]);

    return (
      <div className={componentClasses}>
        {isLoading ? (
          <div className={styles.loadinBlockWrapper}>
            <p>{intl.formatMessage(messages.loadingStakePoolsMessage)}</p>
            {loadingSpinner}
          </div>
        ) : (
          <div>
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
                  delegatedStakePool={getStakePoolById(
                    wallet.delegatedStakePoolId
                  )}
                  nextDelegatedStakePool={getStakePoolById(
                    wallet.nextDelegationStakePoolId
                  )}
                  nextDelegatedStakePoolEpoch={
                    wallet.nextDelegationStakePoolEpoch
                  }
                  lastDelegatedStakePool={getStakePoolById(
                    wallet.lastDelegationStakePoolId
                  )}
                  lastDelegatedStakePoolEpoch={
                    wallet.lastDelegationStakePoolEpoch
                  }
                />
              ))}
            </div>
          </div>
        )}
      </div>
    );
  }
}
