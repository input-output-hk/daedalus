// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Wallet from '../../../domains/Wallet';
import WalletRow from './WalletRow';
import styles from './DelegationCenterBody.scss';
import LoadingSpinner from '../../widgets/LoadingSpinner';

const messages = defineMessages({
  bodyTitle: {
    id: 'staking.delegationCenter.bodyTitle',
    defaultMessage: '!!!Wallets',
    description: 'Title for the Delegation center body section.',
  },
  loadingStakePoolsMessage: {
    id: 'staking.delegationCenter.loadingStakePoolsMessage',
    defaultMessage: '!!!Loading wallets',
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

  render() {
    const { intl } = this.context;
    const {
      wallets,
      numberOfStakePools,
      onDelegate,
      onUndelegate,
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
                  onDelegate={() => onDelegate(wallet.id)}
                  onUndelegate={() => onUndelegate(wallet.id)}
                  delegatedStakePool={getStakePoolById(
                    wallet.delegatedStakePoolId
                  )}
                  getStakePoolById={getStakePoolById}
                />
              ))}
            </div>
          </div>
        )}
      </div>
    );
  }
}
