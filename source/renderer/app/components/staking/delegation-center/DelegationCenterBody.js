// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import { get } from 'lodash';
import Wallet from '../../../domains/Wallet';
import WalletRow from './WalletRow';
import styles from './DelegationCenterBody.scss';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import type { FutureEpoch, NextEpoch } from '../../../api/network/types';

const messages = defineMessages({
  bodyTitle: {
    id: 'staking.delegationCenter.bodyTitle',
    defaultMessage: '!!!Wallets',
    description: 'Title for the Delegation center body section.',
  },
  currentEpochTitle: {
    id: 'staking.delegationCenter.currentEpochTitle',
    defaultMessage: '!!!Now',
    description: 'Title for the Delegation current epoch.',
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
  numberOfRankedStakePools: number,
  onDelegate: Function,
  onUndelegate: Function,
  getStakePoolById: Function,
  isLoading: boolean,
  nextEpoch: ?NextEpoch,
  futureEpoch: ?FutureEpoch,
  isListActive?: boolean,
  currentTheme: string,
  onOpenExternalLink: Function,
  containerClassName: string,
  setListActive?: Function,
  listName?: string,
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
      numberOfRankedStakePools,
      onDelegate,
      onUndelegate,
      getStakePoolById,
      isLoading,
      nextEpoch,
      futureEpoch,
      isListActive,
      currentTheme,
      onOpenExternalLink,
      containerClassName,
      setListActive,
      listName,
    } = this.props;

    const title = intl.formatMessage(messages.bodyTitle);

    const currentEpochTitle = intl.formatMessage(messages.currentEpochTitle);

    const nextEpochNumber = get(nextEpoch, 'epochNumber', 0);
    const futureEpochNumber = get(futureEpoch, 'epochNumber', 0);

    const loadingSpinner = (
      <LoadingSpinner
        big
        ref={(component) => {
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
              <div className={styles.leftBodyTitle}>{title}</div>
              {nextEpochNumber && futureEpochNumber && (
                <div className={styles.rightBodyTitle}>
                  <span>{currentEpochTitle}</span>
                  <span>{nextEpochNumber}</span>
                  <span>{futureEpochNumber}</span>
                </div>
              )}
            </div>
            <div className={styles.mainContent}>
              {wallets.map((wallet: Wallet) => (
                <WalletRow
                  key={wallet.id}
                  wallet={wallet}
                  numberOfStakePools={numberOfStakePools}
                  numberOfRankedStakePools={numberOfRankedStakePools}
                  onDelegate={() => onDelegate(wallet.id)}
                  onUndelegate={() => onUndelegate(wallet.id)}
                  delegatedStakePool={getStakePoolById(
                    wallet.delegatedStakePoolId
                  )}
                  getStakePoolById={getStakePoolById}
                  nextEpochNumber={nextEpochNumber}
                  futureEpochNumber={futureEpochNumber}
                  currentTheme={currentTheme}
                  isListActive={isListActive}
                  listName={listName}
                  onOpenExternalLink={onOpenExternalLink}
                  containerClassName={containerClassName}
                  setListActive={setListActive}
                />
              ))}
            </div>
          </div>
        )}
      </div>
    );
  }
}
