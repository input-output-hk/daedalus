import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import { get } from 'lodash';
import Wallet from '../../../domains/Wallet';
import WalletRow from './WalletRow';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DelegationCenterBody.scss' o... Remove this comment to see the full error message
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
  wallets: Array<Wallet>;
  numberOfStakePools: number;
  numberOfRankedStakePools: number;
  onDelegate: (...args: Array<any>) => any;
  onUndelegate: (...args: Array<any>) => any;
  getStakePoolById: (...args: Array<any>) => any;
  isLoading: boolean;
  nextEpoch: NextEpoch | null | undefined;
  futureEpoch: FutureEpoch | null | undefined;
  isListActive?: boolean;
  currentTheme: string;
  onOpenExternalLink: (...args: Array<any>) => any;
  containerClassName: string;
  setListActive?: (...args: Array<any>) => any;
  listName?: string;
};

@observer
class DelegationCenterBody extends Component<Props> {
  loadingSpinner: LoadingSpinner | null | undefined;
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
                  // @ts-ignore ts-migrate(2322) FIXME: Type '{ key: string; wallet: Wallet; numberOfStake... Remove this comment to see the full error message
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

export default DelegationCenterBody;
