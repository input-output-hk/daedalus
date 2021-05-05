// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { intlShape, defineMessages } from 'react-intl';
import { getColorFromRange } from '../../../utils/colors';
import styles from './WalletsDropdownLabel.scss';
import hardwareWalletsIcon from '../../../assets/images/hardware-wallet/connect-ic.inline.svg';
import Wallet from '../../../domains/Wallet';

const messages = defineMessages({
  syncingLabel: {
    id: 'widgets.itemsDropdown.option.syncingLabel',
    defaultMessage: '!!!syncing',
    description: 'syncingLabel for ItemDropdownOption',
  },
});

export type WalletOption = {
  wallet: $Shape<Wallet>,
  getStakePoolById: Function,
  numberOfStakePools?: number,
  syncingLabel?: string,
};

export default class WalletsDropdownLabel extends Component<WalletOption> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  renderTicker = () => {
    const { wallet, getStakePoolById, numberOfStakePools } = this.props;
    const {
      delegatedStakePoolId,
      lastDelegatedStakePoolId,
      pendingDelegations,
    } = wallet;
    const hasPendingDelegations =
      pendingDelegations && pendingDelegations.length > 0;
    let currentStakePoolId = delegatedStakePoolId;
    if (hasPendingDelegations) {
      currentStakePoolId = lastDelegatedStakePoolId;
    }
    const delegatedStakePool = currentStakePoolId
      ? getStakePoolById(currentStakePoolId)
      : null;
    if (!numberOfStakePools || !delegatedStakePool) {
      return null;
    }
    const { ranking, ticker } = delegatedStakePool;
    const color = getColorFromRange(ranking, numberOfStakePools);
    return (
      <div style={{ color }} className={styles.ticker}>
        [{ticker}]
      </div>
    );
  };

  render() {
    const { intl } = this.context;
    const defaultSyncingLabel = intl.formatMessage(messages.syncingLabel);
    const { wallet, syncingLabel = defaultSyncingLabel } = this.props;
    const { name, isHardwareWallet, isSyncing } = wallet;
    const ticker = this.renderTicker();
    return (
      <div className={styles.component}>
        {ticker}
        <div className={styles.walletName}>
          {name}
          {isHardwareWallet && (
            <SVGInline
              svg={hardwareWalletsIcon}
              className={styles.hardwareWalletsIcon}
            />
          )}
          {isSyncing && (
            <span className={styles.labelSync}>{syncingLabel}</span>
          )}
        </div>
      </div>
    );
  }
}
