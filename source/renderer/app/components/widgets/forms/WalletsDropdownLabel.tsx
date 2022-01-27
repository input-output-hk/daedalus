import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { getColorFromRange } from '../../../utils/colors';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletsDropdownLabel.scss' o... Remove this comment to see the full error message
import styles from './WalletsDropdownLabel.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/hardwar... Remove this comment to see the full error message
import hardwareWalletsIcon from '../../../assets/images/hardware-wallet/connect-ic.inline.svg';
import Wallet from '../../../domains/Wallet';

export type WalletOption = {
  wallet: Partial<Wallet>;
  getStakePoolById: (...args: Array<any>) => any;
  numberOfStakePools?: number;
};
export default class WalletsDropdownLabel extends Component<WalletOption> {
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
      <div
        style={{
          color,
        }}
        className={styles.ticker}
      >
        [{ticker}]
      </div>
    );
  };

  render() {
    const { wallet } = this.props;
    const { name, isHardwareWallet } = wallet;
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
        </div>
      </div>
    );
  }
}
