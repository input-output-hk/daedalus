// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import isNil from 'lodash/isNil';
import Wallet from '../../../domains/Wallet';
import StakePool, { DelegationActions } from '../../../domains/StakePool';
import { getColorFromRange } from '../../../utils/colors';
import settingsIcon from '../../../assets/images/settings-ic.inline.svg';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import DropdownMenu from './DropdownMenu';
import styles from './WalletRow.scss';

import type { DelegationAction } from '../../../api/staking/types';

const messages = defineMessages({
  walletAmount: {
    id: 'staking.delegationCenter.walletAmount',
    defaultMessage: '!!!{amount} ADA',
    description:
      'Amount of each wallet for the Delegation center body section.',
  },
  delegated: {
    id: 'staking.delegationCenter.delegated',
    defaultMessage: '!!!Delegated',
    description: 'Delegated label for the Delegation center body section.',
  },
  notDelegated: {
    id: 'staking.delegationCenter.notDelegated',
    defaultMessage: '!!!Not-delegated',
    description: 'Not-delegated label for the Delegation center body section.',
  },
  changeDelegation: {
    id: 'staking.delegationCenter.changeDelegation',
    defaultMessage: '!!!Change stake pool',
    description:
      'Change delegation label for the Delegation center body section.',
  },
  removeDelegation: {
    id: 'staking.delegationCenter.removeDelegation',
    defaultMessage: '!!!Undelegate',
    description:
      'Remove delegation label for the Delegation center body section.',
  },
  toStakePoolTickerPart1: {
    id: 'staking.delegationCenter.toStakePoolTickerPart1',
    defaultMessage: '!!!To',
    description:
      'Delegated stake pool ticker for the Delegation center body section.',
  },
  toStakePoolTickerPart2: {
    id: 'staking.delegationCenter.toStakePoolTickerPart2',
    defaultMessage: '!!!stake pool',
    description:
      'Delegated stake pool ticker for the Delegation center body section.',
  },
  delegate: {
    id: 'staking.delegationCenter.delegate',
    defaultMessage: '!!!Delegate',
    description: 'Delegate label for the Delegation center body section.',
  },
  yourStake: {
    id: 'staking.delegationCenter.yourStake',
    defaultMessage: '!!!your stake',
    description: 'Your stake label for the Delegation center body section.',
  },
});

type Props = {
  wallet: Wallet,
  delegatedStakePool?: StakePool,
  numberOfStakePools: number,
  onDelegate: Function,
  onMenuItemClick: Function,
};

@observer
export default class WalletRow extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  onDelegate = () => {
    const { wallet, onDelegate } = this.props;
    onDelegate(wallet.id);
  };

  onMenuItemClick = (clickeItem: DelegationAction) => {
    const { wallet } = this.props;
    this.props.onMenuItemClick(clickeItem, wallet.id);
  };

  render() {
    const { intl } = this.context;
    const {
      wallet: { name, amount, delegatedStakePoolId },
      delegatedStakePool,
      numberOfStakePools,
    } = this.props;

    const { ranking } = delegatedStakePool || {};

    const color =
      delegatedStakePoolId && !isNil(ranking)
        ? getColorFromRange(ranking, numberOfStakePools)
        : 'transparent';

    const delegated = intl.formatMessage(messages.delegated);
    const notDelegated = intl.formatMessage(messages.notDelegated);
    const changeDelegation = intl.formatMessage(messages.changeDelegation);
    const removeDelegation = intl.formatMessage(messages.removeDelegation);
    const delegate = intl.formatMessage(messages.delegate);
    const yourStake = intl.formatMessage(messages.yourStake);

    const delegatedWalletActionOptions = [
      {
        label: changeDelegation,
        value: DelegationActions.CHANGE_DELEGATION,
        className: styles.normalOption,
      },
      {
        label: removeDelegation,
        value: DelegationActions.REMOVE_DELEGATION,
        className: styles.removeOption,
      },
    ];

    return (
      <div className={styles.component}>
        <div className={styles.left}>
          <div className={styles.title}>{name}</div>
          <div className={styles.description}>
            <FormattedMessage
              {...messages.walletAmount}
              values={{
                amount: amount.toFormat(DECIMAL_PLACES_IN_ADA),
              }}
            />
          </div>
        </div>
        <div className={styles.right}>
          <div>
            <div className={styles.status}>
              <span>{delegatedStakePoolId ? delegated : notDelegated}</span>
              {delegatedStakePoolId && (
                <DropdownMenu
                  label={
                    <SVGInline svg={settingsIcon} className={styles.gearIcon} />
                  }
                  menuItems={delegatedWalletActionOptions}
                  onMenuItemClick={this.onMenuItemClick}
                />
              )}
            </div>
            <div className={styles.action}>
              {delegatedStakePoolId && delegatedStakePool ? (
                <Fragment>
                  {intl.formatMessage(messages.toStakePoolTickerPart1)}
                  <span style={{ color }}> [{delegatedStakePool.ticker}] </span>
                  {intl.formatMessage(messages.toStakePoolTickerPart2)}
                </Fragment>
              ) : (
                <span>
                  <span
                    className={styles.actionLink}
                    role="presentation"
                    onClick={this.onDelegate}
                  >
                    {delegate}
                  </span>
                  {` ${yourStake}`}
                </span>
              )}
            </div>
          </div>
          <div>
            <div
              className={styles.stakePoolRankingIndicator}
              style={{ background: color }}
            />
          </div>
        </div>
      </div>
    );
  }
}
