// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { isNil, get, map } from 'lodash';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import Wallet from '../../../domains/Wallet';
import StakePool, { DelegationActions } from '../../../domains/StakePool';
import { getColorFromRange } from '../../../utils/colors';
import settingsIcon from '../../../assets/images/settings-ic.inline.svg';
import sandClockIcon from '../../../assets/images/sand-clock.inline.svg';
import adaIcon from '../../../assets/images/ada-symbol.inline.svg';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import DropdownMenu from './DropdownMenu';
import styles from './WalletRow.scss';
import tooltipStyles from './WalletRowTooltip.scss';
import LoadingSpinner from '../../widgets/LoadingSpinner';

import type { DelegationAction } from '../../../api/staking/types';
import type { WalletNextDelegationEpoch } from '../../../api/wallets/types';

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
    defaultMessage: '!!!Undelegated',
    description: 'Undelegated label for the Delegation center body section.',
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
    defaultMessage: '!!!to',
    description:
      'Delegated stake pool ticker for the Delegation center body section.',
  },
  toStakePoolTickerPart2: {
    id: 'staking.delegationCenter.toStakePoolTickerPart2',
    defaultMessage: '!!!from epoch',
    description:
      'Delegated stake pool ticker for the Delegation center body section.',
  },
  toStakePoolTooltipTickerPart1: {
    id: 'staking.delegationCenter.toStakePoolTooltipTickerPart1',
    defaultMessage: '!!!currently',
    description:
      'Delegated stake pool tooltip ticker for the Delegation center body section.',
  },
  toStakePoolTooltipTickerPart2: {
    id: 'staking.delegationCenter.toStakePoolTooltipTickerPart2',
    defaultMessage: '!!!from epoch',
    description:
      'Delegated stake pool tooltip ticker for the Delegation center body section.',
  },
  delegate: {
    id: 'staking.delegationCenter.delegate',
    defaultMessage: '!!!Delegate',
    description: 'Delegate label for the Delegation center body section.',
  },
  unknownStakePoolLabel: {
    id: 'staking.delegationCenter.unknownStakePoolLabel',
    defaultMessage: '!!!unknown',
    description:
      'unknown stake pool label for the Delegation center body section.',
  },
  syncingTooltipLabel: {
    id: 'staking.delegationCenter.syncingTooltipLabel',
    defaultMessage: '!!!Syncing {syncingProgress}%',
    description:
      'unknown stake pool label for the Delegation center body section.',
  },
});

type Props = {
  wallet: Wallet,
  delegatedStakePool?: ?StakePool,
  nextDelegatedStakePool?: ?StakePool,
  nextDelegatedStakePoolEpoch?: ?WalletNextDelegationEpoch,
  lastDelegatedStakePool?: ?StakePool,
  lastDelegatedStakePoolEpoch?: ?WalletNextDelegationEpoch,
  numberOfStakePools: number,
  onDelegate: Function,
  onMenuItemClick: Function,
  getStakePoolById: Function,
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

  onMenuItemClick = ({ value }: { value: DelegationAction }) => {
    console.debug('onMenuItemClick: ', value);
    const { wallet } = this.props;
    this.props.onMenuItemClick(value, wallet.id);
  };

  render() {
    const { intl } = this.context;
    const {
      wallet: {
        name,
        amount,
        delegatedStakePoolId,
        delegationStakePoolStatus,
        nextDelegationStakePoolId,
        nextDelegationStakePoolStatus,
        lastDelegationStakePoolId,
        lastDelegationStakePoolStatus,
        isRestoring,
        syncState,
        pendingDelegations,
      },
      delegatedStakePool,
      nextDelegatedStakePoolEpoch,
      lastDelegatedStakePool,
      lastDelegatedStakePoolEpoch,
      numberOfStakePools,
      getStakePoolById,
    } = this.props;

    const syncingProgress = get(syncState, 'progress.quantity', '');

    const stakePoolToShow =
      lastDelegationStakePoolId &&
      lastDelegationStakePoolStatus !== 'not_delegating'
        ? lastDelegatedStakePool
        : delegatedStakePool;

    const { ranking } = stakePoolToShow || {};

    const color =
      stakePoolToShow && !isNil(ranking)
        ? getColorFromRange(ranking, numberOfStakePools)
        : null;

    const delegated = intl.formatMessage(messages.delegated);
    const notDelegated = intl.formatMessage(messages.notDelegated);
    const changeDelegation = intl.formatMessage(messages.changeDelegation);
    const removeDelegation = intl.formatMessage(messages.removeDelegation);
    const delegate = intl.formatMessage(messages.delegate);
    const delegatedStakePoolTicker = delegatedStakePool
      ? `[${delegatedStakePool.ticker}]`
      : notDelegated.toLowerCase();
    const stakePoolToShowTicker = stakePoolToShow
      ? `[${stakePoolToShow.ticker}]`
      : notDelegated.toLowerCase();
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

    const displayAsDelegated =
      (!!delegatedStakePoolId || !!lastDelegationStakePoolId) && lastDelegationStakePoolStatus !== 'not_delegating';

    const displayPendingDelegationInfo =
      !!lastDelegationStakePoolStatus || !!nextDelegationStakePoolStatus;

    // const hasPendingDelegations =
    //   nextDelegatedStakePoolEpoch || lastDelegatedStakePoolEpoch;

    // @TODO - remove comment - NEW PART
    const hasActiveDelegation = delegatedStakePoolId && delegationStakePoolStatus !== 'not_delegating';
    // const hasPendingDelegations = nextDelegationStakePoolId || lastDelegationStakePoolId;
    const hasPendingDelegations = pendingDelegations && pendingDelegations.length > 0;
    const isDelegating = hasActiveDelegation || hasPendingDelegations;
    const delegatedStakePoolColor = delegatedStakePool && !isNil(delegatedStakePool.ranking)
      ? getColorFromRange(delegatedStakePool.ranking, numberOfStakePools)
      : null;
    // const canUndelegate = (!lastDelegationStakePoolId && !!delegatedStakePoolId && delegationStakePoolStatus !== 'not_delegating') || (!!lastDelegationStakePoolId && lastDelegationStakePoolStatus !== 'not_delegating')

    // Check last
    // if last exist
      // check if is delegated
        // // yes - TRUE
    // else
      // check if current is delegated
        // yes - TRUE

    const canUndelegate = (hasPendingDelegations && pendingDelegations[pendingDelegations.length - 1].status !== 'not_delegating') || (!hasPendingDelegations && !!delegatedStakePoolId && delegationStakePoolStatus !== 'not_delegating')

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

        {/* Delegation preferences */}
        <div className={styles.right}>
          {!isRestoring ? (
            <Fragment>
              <div>
                {/* Statuses */}
                <div className={styles.status}>
                  {/* Current (active) delegation */}
                  <span style={{ color: delegatedStakePoolColor }} className={styles.ticker}>{
                    delegatedStakePool ?

                        <Tooltip
                          skin={TooltipSkin}
                          // TIP: Currently earning rewards
                          tip={
                            <div className={styles.tooltipLabelWrapper}>
                              Currently earning rewards
                            </div>
                          }
                        >
                          <div>
                          <SVGInline
                            svg={adaIcon}
                            className={styles.activeAdaSymbol}
                          />
                          [{delegatedStakePool.ticker}]
                          </div>
                        </Tooltip>
                      :
                      notDelegated
                  }</span>

                  {/* Pending (next) delegations */}
                  {hasPendingDelegations && ' > '}
                  {hasPendingDelegations && map(pendingDelegations, (pendingDelegation, key) => {
                    const pendingDelegationStakePool = getStakePoolById(pendingDelegation.target);
                    const isLast = (key + 1) === pendingDelegations.length;

                    if (!pendingDelegationStakePool) {
                      return ([
                        <span className={styles.ticker}>{notDelegated}</span>,
                        !isLast && ' > '
                      ]);
                    }

                    const pendingStakePoolColor = pendingDelegationStakePool && !isNil(pendingDelegationStakePool.ranking)
                      ? getColorFromRange(pendingDelegationStakePool.ranking, numberOfStakePools)
                      : null;
                    return ([
                      <span className={styles.ticker} style={{ color: pendingStakePoolColor }}>[{pendingDelegationStakePool.ticker}]</span>,
                      !isLast && ' > '
                    ]);
                  })}
                </div>



                {/* Actions */}
                <div className={styles.action}>
                  {canUndelegate &&
                    <span
                      className={styles.actionUndelegate}
                      role="presentation"
                      // @TODO - change handler onMenuItemClick to somethin more specific outside menu
                      onClick={this.onMenuItemClick.bind(this, {value: DelegationActions.REMOVE_DELEGATION})}
                    >
                      {/* @TODO - add translations for "OR" */}
                      {removeDelegation}
                    </span>
                  }
                  {canUndelegate && <span> or </span>}
                  <span
                    className={styles.actionDelegate}
                    role="presentation"
                    // @TODO - change handler onMenuItemClick to somethin more specific outside menu
                    onClick={(!delegatedStakePool && !hasPendingDelegations) ? this.onDelegate : this.onMenuItemClick.bind(this, {value: DelegationActions.CHANGE_DELEGATION})}
                  >
                    {delegate}
                  </span>
                </div>


              </div>
              <div>
                <div
                  className={styles.stakePoolRankingIndicator}
                  style={{ background: color }}
                />
              </div>
            </Fragment>
          ) : (
            <Tooltip
              skin={TooltipSkin}
              themeOverrides={tooltipStyles}
              tip={intl.formatMessage(messages.syncingTooltipLabel, {
                syncingProgress,
              })}
            >
              <LoadingSpinner medium />
            </Tooltip>
          )}
        </div>
      </div>
    );
  }
}
