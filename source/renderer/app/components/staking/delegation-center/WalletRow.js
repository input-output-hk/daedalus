// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { isNil, get } from 'lodash';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import Wallet from '../../../domains/Wallet';
import StakePool, { DelegationActions } from '../../../domains/StakePool';
import { getColorFromRange } from '../../../utils/colors';
import settingsIcon from '../../../assets/images/settings-ic.inline.svg';
import sandClockIcon from '../../../assets/images/sand-clock.inline.svg';
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
        nextDelegationStakePoolId,
        lastDelegationStakePoolId,
        isRestoring,
        syncState,
      },
      delegatedStakePool,
      nextDelegatedStakePool,
      nextDelegatedStakePoolEpoch,
      lastDelegatedStakePool,
      lastDelegatedStakePoolEpoch,
      numberOfStakePools,
    } = this.props;

    const syncingProgress = get(syncState, 'progress.quantity', '');

    const nextEpochNumber = get(
      nextDelegatedStakePoolEpoch,
      'epoch_number',
      null
    );

    const stakePoolToShow = lastDelegationStakePoolId
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
    const lastDelegatedStakePoolTicker = lastDelegatedStakePool
      ? `[${lastDelegatedStakePool.ticker}]`
      : notDelegated.toLowerCase();
    const nextDelegatedStakePoolTicker = nextDelegatedStakePool
      ? `[${nextDelegatedStakePool.ticker}]`
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

    const isDelegationActive =
      delegatedStakePoolId || nextDelegationStakePoolId;

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
          {!isRestoring ? (
            <Fragment>
              <div>
                <div className={styles.status}>
                  <span>{isDelegationActive ? delegated : notDelegated}</span>
                  {isDelegationActive && (
                    <DropdownMenu
                      label={
                        <SVGInline
                          svg={settingsIcon}
                          className={styles.gearIcon}
                        />
                      }
                      menuItems={delegatedWalletActionOptions}
                      onMenuItemClick={this.onMenuItemClick}
                    />
                  )}
                </div>
                <div className={styles.action}>
                  {isDelegationActive ? (
                    <Fragment>
                      {nextDelegatedStakePoolEpoch ? (
                        <Fragment>
                          <Tooltip
                            skin={TooltipSkin}
                            tip={
                              <div className={styles.tooltipLabelWrapper}>
                                {intl.formatMessage(
                                  messages.toStakePoolTooltipTickerPart1
                                )}
                                {': '}
                                <span
                                  className={
                                    delegatedStakePool ? styles.unknown : null
                                  }
                                >
                                  {delegatedStakePoolTicker}
                                </span>
                                {nextDelegatedStakePoolEpoch && (
                                  <Fragment>
                                    <br />
                                    {intl.formatMessage(
                                      messages.toStakePoolTooltipTickerPart2
                                    )}{' '}
                                    {nextEpochNumber}
                                    {': '}
                                    {nextDelegationStakePoolId ? (
                                      <span
                                        className={
                                          nextDelegatedStakePool
                                            ? styles.unknown
                                            : null
                                        }
                                      >
                                        {nextDelegatedStakePoolTicker}
                                      </span>
                                    ) : (
                                      <span
                                        className={
                                          !nextDelegatedStakePool
                                            ? styles.unknown
                                            : null
                                        }
                                      >
                                        {notDelegated}
                                      </span>
                                    )}
                                  </Fragment>
                                )}
                              </div>
                            }
                          >
                            <SVGInline
                              svg={sandClockIcon}
                              className={styles.sandClockIcon}
                            />
                          </Tooltip>
                          {intl.formatMessage(messages.toStakePoolTickerPart1)}{' '}
                          <span
                            className={
                              lastDelegatedStakePool ? styles.unknown : null
                            }
                            style={{ color }}
                          >
                            {lastDelegatedStakePoolTicker}
                          </span>{' '}
                          {intl.formatMessage(messages.toStakePoolTickerPart2)}
                        </Fragment>
                      ) : (
                        <Fragment>
                          {intl.formatMessage(messages.toStakePoolTickerPart1)}{' '}
                          <span
                            className={
                              !delegatedStakePool ? styles.unknown : null
                            }
                            style={{ color }}
                          >
                            {delegatedStakePoolTicker}
                          </span>{' '}
                          {intl.formatMessage(messages.toStakePoolTickerPart2)}
                        </Fragment>
                      )}
                    </Fragment>
                  ) : (
                    <Fragment>
                      {lastDelegatedStakePoolEpoch ? (
                        <Fragment>
                          <Tooltip
                            skin={TooltipSkin}
                            tip={
                              <div className={styles.tooltipLabelWrapper}>
                                <Fragment>
                                  {intl.formatMessage(
                                    messages.toStakePoolTooltipTickerPart2
                                  )}{' '}
                                  {nextEpochNumber}
                                  {': '}
                                  {nextDelegationStakePoolId ? (
                                    <span
                                      className={
                                        lastDelegatedStakePool
                                          ? styles.unknown
                                          : null
                                      }
                                    >
                                      {lastDelegatedStakePoolTicker}
                                    </span>
                                  ) : (
                                    <span
                                      className={
                                        !lastDelegatedStakePool
                                          ? styles.unknown
                                          : null
                                      }
                                    >
                                      {notDelegated}
                                    </span>
                                  )}
                                </Fragment>
                              </div>
                            }
                          >
                            <SVGInline
                              svg={sandClockIcon}
                              className={styles.sandClockIcon}
                            />
                          </Tooltip>
                          <span
                            className={styles.actionLink}
                            role="presentation"
                            onClick={this.onDelegate}
                          >
                            {delegate}
                          </span>
                        </Fragment>
                      ) : (
                        <span
                          className={styles.actionLink}
                          role="presentation"
                          onClick={this.onDelegate}
                        >
                          {delegate}
                        </span>
                      )}
                    </Fragment>
                  )}
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
