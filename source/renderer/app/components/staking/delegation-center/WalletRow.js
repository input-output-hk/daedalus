// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { isNil, get, map } from 'lodash';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import Wallet from '../../../domains/Wallet';
import StakePool from '../../../domains/StakePool';
import { getColorFromRange } from '../../../utils/colors';
import adaIcon from '../../../assets/images/ada-symbol.inline.svg';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import styles from './WalletRow.scss';
import tooltipStyles from './WalletRowTooltip.scss';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import arrow from '../../../assets/images/collapse-arrow.inline.svg';

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
  or: {
    id: 'staking.delegationCenter.or',
    defaultMessage: '!!!or',
    description: '"or" text for the Delegation center body section.',
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
  stakePoolTooltipTickerEarningRewards: {
    id: 'staking.delegationCenter.stakePoolTooltipTickerEarningRewards',
    defaultMessage: '!!!Currently earning rewards',
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
  lastDelegatedStakePool?: ?StakePool,
  numberOfStakePools: number,
  onDelegate: Function,
  onUndelegate: Function,
  getStakePoolById: Function,
};

@observer
export default class WalletRow extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      wallet: {
        name,
        amount,
        delegatedStakePoolId,
        delegationStakePoolStatus,
        lastDelegationStakePoolId,
        lastDelegationStakePoolStatus,
        isRestoring,
        syncState,
        pendingDelegations,
      },
      delegatedStakePool,
      lastDelegatedStakePool,
      numberOfStakePools,
      getStakePoolById,
      onDelegate,
      onUndelegate,
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

    const notDelegatedText = intl.formatMessage(messages.notDelegated);
    const removeDelegationText = intl.formatMessage(messages.removeDelegation);
    const delegateText = intl.formatMessage(messages.delegate);
    const orText = intl.formatMessage(messages.or);

    const hasPendingDelegations =
      pendingDelegations && pendingDelegations.length > 0;
    // const isDelegating = hasActiveDelegation || hasPendingDelegations;
    const delegatedStakePoolColor =
      delegatedStakePool && !isNil(delegatedStakePool.ranking)
        ? getColorFromRange(delegatedStakePool.ranking, numberOfStakePools)
        : null;

    // @TODO - @Danilo - remove comment
    // Check last
    // if last exist
    // check if is delegated
    // yes - TRUE
    // else
    // check if current is delegated
    // yes - TRUE

    const canUndelegate =
      (hasPendingDelegations &&
        pendingDelegations[pendingDelegations.length - 1].status !==
          'not_delegating') ||
      (!hasPendingDelegations &&
        !!delegatedStakePoolId &&
        delegationStakePoolStatus !== 'not_delegating');

    const actionStyles = classnames([
      styles.action,
      canUndelegate ? styles.undelegateActions : null,
    ]);

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
                  <span
                    style={{ color: delegatedStakePoolColor }}
                    className={styles.ticker}
                  >
                    {delegatedStakePool ? (
                      <Tooltip
                        skin={TooltipSkin}
                        tip={
                          <div className={styles.tooltipLabelWrapper}>
                            {intl.formatMessage(
                              messages.stakePoolTooltipTickerEarningRewards
                            )}
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
                    ) : (
                      notDelegatedText
                    )}
                  </span>

                  {/* Pending (next) delegations */}
                  {hasPendingDelegations && (
                    <SVGInline
                      key="arrow"
                      svg={arrow}
                      className={styles.arrow}
                    />
                  )}
                  {hasPendingDelegations &&
                    map(pendingDelegations, (pendingDelegation, key) => {
                      const pendingDelegationStakePool = getStakePoolById(
                        pendingDelegation.target
                      );
                      const isLast = key + 1 === pendingDelegations.length;
                      const fromEpoch = get(
                        pendingDelegation,
                        'changes_at.epoch_number',
                        0
                      );

                      if (!pendingDelegationStakePool) {
                        return [
                          <span key="ticker" className={styles.ticker}>
                            {notDelegatedText}
                          </span>,
                          !isLast && (
                            <SVGInline
                              key="arrow"
                              svg={arrow}
                              className={styles.arrow}
                            />
                          ),
                        ];
                      }

                      const pendingStakePoolColor =
                        pendingDelegationStakePool &&
                        !isNil(pendingDelegationStakePool.ranking)
                          ? getColorFromRange(
                              pendingDelegationStakePool.ranking,
                              numberOfStakePools
                            )
                          : null;
                      return [
                        <Tooltip
                          skin={TooltipSkin}
                          key="ticker"
                          tip={
                            <div className={styles.tooltipLabelWrapper}>
                              {intl.formatMessage(
                                messages.toStakePoolTooltipTickerPart2
                              )}{' '}
                              {fromEpoch}
                            </div>
                          }
                        >
                          <span
                            className={styles.ticker}
                            style={{ color: pendingStakePoolColor }}
                          >
                            [{pendingDelegationStakePool.ticker}]
                          </span>
                        </Tooltip>,
                        !isLast && (
                          <SVGInline
                            key="arrow"
                            svg={arrow}
                            className={styles.arrow}
                          />
                        ),
                      ];
                    })}
                </div>

                {/* Actions */}
                <div className={actionStyles}>
                  {canUndelegate && [
                    <span
                      className={styles.actionUndelegate}
                      role="presentation"
                      onClick={onUndelegate}
                      key="undelegate"
                    >
                      {removeDelegationText}
                    </span>,
                    <span key="or"> {orText} </span>,
                  ]}
                  <span
                    className={styles.actionDelegate}
                    role="presentation"
                    onClick={onDelegate}
                  >
                    {delegateText}
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
