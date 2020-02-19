// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { isNil, get, map } from 'lodash';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import Wallet, { WalletDelegationStatuses } from '../../../domains/Wallet';
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
  notDelegated: {
    id: 'staking.delegationCenter.notDelegated',
    defaultMessage: '!!!Undelegated',
    description: 'Undelegated label for the Delegation center body section.',
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
  stakePoolTooltipTickerEpoch: {
    id: 'staking.delegationCenter.stakePoolTooltipTickerEpoch',
    defaultMessage: '!!!From epoch {fromEpoch}',
    description:
      'Delegated stake pool tooltip ticker for the Delegation center body section.',
  },
  stakePoolTooltipTickerEarningRewards: {
    id: 'staking.delegationCenter.stakePoolTooltipTickerEarningRewards',
    defaultMessage: '!!!Earning rewards',
    description:
      'Delegated stake pool tooltip ticker for the Delegation center body section.',
  },
  delegate: {
    id: 'staking.delegationCenter.delegate',
    defaultMessage: '!!!Delegate',
    description: 'Delegate label for the Delegation center body section.',
  },
  redelegate: {
    id: 'staking.delegationCenter.redelegate',
    defaultMessage: '!!!Redelegate',
    description: 'Redelegate label for the Delegation center body section.',
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
        isRestoring,
        syncState,
        delegationStakePoolStatus,
        pendingDelegations,
      },
      delegatedStakePool,
      numberOfStakePools,
      getStakePoolById,
      onDelegate,
      onUndelegate,
    } = this.props;

    const syncingProgress = get(syncState, 'progress.quantity', '');
    const notDelegatedText = intl.formatMessage(messages.notDelegated);
    const removeDelegationText = intl.formatMessage(messages.removeDelegation);
    const delegateText = intl.formatMessage(messages.delegate);
    const redelegateText = intl.formatMessage(messages.redelegate);
    const orText = intl.formatMessage(messages.or);

    const hasPendingDelegations =
      pendingDelegations && pendingDelegations.length > 0;
    const lastDelegationStatus = hasPendingDelegations
      ? pendingDelegations[pendingDelegations.length - 1].status
      : delegationStakePoolStatus;
    const isLastDelegationDelegating =
      lastDelegationStatus !== WalletDelegationStatuses.NOT_DELEGATING;
    const delegatedStakePoolColor =
      delegatedStakePool && !isNil(delegatedStakePool.ranking)
        ? getColorFromRange(delegatedStakePool.ranking, numberOfStakePools)
        : null;

    const actionStyles = classnames([
      styles.action,
      isLastDelegationDelegating ? styles.undelegateActions : null,
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
                  {/* Active (current) delegation */}
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

                  {/* Next (pending) delegations */}
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
                        ['changes_at', 'epoch_number'],
                        0
                      );

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
                              <FormattedMessage
                                {...messages.stakePoolTooltipTickerEpoch}
                                values={{
                                  fromEpoch,
                                }}
                              />
                            </div>
                          }
                        >
                          <span
                            className={styles.ticker}
                            style={{ color: pendingStakePoolColor }}
                          >
                            {pendingDelegationStakePool
                              ? `[${pendingDelegationStakePool.ticker}]`
                              : notDelegatedText}
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
                  {isLastDelegationDelegating && [
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
                    {!isLastDelegationDelegating
                      ? delegateText
                      : redelegateText}
                  </span>
                </div>
              </div>
              <div>
                <div
                  className={styles.stakePoolRankingIndicator}
                  style={{ background: delegatedStakePoolColor }}
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
