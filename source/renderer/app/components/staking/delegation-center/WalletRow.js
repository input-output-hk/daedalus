// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import Wallet, { WalletDelegationStatuses } from '../../../domains/Wallet';
import StakePool from '../../../domains/StakePool';
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
import adaIcon from '../../../assets/images/ada-symbol.inline.svg';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import styles from './WalletRow.scss';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import arrow from '../../../assets/images/collapse-arrow.inline.svg';
import hardwareWalletsIcon from '../../../assets/images/hardware-wallet/connect-ic.inline.svg';
import {
  IS_RANKING_DATA_AVAILABLE,
  IS_SATURATION_DATA_AVAILABLE,
} from '../../../config/stakingConfig';
import noDataDashBigImage from '../../../assets/images/no-data-dash-big.inline.svg';

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
  TooltipPoolTickerEpoch: {
    id: 'staking.delegationCenter.stakePoolTooltipTickerEpoch',
    defaultMessage: '!!!From epoch {fromEpoch}',
    description:
      'Delegated stake pool tooltip ticker for the Delegation center body section.',
  },
  TooltipPoolTickerEarningRewards: {
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
  numberOfRankedStakePools?: number,
  onDelegate: Function,
  onUndelegate: Function,
  getStakePoolById: Function,
  nextEpochNumber: ?number,
  futureEpochNumber: ?number,
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
        delegatedStakePoolId,
        delegationStakePoolStatus,
        pendingDelegations,
        isHardwareWallet,
      },
      delegatedStakePool,
      numberOfStakePools,
      numberOfRankedStakePools,
      getStakePoolById,
      onDelegate,
      onUndelegate,
      nextEpochNumber,
      futureEpochNumber,
    } = this.props;

    // @TODO - remove once quit stake pool delegation is connected with rewards balance
    const isUndelegateBlocked = true;

    const syncingProgress = get(syncState, 'progress.quantity', '');
    const notDelegatedText = intl.formatMessage(messages.notDelegated);
    const removeDelegationText = intl.formatMessage(messages.removeDelegation);
    const delegateText = intl.formatMessage(messages.delegate);
    const redelegateText = intl.formatMessage(messages.redelegate);

    const hasPendingDelegations =
      pendingDelegations && pendingDelegations.length > 0;
    const futureDelegationStatus = hasPendingDelegations
      ? pendingDelegations[pendingDelegations.length - 1].status
      : delegationStakePoolStatus;
    const isFutureDelegationDelegating =
      futureDelegationStatus !== WalletDelegationStatuses.NOT_DELEGATING;

    let nextPendingDelegationStakePool;
    let futurePendingDelegationStakePool;
    let nextPendingDelegationStakePoolId;
    let futurePendingDelegationStakePoolId;
    if (hasPendingDelegations) {
      const nextPendingDelegation = pendingDelegations.filter(
        (item) =>
          get(item, ['changes_at', 'epoch_number'], 0) === nextEpochNumber
      );
      const futurePendingDelegation = pendingDelegations.filter(
        (item) =>
          get(item, ['changes_at', 'epoch_number'], 0) === futureEpochNumber
      );

      nextPendingDelegationStakePoolId = nextPendingDelegation.length
        ? nextPendingDelegation[0].target
        : null;
      futurePendingDelegationStakePoolId = futurePendingDelegation.length
        ? futurePendingDelegation[0].target
        : null;
      nextPendingDelegationStakePool = getStakePoolById(
        nextPendingDelegationStakePoolId
      );
      futurePendingDelegationStakePool = getStakePoolById(
        futurePendingDelegationStakePoolId
      );
    }

    const stakePoolRankingColor = !futurePendingDelegationStakePool
      ? null
      : getColorFromRange(
          futurePendingDelegationStakePool.ranking,
          numberOfStakePools
        );

    const saturationClassnames = classnames([
      styles.saturationBar,
      futurePendingDelegationStakePool
        ? styles[
            getSaturationColor(futurePendingDelegationStakePool.saturation)
          ]
        : null,
    ]);

    const futureStakePoolTileStyles = classnames([
      styles.stakePoolTile,
      futurePendingDelegationStakePoolId && futurePendingDelegationStakePool
        ? styles.futureStakePoolTileDelegated
        : styles.futureStakePoolTitleUndelegated,
      futurePendingDelegationStakePoolId && !futurePendingDelegationStakePool
        ? styles.futureStakePoolTitleUndefined
        : null,
    ]);

    return (
      <div className={styles.component}>
        <div className={styles.left}>
          <div className={styles.title}>
            {name}
            {isHardwareWallet && (
              <SVGInline
                svg={hardwareWalletsIcon}
                className={styles.hardwareWalletsIcon}
              />
            )}
          </div>
          <div className={styles.description}>
            {!isRestoring ? (
              <FormattedMessage
                {...messages.walletAmount}
                values={{
                  amount: amount.toFormat(DECIMAL_PLACES_IN_ADA),
                }}
              />
            ) : (
              '-'
            )}
          </div>
        </div>

        <div className={styles.right}>
          {!isRestoring ? (
            <Fragment>
              <div className={styles.stakePoolTile}>
                {delegatedStakePoolId ? (
                  <PopOver
                    content={
                      <div className={styles.tooltipLabelWrapper}>
                        <span>
                          {intl.formatMessage(
                            messages.TooltipPoolTickerEarningRewards
                          )}
                        </span>
                      </div>
                    }
                  >
                    <div
                      className={!delegatedStakePool ? styles.unknown : null}
                    >
                      {delegatedStakePool ? (
                        <>
                          <SVGInline
                            svg={adaIcon}
                            className={styles.activeAdaSymbol}
                          />
                          <span className={styles.stakePoolTicker}>
                            {delegatedStakePool.ticker}
                          </span>
                        </>
                      ) : (
                        <span className={styles.stakePoolUnknown}>
                          {intl.formatMessage(messages.unknownStakePoolLabel)}
                        </span>
                      )}
                    </div>
                  </PopOver>
                ) : (
                  <span className={styles.nonDelegatedText}>
                    {notDelegatedText}
                  </span>
                )}
              </div>
              <SVGInline svg={arrow} className={styles.arrow} />
              <div className={styles.stakePoolTile}>
                {nextPendingDelegationStakePoolId ? (
                  <div
                    className={
                      !nextPendingDelegationStakePool ? styles.unknown : null
                    }
                  >
                    {nextPendingDelegationStakePool ? (
                      <span className={styles.stakePoolTicker}>
                        {nextPendingDelegationStakePool.ticker}
                      </span>
                    ) : (
                      <span className={styles.stakePoolUnknown}>
                        {intl.formatMessage(messages.unknownStakePoolLabel)}
                      </span>
                    )}
                  </div>
                ) : (
                  <span className={styles.nonDelegatedText}>
                    {notDelegatedText}
                  </span>
                )}
              </div>
              <SVGInline svg={arrow} className={styles.arrow} />
              <div className={futureStakePoolTileStyles}>
                {futurePendingDelegationStakePoolId ? (
                  <div
                    className={
                      !futurePendingDelegationStakePool ? styles.unknown : null
                    }
                  >
                    {futurePendingDelegationStakePool ? (
                      <>
                        <span className={styles.stakePoolTicker}>
                          {futurePendingDelegationStakePool.ticker}
                        </span>
                        {IS_RANKING_DATA_AVAILABLE ? (
                          <div
                            className={styles.ranking}
                            style={{ color: stakePoolRankingColor }}
                          >
                            {futurePendingDelegationStakePool.nonMyopicMemberRewards ? (
                              futurePendingDelegationStakePool.ranking
                            ) : (
                              <>
                                {numberOfRankedStakePools + 1}
                                <sup>*</sup>
                              </>
                            )}
                          </div>
                        ) : (
                          <div className={styles.noDataDash}>
                            <SVGInline svg={noDataDashBigImage} />
                          </div>
                        )}
                        {IS_SATURATION_DATA_AVAILABLE && (
                          <div className={saturationClassnames}>
                            <span
                              style={{
                                width: `${parseFloat(
                                  futurePendingDelegationStakePool.saturation
                                ).toFixed(2)}%`,
                              }}
                            />
                          </div>
                        )}
                        <div
                          className={styles.stakePoolRankingIndicator}
                          style={{ background: stakePoolRankingColor }}
                        />
                      </>
                    ) : (
                      <span className={styles.stakePoolUnknown}>
                        {intl.formatMessage(messages.unknownStakePoolLabel)}
                      </span>
                    )}
                  </div>
                ) : (
                  <span className={styles.nonDelegatedText}>
                    {notDelegatedText}
                  </span>
                )}
                <div className={styles.action}>
                  {isFutureDelegationDelegating && !isUndelegateBlocked && (
                    <span
                      className={styles.actionUndelegate}
                      role="presentation"
                      onClick={onUndelegate}
                      key="undelegate"
                    >
                      {removeDelegationText}
                    </span>
                  )}
                  <span
                    className={styles.actionDelegate}
                    role="presentation"
                    onClick={onDelegate}
                  >
                    {!isFutureDelegationDelegating
                      ? delegateText
                      : redelegateText}
                  </span>
                </div>
              </div>
            </Fragment>
          ) : (
            <PopOver
              content={intl.formatMessage(messages.syncingTooltipLabel, {
                syncingProgress,
              })}
            >
              <LoadingSpinner medium />
            </PopOver>
          )}
        </div>
      </div>
    );
  }
}
