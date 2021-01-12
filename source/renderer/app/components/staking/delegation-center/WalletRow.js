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
import TooltipPool from '../widgets/TooltipPool';

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
  numberOfRankedStakePools: number,
  onDelegate: Function,
  onUndelegate: Function,
  getStakePoolById: Function,
  nextEpochNumber: ?number,
  futureEpochNumber: ?number,
  onSelect?: Function,
  selectedPoolId?: ?number,
  isListActive?: boolean,
  currentTheme: string,
  onOpenExternalLink: Function,
  showWithSelectButton?: boolean,
  containerClassName: string,
  setListActive?: Function,
  listName?: string,
};

type WalletRowState = {
  highlightedPoolId: ?string,
  top: number,
  left: number,
};

const initialWalletRowState = {
  highlightedPoolId: null,
  top: 0,
  left: 0,
};

@observer
export default class WalletRow extends Component<Props, WalletRowState> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    ...initialWalletRowState,
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
        pendingDelegations,
        isHardwareWallet,
      },
      delegatedStakePool,
      numberOfRankedStakePools,
      onDelegate,
      onUndelegate,
      nextEpochNumber,
      futureEpochNumber,
      currentTheme,
      onOpenExternalLink,
      showWithSelectButton,
      containerClassName,
    } = this.props;

    // @TODO - remove once quit stake pool delegation is connected with rewards balance
    const isUndelegateBlocked = true;

    const syncingProgress = get(syncState, 'progress.quantity', '');
    const notDelegatedText = intl.formatMessage(messages.notDelegated);
    const removeDelegationText = intl.formatMessage(messages.removeDelegation);
    const delegateText = intl.formatMessage(messages.delegate);
    const redelegateText = intl.formatMessage(messages.redelegate);

    const {
      stakePoolId: nextPendingDelegationStakePoolId,
      stakePool: nextPendingDelegationStakePool,
    } = this.getPendingStakePool(nextEpochNumber || 0);

    const {
      stakePoolId: futurePendingDelegationStakePoolId,
      stakePool: futurePendingDelegationStakePool,
    } = this.getPendingStakePool(
      futureEpochNumber || 0,
      nextPendingDelegationStakePool
    );

    const futureDelegationStatus =
      pendingDelegations &&
      pendingDelegations.length &&
      futurePendingDelegationStakePool &&
      futurePendingDelegationStakePool.status
        ? futurePendingDelegationStakePool.status
        : false;

    const isFutureDelegationDelegating =
      futureDelegationStatus !== WalletDelegationStatuses.NOT_DELEGATING;
    const stakePoolRankingColor = futurePendingDelegationStakePool
      ? getColorFromRange(
          futurePendingDelegationStakePool.ranking,
          numberOfRankedStakePools
        )
      : '';

    const saturationStyles = classnames([
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

    const rightContainerStyles = classnames([
      styles.right,
      isRestoring ? styles.isRestoring : null,
    ]);

    const { top, left } = this.state;

    const popOverThemeVariables = {
      '--rp-pop-over-bg-color':
        'var(--theme-staking-stake-pool-tooltip-background-color)',
      '--rp-pop-over-box-shadow':
        '0 1.5px 5px 0 var(--theme-staking-stake-pool-tooltip-shadow-color)',
      '--rp-pop-over-border-color':
        'var(--theme-staking-stake-pool-tooltip-border-color)',
      '--rp-pop-over-border-radius': '5px',
      '--rp-pop-over-border-style': 'solid',
    };

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

        <div className={rightContainerStyles}>
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
                        <div className={styles.stakePoolName}>
                          <SVGInline
                            svg={adaIcon}
                            className={styles.activeAdaSymbol}
                          />
                          <div className={styles.stakePoolTicker}>
                            {delegatedStakePool.ticker}
                          </div>
                        </div>
                      ) : (
                        <div className={styles.stakePoolUnknown}>
                          {intl.formatMessage(messages.unknownStakePoolLabel)}
                        </div>
                      )}
                    </div>
                  </PopOver>
                ) : (
                  <div className={styles.nonDelegatedText}>
                    {notDelegatedText}
                  </div>
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
                      <div className={styles.stakePoolTicker}>
                        {nextPendingDelegationStakePool.ticker}
                      </div>
                    ) : (
                      <div className={styles.stakePoolUnknown}>
                        {intl.formatMessage(messages.unknownStakePoolLabel)}
                      </div>
                    )}
                  </div>
                ) : (
                  <div className={styles.nonDelegatedText}>
                    {notDelegatedText}
                  </div>
                )}
              </div>
              <SVGInline svg={arrow} className={styles.arrow} />
              <div className={futureStakePoolTileStyles}>
                {futurePendingDelegationStakePoolId ? (
                  <div>
                    {futurePendingDelegationStakePool ? (
                      <PopOver
                        key="stakePoolTooltip"
                        placement="auto"
                        maxWidth={300}
                        popperOptions={{
                          strategy: 'fixed',
                        }}
                        themeVariables={popOverThemeVariables}
                        allowHTML
                        content={
                          <TooltipPool
                            stakePool={futurePendingDelegationStakePool}
                            isVisible
                            onClick={this.handleCloseTooltip}
                            currentTheme={currentTheme}
                            onOpenExternalLink={onOpenExternalLink}
                            top={top}
                            left={left}
                            color={stakePoolRankingColor}
                            showWithSelectButton={showWithSelectButton}
                            containerClassName={containerClassName}
                            numberOfRankedStakePools={numberOfRankedStakePools}
                            isDelegationView
                          />
                        }
                      >
                        <div className={styles.stakePoolTicker}>
                          {futurePendingDelegationStakePool.ticker}
                        </div>
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
                          <div className={saturationStyles}>
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
                      </PopOver>
                    ) : (
                      <div className={styles.stakePoolUnknown}>
                        {intl.formatMessage(messages.unknownStakePoolLabel)}
                      </div>
                    )}
                  </div>
                ) : (
                  <div className={styles.nonDelegatedText}>
                    {notDelegatedText}
                  </div>
                )}
                {isFutureDelegationDelegating && !isUndelegateBlocked && (
                  <div
                    className={styles.action}
                    role="presentation"
                    onClick={onUndelegate}
                    key="undelegate"
                  >
                    {removeDelegationText}
                  </div>
                )}
                <div
                  className={styles.action}
                  role="presentation"
                  onClick={onDelegate}
                >
                  {!isFutureDelegationDelegating
                    ? delegateText
                    : redelegateText}
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

  handleCloseTooltip = () => {
    const { isListActive, setListActive } = this.props;
    this.setState({
      ...initialWalletRowState,
    });
    if (isListActive !== false && setListActive) setListActive(null);
  };

  getPendingStakePool = (
    epochNumber: number,
    fallbackStakePool: ?StakePool
  ) => {
    const {
      wallet: { delegatedStakePoolId, pendingDelegations },
      delegatedStakePool,
      getStakePoolById,
    } = this.props;

    let stakePoolId;
    let stakePool;
    const hasPendingDelegations =
      pendingDelegations && pendingDelegations.length;

    if (hasPendingDelegations) {
      const pendingDelegation = pendingDelegations.filter(
        (item) => get(item, ['changes_at', 'epoch_number'], 0) === epochNumber
      );
      stakePoolId = get(pendingDelegation, '[0].target');
      stakePool = getStakePoolById(stakePoolId);
    }

    if (!stakePool && fallbackStakePool) {
      stakePoolId = fallbackStakePool.id;
      stakePool = fallbackStakePool;
    }

    if (!stakePool && delegatedStakePoolId) {
      stakePoolId = delegatedStakePoolId;
      stakePool = delegatedStakePool;
    }

    return {
      stakePoolId,
      stakePool,
    };
  };
}
