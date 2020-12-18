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
import { getRelativePosition } from '../../../utils/domManipulation';

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
  onSelect?: Function,
  selectedPoolId?: ?number,
  isListActive?: boolean,
  currentTheme: string,
  onOpenExternalLink: Function,
  showWithSelectButton?: boolean,
  containerClassName: string,
  setListActive?: Function,
  listName?: string,
  highlightOnHover?: boolean,
};

type WalletRowState = {
  highlightedPoolId: ?number,
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
        delegationStakePoolStatus,
        pendingDelegations,
        isHardwareWallet,
      },
      delegatedStakePool,
      numberOfRankedStakePools,
      getStakePoolById,
      onDelegate,
      onUndelegate,
      nextEpochNumber,
      futureEpochNumber,
      currentTheme,
      onOpenExternalLink,
      showWithSelectButton,
      containerClassName,
      highlightOnHover,
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

      if (delegatedStakePoolId) {
        if (!nextPendingDelegationStakePool) {
          nextPendingDelegationStakePool = delegatedStakePool;
          nextPendingDelegationStakePoolId = delegatedStakePoolId;
        } else if (!futurePendingDelegationStakePool) {
          futurePendingDelegationStakePool = delegatedStakePool;
          futurePendingDelegationStakePoolId = delegatedStakePoolId;
        }
      } else if (nextPendingDelegationStakePool && !futurePendingDelegationStakePool) {
        futurePendingDelegationStakePool = nextPendingDelegationStakePool;
        futurePendingDelegationStakePoolId = nextPendingDelegationStakePoolId;
      }
    }

    if (!hasPendingDelegations && delegatedStakePoolId) {
      nextPendingDelegationStakePool = delegatedStakePool;
      nextPendingDelegationStakePoolId = delegatedStakePoolId;
      futurePendingDelegationStakePool = delegatedStakePool;
      futurePendingDelegationStakePoolId = delegatedStakePoolId;
    }

    const futureDelegationStatus =
      hasPendingDelegations && futurePendingDelegationStakePool
        ? futurePendingDelegationStakePool.status
        : delegationStakePoolStatus;
    const isFutureDelegationDelegating =
      futureDelegationStatus !== WalletDelegationStatuses.NOT_DELEGATING;

    const stakePoolRankingColor = !futurePendingDelegationStakePool
      ? null
      : getColorFromRange(
          futurePendingDelegationStakePool.ranking,
          numberOfRankedStakePools
        );

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

    const isHighlighted =
      futurePendingDelegationStakePoolId && futurePendingDelegationStakePool
        ? this.getIsHighlighted(futurePendingDelegationStakePool.id)
        : false;
    const { top, left } = this.state;

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
              <div
                className={futureStakePoolTileStyles}
                onMouseLeave={highlightOnHover ? this.handleCloseTooltip : null}
              >
                {futurePendingDelegationStakePoolId ? (
                  <div
                    className={
                      !futurePendingDelegationStakePool ? styles.unknown : null
                    }
                    onMouseEnter={(event) =>
                      highlightOnHover
                        ? this.handleOpenTooltip(
                            event,
                            futurePendingDelegationStakePool
                          )
                        : null
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
                        {isHighlighted && (
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
                        )}
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

  getIsHighlighted = (id: string) =>
    this.props.isListActive !== false && id === this.state.highlightedPoolId;

  handleCloseTooltip = () => {
    const { isListActive, setListActive } = this.props;
    this.setState({
      ...initialWalletRowState,
    });
    if (isListActive !== false && setListActive) setListActive(null);
  };

  handleOpenTooltip = (
    poolId: SyntheticMouseEvent<HTMLElement>,
    futurePendingDelegationStakePool: StakePool
  ) => {
    const {
      isListActive,
      setListActive,
      listName,
      containerClassName,
    } = this.props;
    if (poolId.target) {
      poolId.persist();
      const targetElement = poolId.target;
      if (targetElement instanceof HTMLElement) {
        const { top, left } = getRelativePosition(
          targetElement,
          `.${containerClassName}`
        );
        this.setState({ top, left });
      }
    }
    if (isListActive === false && setListActive) setListActive(listName);
    const targetEl = poolId.currentTarget;
    const { parentElement } = targetEl;
    if (parentElement) {
      const highlightedPoolId = futurePendingDelegationStakePool
        ? futurePendingDelegationStakePool.id
        : null;
      return this.setState({
        highlightedPoolId,
      });
    }
    return null;
  };
}
