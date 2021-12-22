import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import Wallet, { WalletDelegationStatuses } from '../../../domains/Wallet';
import type { WalletNextDelegation } from '../../../api/wallets/types';
import StakePool from '../../../domains/StakePool';
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/ada-sym... Remove this comment to see the full error message
import adaIcon from '../../../assets/images/ada-symbol.inline.svg';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import { PoolPopOver } from '../widgets/PoolPopOver';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletRow.scss' or its corre... Remove this comment to see the full error message
import styles from './WalletRow.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletRowPopOverOverrides.sc... Remove this comment to see the full error message
import popOverThemeOverrides from './WalletRowPopOverOverrides.scss';
import LoadingSpinner from '../../widgets/LoadingSpinner';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/collaps... Remove this comment to see the full error message
import arrow from '../../../assets/images/collapse-arrow.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/hardwar... Remove this comment to see the full error message
import hardwareWalletsIcon from '../../../assets/images/hardware-wallet/connect-ic.inline.svg';
import {
  IS_RANKING_DATA_AVAILABLE,
  IS_SATURATION_DATA_AVAILABLE,
} from '../../../config/stakingConfig';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/no-data... Remove this comment to see the full error message
import noDataDashBigImage from '../../../assets/images/no-data-dash-big.inline.svg';
import { WalletAmount } from './widgets';

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
  wallet: Wallet;
  delegatedStakePool?: StakePool | null | undefined;
  numberOfRankedStakePools: number;
  onDelegate: (...args: Array<any>) => any;
  onUndelegate: (...args: Array<any>) => any;
  getStakePoolById: (...args: Array<any>) => any;
  nextEpochNumber: number;
  futureEpochNumber: number;
  currentTheme: string;
  onOpenExternalLink: (...args: Array<any>) => any;
  showWithSelectButton?: boolean;
  containerClassName: string;
};
type WalletRowState = {
  highlightedPoolId: boolean;
};
const initialWalletRowState = {
  highlightedPoolId: false,
};

@observer
class WalletRow extends Component<Props, WalletRowState> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = { ...initialWalletRowState };
  stakePoolFirstTileRef: {
    current: null | HTMLDivElement;
  };
  stakePoolAdaSymbolRef: {
    current: null | HTMLDivElement;
  };

  constructor(props: Props) {
    super(props);
    this.stakePoolFirstTileRef = React.createRef();
    this.stakePoolAdaSymbolRef = React.createRef();
  }

  componentDidUpdate() {
    this.handleFirstTilePopOverStyle();
  }

  handleFirstTilePopOverStyle = () => {
    const {
      wallet: { id },
    } = this.props;
    const existingStyle = document.getElementById(`wallet-row-${id}-style`);
    const { current: firstTileDom } = this.stakePoolFirstTileRef;
    const { current: adaSymbolDom } = this.stakePoolAdaSymbolRef;

    if (!firstTileDom || !adaSymbolDom) {
      if (existingStyle) {
        existingStyle.remove();
      }

      return;
    }

    if (existingStyle) {
      return;
    }

    const firstTileDomRect = firstTileDom.getBoundingClientRect();
    const adaSymbolDomRect = adaSymbolDom.getBoundingClientRect();
    const horizontalDelta =
      firstTileDomRect.width / 2 -
      adaSymbolDomRect.width / 2 -
      (adaSymbolDomRect.left - firstTileDomRect.left);
    const firstTilePopOverStyle = document.createElement('style');
    firstTilePopOverStyle.setAttribute('id', `wallet-row-${id}-style`);
    firstTilePopOverStyle.innerHTML = `.wallet-row-${id} .tippy-arrow { transform: translate(-${horizontalDelta}px, 0); }`;
    document.getElementsByTagName('head')[0].appendChild(firstTilePopOverStyle);
  };
  handlePopOverOpen = () => {
    this.setState({
      highlightedPoolId: true,
    });
  };
  handlePopOverClose = () => {
    this.setState({
      highlightedPoolId: false,
    });
  };
  getPendingDelegatedStakePoolId = (
    epochNumber: number,
    fallbackStakePoolId: string | null | undefined
  ): string | null | undefined => {
    const {
      wallet: { pendingDelegations },
    } = this.props;

    if (!pendingDelegations || !pendingDelegations.length) {
      return fallbackStakePoolId;
    }

    const foundDelegation = pendingDelegations.find(
      (delegation: WalletNextDelegation) =>
        get(delegation, ['changes_at', 'epoch_number'], 0) === epochNumber
    );

    if (!foundDelegation) {
      return fallbackStakePoolId;
    }

    const isDelegating =
      get(foundDelegation, 'status') === WalletDelegationStatuses.DELEGATING;

    if (!isDelegating) {
      return null;
    }

    return get(foundDelegation, 'target', null);
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
        isHardwareWallet,
        id,
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
    } = this.props;
    const { highlightedPoolId } = this.state;
    // @TODO - remove once quit stake pool delegation is connected with rewards balance
    const isUndelegateBlocked = true;
    const syncingProgress = get(syncState, 'progress.quantity', '');
    const notDelegatedText = intl.formatMessage(messages.notDelegated);
    const removeDelegationText = intl.formatMessage(messages.removeDelegation);
    const delegateText = intl.formatMessage(messages.delegate);
    const redelegateText = intl.formatMessage(messages.redelegate);
    const nextPendingDelegatedStakePoolId = this.getPendingDelegatedStakePoolId(
      nextEpochNumber,
      delegatedStakePoolId
    );
    const nextPendingDelegatedStakePool = nextPendingDelegatedStakePoolId
      ? getStakePoolById(nextPendingDelegatedStakePoolId)
      : null;
    const futurePendingDelegatedStakePoolId = this.getPendingDelegatedStakePoolId(
      futureEpochNumber,
      nextPendingDelegatedStakePoolId
    );
    const futurePendingDelegatedStakePool = futurePendingDelegatedStakePoolId
      ? getStakePoolById(futurePendingDelegatedStakePoolId)
      : null;
    const stakePoolRankingColor = futurePendingDelegatedStakePool
      ? getColorFromRange(
          futurePendingDelegatedStakePool.ranking,
          numberOfRankedStakePools
        )
      : '';
    const saturationStyles = classnames([
      styles.saturationBar,
      futurePendingDelegatedStakePool
        ? styles[getSaturationColor(futurePendingDelegatedStakePool.saturation)]
        : null,
    ]);
    const futureStakePoolTileStyles = classnames([
      styles.stakePoolTile,
      highlightedPoolId ? styles.active : null,
      futurePendingDelegatedStakePoolId && futurePendingDelegatedStakePool
        ? styles.futureStakePoolTileDelegated
        : styles.futureStakePoolTileUndelegated,
      futurePendingDelegatedStakePoolId && !futurePendingDelegatedStakePool
        ? styles.futureStakePoolTileUndefined
        : null,
    ]);
    const rightContainerStyles = classnames([
      styles.right,
      isRestoring ? styles.isRestoring : null,
    ]);
    const actionButtonStyles = classnames([
      styles.action,
      highlightedPoolId ? styles.active : null,
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
              <WalletAmount
                walletAmount={messages.walletAmount}
                amount={amount.toFormat(DECIMAL_PLACES_IN_ADA)}
              />
            ) : (
              '-'
            )}
          </div>
        </div>

        <div className={rightContainerStyles}>
          {!isRestoring ? (
            <Fragment>
              {delegatedStakePoolId ? (
                <PopOver
                  themeOverrides={popOverThemeOverrides}
                  className={`wallet-row-${id}`}
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
                    className={styles.stakePoolTile}
                    ref={this.stakePoolFirstTileRef}
                  >
                    <div
                      className={!delegatedStakePool ? styles.unknown : null}
                    >
                      {delegatedStakePool ? (
                        <div className={styles.stakePoolName}>
                          <div
                            className={styles.activeAdaSymbol}
                            ref={this.stakePoolAdaSymbolRef}
                          >
                            <SVGInline svg={adaIcon} />
                          </div>
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
                  </div>
                </PopOver>
              ) : (
                <div className={styles.stakePoolTile}>
                  <div className={styles.nonDelegatedText}>
                    {notDelegatedText}
                  </div>
                </div>
              )}
              <SVGInline svg={arrow} className={styles.arrow} />
              <div className={styles.stakePoolTile}>
                {nextPendingDelegatedStakePoolId ? (
                  <div
                    className={
                      !nextPendingDelegatedStakePool ? styles.unknown : null
                    }
                  >
                    {nextPendingDelegatedStakePool ? (
                      <div className={styles.stakePoolTicker}>
                        {nextPendingDelegatedStakePool.ticker}
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
                {futurePendingDelegatedStakePoolId ? (
                  <>
                    {futurePendingDelegatedStakePool ? (
                      <PoolPopOver
                        openOnHover
                        color={stakePoolRankingColor}
                        currentTheme={currentTheme}
                        onClose={this.handlePopOverClose}
                        onOpen={this.handlePopOverOpen}
                        onOpenExternalLink={onOpenExternalLink}
                        openWithDelay={false}
                        stakePool={futurePendingDelegatedStakePool}
                        containerClassName={containerClassName}
                        numberOfRankedStakePools={numberOfRankedStakePools}
                        showWithSelectButton={showWithSelectButton}
                      >
                        <div className={styles.stakePoolTicker}>
                          {futurePendingDelegatedStakePool.ticker}
                        </div>
                        {IS_RANKING_DATA_AVAILABLE ? (
                          <div
                            className={styles.ranking}
                            style={{
                              color: stakePoolRankingColor,
                            }}
                          >
                            {futurePendingDelegatedStakePool.nonMyopicMemberRewards ? (
                              futurePendingDelegatedStakePool.ranking
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
                                  futurePendingDelegatedStakePool.saturation
                                ).toFixed(2)}%`,
                              }}
                            />
                          </div>
                        )}
                        <div
                          className={styles.stakePoolRankingIndicator}
                          style={{
                            background: stakePoolRankingColor,
                          }}
                        />
                      </PoolPopOver>
                    ) : (
                      <div className={styles.stakePoolUnknown}>
                        {intl.formatMessage(messages.unknownStakePoolLabel)}
                      </div>
                    )}
                  </>
                ) : (
                  <div className={styles.nonDelegatedText}>
                    {notDelegatedText}
                  </div>
                )}
                {futurePendingDelegatedStakePoolId && !isUndelegateBlocked && (
                  <div
                    className={actionButtonStyles}
                    role="presentation"
                    onClick={onUndelegate}
                    key="undelegate"
                  >
                    {removeDelegationText}
                  </div>
                )}
                <div
                  className={actionButtonStyles}
                  role="presentation"
                  onClick={onDelegate}
                >
                  {!futurePendingDelegatedStakePoolId
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
}

export default WalletRow;
