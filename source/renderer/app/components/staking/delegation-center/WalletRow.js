// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import Wallet, { WalletDelegationStatuses } from '../../../domains/Wallet';
import type { WalletNextDelegation } from '../../../api/wallets/types';
import StakePool from '../../../domains/StakePool';
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
import adaIcon from '../../../assets/images/ada-symbol.inline.svg';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import { PoolPopOver } from '../widgets/PoolPopOver';
import styles from './WalletRow.scss';
import popOverThemeOverrides from './WalletRowPopOverOverrides.scss';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import arrow from '../../../assets/images/collapse-arrow.inline.svg';
import hardwareWalletsIcon from '../../../assets/images/hardware-wallet/connect-ic.inline.svg';
import clockIcon from '../../../assets/images/clock-corner.inline.svg';
import crossIcon from '../../../assets/images/cross-corner.inline.svg';
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
  pledgeNotMetPopOver: {
    id: 'staking.stakePools.tooltip.pledgeNotMet.popover',
    defaultMessage:
      '!!!This pool has not met its pledge requirements. This means that the pool will not produce blocks or generate rewards until the pledge is met.',
    description:
      '"pledgeNotMet" popover for the Delegation center body section.',
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
  nextEpochNumber: number,
  futureEpochNumber: number,
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
  highlightedPoolId: boolean,
};
const initialWalletRowState = {
  highlightedPoolId: false,
};
@observer
export default class WalletRow extends Component<Props, WalletRowState> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    ...initialWalletRowState,
  };

  stakePoolFirstTileRef: { current: null | HTMLDivElement };
  stakePoolAdaSymbolRef: { current: null | HTMLDivElement };

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
    fallbackStakePoolId: ?string
  ): ?string => {
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

  renderLeftContent = () => {
    const {
      wallet: { name, amount, isRestoring, isHardwareWallet },
    } = this.props;
    return (
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
    );
  };

  renderRestoringRightContent = () => {
    const { intl } = this.context;
    const { syncState } = this.props.wallet;
    const syncingProgress = get(syncState, 'progress.quantity', '');
    const containerStyles = classnames([styles.right, styles.isRestoring]);
    return (
      <div className={containerStyles}>
        <PopOver
          content={intl.formatMessage(messages.syncingTooltipLabel, {
            syncingProgress,
          })}
        >
          <LoadingSpinner medium />
        </PopOver>
      </div>
    );
  };

  renderInteractiveDelegatedTile = (stakePool: ?StakePool) => {
    const { intl } = this.context;
    const {
      numberOfRankedStakePools,
      currentTheme,
      onOpenExternalLink,
      showWithSelectButton,
      containerClassName,
    } = this.props;

    if (!stakePool) {
      return this.renderUnknownPoolTile();
    }

    const { pledgeNotMet, retiring } = stakePool;

    const stakePoolRankingColor = !pledgeNotMet
      ? getColorFromRange(stakePool.ranking, numberOfRankedStakePools)
      : 'null';

    const saturationStyles = classnames([
      styles.saturationBar,
      styles[getSaturationColor(stakePool.saturation)],
    ]);
    const stakePoolRankingIndicatorStyles = classnames([
      styles.stakePoolRankingIndicator,
      pledgeNotMet ? styles.pledgeNotMet : null,
    ]);

    return (
      <PoolPopOver
        openOnHover
        color={stakePoolRankingColor}
        currentTheme={currentTheme}
        onClose={this.handlePopOverClose}
        onOpen={this.handlePopOverOpen}
        onOpenExternalLink={onOpenExternalLink}
        openWithDelay={false}
        stakePool={stakePool}
        containerClassName={containerClassName}
        numberOfRankedStakePools={numberOfRankedStakePools}
        showWithSelectButton={showWithSelectButton}
      >
        <div className={styles.stakePoolTicker}>{stakePool.ticker}</div>
        {pledgeNotMet && (
          <div className={styles.cornerIcon}>
            <PopOver
              content={intl.formatMessage(messages.pledgeNotMetPopOver)}
              zIndex={10000}
            >
              <SVGInline svg={crossIcon} />
            </PopOver>
          </div>
        )}
        {!pledgeNotMet && retiring && (
          <div className={styles.cornerIcon}>
            <SVGInline svg={clockIcon} />
          </div>
        )}
        {IS_RANKING_DATA_AVAILABLE && !pledgeNotMet ? (
          <div
            className={styles.ranking}
            style={{ color: stakePoolRankingColor }}
          >
            {stakePool.ranking}
          </div>
        ) : (
          <div className={styles.noDataDash}>
            <SVGInline svg={noDataDashBigImage} />
          </div>
        )}
        {IS_SATURATION_DATA_AVAILABLE && !pledgeNotMet && (
          <div className={saturationStyles}>
            <span
              style={{
                width: `${parseFloat(stakePool.saturation).toFixed(2)}%`,
              }}
            />
          </div>
        )}
        <div
          className={stakePoolRankingIndicatorStyles}
          style={{ background: stakePoolRankingColor }}
        />
      </PoolPopOver>
    );
  };

  renderNonInteractiveDelegatedTile = (
    stakePool: ?StakePool,
    stakePoolFirstTileRef?: ?any,
    stakePoolAdaSymbolRef?: ?any
  ) => {
    return (
      <div className={styles.stakePoolTile} ref={stakePoolFirstTileRef}>
        <div className={!stakePool ? styles.unknown : null}>
          {stakePool ? (
            <div className={styles.stakePoolName}>
              <div
                className={styles.activeAdaSymbol}
                ref={stakePoolAdaSymbolRef}
              >
                <SVGInline svg={adaIcon} />
              </div>
              <div className={styles.stakePoolTicker}>{stakePool.ticker}</div>
            </div>
          ) : (
            this.renderUnknownPoolTile()
          )}
        </div>
      </div>
    );
  };

  renderUnknownPoolTile = () => (
    <div className={styles.stakePoolUnknown}>
      {this.context.intl.formatMessage(messages.unknownStakePoolLabel)}
    </div>
  );

  renderNotDelegatedTile = () => (
    <div className={styles.nonDelegatedText}>
      {this.context.intl.formatMessage(messages.notDelegated)}
    </div>
  );

  renderRightContent = () => {
    const { intl } = this.context;
    const {
      wallet: { isRestoring, delegatedStakePoolId, id },
      delegatedStakePool,
      getStakePoolById,
      onDelegate,
      onUndelegate,
      nextEpochNumber,
      futureEpochNumber,
    } = this.props;
    const { highlightedPoolId } = this.state;

    // @TODO - remove once quit stake pool delegation is connected with rewards balance
    const isUndelegateBlocked = true;

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

    const removeDelegationButtonStyles = classnames([
      styles.action,
      highlightedPoolId ? styles.active : null,
    ]);
    const delegateButtonStyles = classnames([
      styles.action,
      highlightedPoolId ? styles.active : null,
    ]);

    if (isRestoring) {
      return this.renderRestoringRightContent();
    }

    return (
      <div className={styles.right}>
        {/* LEFT COLUM - Current Epoch */}
        {delegatedStakePoolId ? (
          <PopOver
            themeOverrides={popOverThemeOverrides}
            className={`wallet-row-${id}`}
            content={
              <div className={styles.tooltipLabelWrapper}>
                <span>
                  {intl.formatMessage(messages.TooltipPoolTickerEarningRewards)}
                </span>
              </div>
            }
          >
            {this.renderNonInteractiveDelegatedTile(
              delegatedStakePool,
              this.stakePoolFirstTileRef,
              this.stakePoolAdaSymbolRef
            )}
          </PopOver>
        ) : (
          <div className={styles.stakePoolTile}>
            {this.renderNotDelegatedTile()}
          </div>
        )}
        <SVGInline svg={arrow} className={styles.arrow} />

        {/* MIDDLE COLUM - Next Epoch */}
        {delegatedStakePoolId ? (
          this.renderNonInteractiveDelegatedTile(nextPendingDelegatedStakePool)
        ) : (
          <div className={styles.stakePoolTile}>
            {this.renderNotDelegatedTile()}
          </div>
        )}
        <SVGInline svg={arrow} className={styles.arrow} />

        {/* MIDDLE COLUM - Future Epoch */}
        <div className={futureStakePoolTileStyles}>
          {futurePendingDelegatedStakePoolId
            ? this.renderInteractiveDelegatedTile(
                futurePendingDelegatedStakePool
              )
            : this.renderNotDelegatedTile()}
          {futurePendingDelegatedStakePoolId && !isUndelegateBlocked && (
            <div
              className={removeDelegationButtonStyles}
              role="presentation"
              onClick={onUndelegate}
              key="undelegate"
            >
              {removeDelegationText}
            </div>
          )}
          <div
            className={delegateButtonStyles}
            role="presentation"
            onClick={onDelegate}
          >
            {!futurePendingDelegatedStakePoolId ? delegateText : redelegateText}
          </div>
        </div>
      </div>
    );
  };

  render() {
    return (
      <div className={styles.component}>
        {this.renderLeftContent()}
        {this.renderRightContent()}
      </div>
    );
  }
}
