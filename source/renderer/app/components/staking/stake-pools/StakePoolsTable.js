// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { orderBy } from 'lodash';
import classNames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import styles from './StakePoolsTable.scss';
import StakePool from '../../../domains/StakePool';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import BorderedBox from '../../widgets/BorderedBox';
import { StakePoolsTableHeader } from './StakePoolsTableHeader';
import { StakePoolsTableBody } from './StakePoolsTableBody';

const messages = defineMessages({
  tableHeaderRank: {
    id: 'staking.stakePools.tableHeader.rank',
    defaultMessage: '!!!Rank',
    description: 'Table header "Rank" label on stake pools list view page',
  },
  tableHeaderRankTooltip: {
    id: 'staking.stakePools.tooltip.rankingTooltip',
    defaultMessage:
      '!!!A hierarchical ranking based on the potential rewards you will earn if you delegate the intended amount of stake to this pool, assuming that it reaches saturation.',
    description: '"Rank" tooltip for the Stake Pools Table.',
  },
  tableHeaderTicker: {
    id: 'staking.stakePools.tableHeader.ticker',
    defaultMessage: '!!!Ticker',
    description: 'Table header "Ticker" label on stake pools list view page',
  },
  tableHeaderSaturation: {
    id: 'staking.stakePools.tableHeader.saturation',
    defaultMessage: '!!!Saturation',
    description:
      'Table header "Saturation" label on stake pools list view page',
  },
  tableHeaderSaturationTooltip: {
    id: 'staking.stakePools.tooltip.saturationTooltip',
    defaultMessage:
      '!!!Saturation measures the stake in the pool and indicates the point at which rewards stop increasing with increases in stake. This capping mechanism encourages decentralization by discouraging users from delegating to oversaturated stake pools.',
    description: '"Saturation" tooltip for the Stake Pools Table.',
  },
  tableHeaderPerformance: {
    id: 'staking.stakePools.tableHeader.performance',
    defaultMessage: '!!!Performance',
    description:
      'Table header "Performance" label on stake pools list view page',
  },
  tableHeaderUptime: {
    id: 'staking.stakePools.tableHeader.uptime',
    defaultMessage: '!!!Uptime (days)',
    description: 'Table header "Uptime" label on stake pools list view page',
  },
  tableHeaderMargin: {
    id: 'staking.stakePools.tableHeader.margin',
    defaultMessage: '!!!Margin',
    description: 'Table header "Margin" label on stake pools list view page',
  },
  tableHeaderMarginTooltip: {
    id: 'staking.stakePools.tooltip.profitMarginTooltip',
    defaultMessage:
      "!!!The pool's profit, defined as the rewards percentage kept by the pool from the stake that was delegated to it.",
    description: '"Pool margin" tooltip for the Stake Pools Table.',
  },
  tableHeaderRoi: {
    id: 'staking.stakePools.tableHeader.roi',
    defaultMessage: '!!!Roi',
    description: 'Table header "Roi" label on stake pools list view page',
  },
  tableHeaderCost: {
    id: 'staking.stakePools.tableHeader.cost',
    defaultMessage: '!!!Cost (ADA)',
    description: 'Table header "Cost" label on stake pools list view page',
  },
  tableHeaderCostTooltip: {
    id: 'staking.stakePools.tooltip.costPerEpochTooltip',
    defaultMessage:
      '!!!Fixed operational costs that the stake pool retains from any rewards earned during each epoch.',
    description: '"Cost per epoch" tooltip for the Stake Pools Table.',
  },
  tableHeaderProducedBlocks: {
    id: 'staking.stakePools.tableHeader.producedBlocks',
    defaultMessage: '!!!Produced Blocks',
    description:
      'Table header "Produced Blocks" label on stake pools list view page',
  },
  tableHeaderProducedBlocksTooltip: {
    id: 'staking.stakePools.tooltip.producedBlocksTooltip',
    defaultMessage:
      '!!!The total number of blocks the stake pool has produced.',
    description: '"Blocks" tooltip for the Stake Pools Table.',
  },
  tableHeaderPotentialRewards: {
    id: 'staking.stakePools.tableHeader.potentialRewards',
    defaultMessage: '!!!Potential rewards',
    description:
      'Table header "Potential rewards" label on stake pools list view page',
  },
  tableHeaderPotentialRewardsTooltip: {
    id: 'staking.stakePools.tooltip.potentialRewardsTooltip',
    defaultMessage:
      "!!!An estimation of the potential rewards you will earn per epoch if you delegate the intended amount of stake. The system looks at the pool's parameters and historical performance data to calculate potential rewards, assuming that the pool reaches optimal saturation.",
    description: '"Rewards" tooltip for the Stake Pools Table.',
  },
  tableHeaderPledge: {
    id: 'staking.stakePools.tableHeader.pledge',
    defaultMessage: '!!!Pledge (ADA)',
    description: 'Table header "Pledge" label on stake pools list view page',
  },
  tableHeaderPledgeTooltip: {
    id: 'staking.stakePools.tooltip.pledgeTooltip',
    defaultMessage:
      '!!!The amount of stake that a pool operator contributes to a pool. Pools with higher pledge amounts earn more rewards for themselves and their delegators. Pools that do not honor their pledge earn zero rewards and accrue low ranking.',
    description: '"Pledge" tooltip for the Stake Pools Table.',
  },
  tableHeaderRetiring: {
    id: 'staking.stakePools.tableHeader.retiring',
    defaultMessage: '!!!Retiring in',
    description: 'Table header "Retiring" label on stake pools list view page',
  },
});

export const defaultTableOrdering = {
  ranking: 'asc',
  ticker: 'asc',
  saturation: 'asc',
  cost: 'asc',
  profitMargin: 'asc',
  producedBlocks: 'desc',
  nonMyopicMemberRewards: 'desc',
  pledge: 'asc',
  retiring: 'asc',
};

// Maximum number of stake pools for which we do not need to use the preloading
const PRELOADER_THRESHOLD = 100;

type Props = {
  stakePoolsList: Array<StakePool>,
  listName?: string,
  isListActive?: boolean,
  currentTheme: string,
  setListActive?: Function,
  showWithSelectButton?: boolean,
  onSelect?: Function,
  containerClassName: string,
  numberOfRankedStakePools: number,
  selectedPoolId?: ?number,
  onOpenExternalLink: Function,
  currentLocale: string,
  onTableHeaderMouseEnter: Function,
  onTableHeaderMouseLeave: Function,
};

type State = {
  isPreloading: boolean,
  stakePoolsOrder: string,
  stakePoolsSortBy: string,
};

const initialState = {
  isPreloading: true,
  stakePoolsOrder: 'asc',
  stakePoolsSortBy: 'ranking',
};

@observer
export class StakePoolsTable extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    isListActive: true,
    showWithSelectButton: false,
  };

  state = { ...initialState };

  scrollableDomElement: ?HTMLElement = null;

  searchInput: ?HTMLElement = null;

  _isMounted = false;

  componentDidMount() {
    this._isMounted = true;
    setTimeout(() => {
      if (this._isMounted) this.setState({ isPreloading: false });
    }, 0);
  }

  componentWillUnmount() {
    this._isMounted = false;
    this.scrollableDomElement = document.querySelector(
      `.${this.props.containerClassName}`
    );
  }

  handleSort = (newSortBy: string) => {
    const { stakePoolsOrder, stakePoolsSortBy } = this.state;
    let newOrder = defaultTableOrdering[newSortBy];
    if (newSortBy === stakePoolsSortBy) {
      newOrder = stakePoolsOrder === 'asc' ? 'desc' : 'asc';
    }
    this.setState({
      stakePoolsOrder: newOrder,
      stakePoolsSortBy: newSortBy,
    });
  };

  render() {
    const {
      currentTheme,
      onOpenExternalLink,
      showWithSelectButton,
      stakePoolsList,
      containerClassName,
      numberOfRankedStakePools,
      listName,
      onSelect,
      selectedPoolId,
      setListActive,
      isListActive,
      currentLocale,
      onTableHeaderMouseEnter,
      onTableHeaderMouseLeave,
    } = this.props;
    const { isPreloading, stakePoolsSortBy, stakePoolsOrder } = this.state;
    const { intl } = this.context;
    const componentClasses = classNames([styles.component, listName]);

    if (stakePoolsList.length > PRELOADER_THRESHOLD && isPreloading)
      return (
        <div className={styles.preloadingBlockWrapper}>
          <LoadingSpinner big />
        </div>
      );

    const sortedStakePoolList = orderBy(
      stakePoolsList.map((stakePool) => {
        let calculatedPledge;
        let calculatedCost;
        let formattedTicker;
        if (stakePoolsSortBy === 'ticker') {
          formattedTicker = stakePool.ticker
            .replace(/[^\w\s]/gi, '')
            .toLowerCase();
        }
        if (stakePoolsSortBy === 'pledge') {
          const formattedPledgeValue = stakePool.pledge.toFixed(2);
          calculatedPledge = Number(
            parseFloat(formattedPledgeValue).toFixed(2)
          );
        }
        if (stakePoolsSortBy === 'cost') {
          const formattedCostValue = stakePool.cost.toFixed(2);
          calculatedCost = Number(parseFloat(formattedCostValue).toFixed(2));
        }
        return {
          ...stakePool,
          calculatedPledge,
          calculatedCost,
          formattedTicker,
        };
      }),
      [
        'formattedTicker',
        'calculatedPledge',
        'calculatedCost',
        stakePoolsSortBy,
      ],
      [stakePoolsOrder, stakePoolsOrder, stakePoolsOrder, stakePoolsOrder]
    );

    const availableTableHeaders = [
      {
        name: 'ranking',
        title: (
          <Tooltip
            key="ranking"
            isOpeningUpward={false}
            skin={TooltipSkin}
            tip={intl.formatMessage(messages.tableHeaderRankTooltip)}
          >
            {intl.formatMessage(messages.tableHeaderRank)}
          </Tooltip>
        ),
      },
      {
        name: 'ticker',
        title: intl.formatMessage(messages.tableHeaderTicker),
      },
      {
        name: 'saturation',
        title: (
          <Tooltip
            key="saturation"
            isOpeningUpward={false}
            skin={TooltipSkin}
            tip={intl.formatMessage(messages.tableHeaderSaturationTooltip)}
          >
            {intl.formatMessage(messages.tableHeaderSaturation)}
          </Tooltip>
        ),
      },
      {
        name: 'cost',
        title: (
          <Tooltip
            key="cost"
            isOpeningUpward={false}
            skin={TooltipSkin}
            tip={intl.formatMessage(messages.tableHeaderCostTooltip)}
          >
            {intl.formatMessage(messages.tableHeaderCost)}
          </Tooltip>
        ),
      },
      {
        name: 'profitMargin',
        title: (
          <Tooltip
            key="profitMargin"
            isOpeningUpward={false}
            skin={TooltipSkin}
            tip={intl.formatMessage(messages.tableHeaderMarginTooltip)}
          >
            {intl.formatMessage(messages.tableHeaderMargin)}
          </Tooltip>
        ),
      },
      {
        name: 'producedBlocks',
        title: (
          <Tooltip
            key="producedBlocks"
            isOpeningUpward={false}
            skin={TooltipSkin}
            tip={intl.formatMessage(messages.tableHeaderProducedBlocksTooltip)}
          >
            {intl.formatMessage(messages.tableHeaderProducedBlocks)}
          </Tooltip>
        ),
      },
      {
        name: 'nonMyopicMemberRewards',
        title: (
          <Tooltip
            key="nonMyopicMemberRewards"
            isOpeningUpward={false}
            skin={TooltipSkin}
            tip={intl.formatMessage(
              messages.tableHeaderPotentialRewardsTooltip
            )}
          >
            {intl.formatMessage(messages.tableHeaderPotentialRewards)}
          </Tooltip>
        ),
      },
      {
        name: 'pledge',
        title: (
          <Tooltip
            key="pledge"
            isOpeningUpward={false}
            skin={TooltipSkin}
            tip={intl.formatMessage(messages.tableHeaderPledgeTooltip)}
          >
            {intl.formatMessage(messages.tableHeaderPledge)}
          </Tooltip>
        ),
      },
      {
        name: 'retiring',
        title: intl.formatMessage(messages.tableHeaderRetiring),
      },
    ];

    return (
      <div>
        <div className={componentClasses}>
          {sortedStakePoolList.length > 0 && (
            <BorderedBox>
              <table>
                <thead
                  onMouseEnter={onTableHeaderMouseEnter}
                  onMouseLeave={onTableHeaderMouseLeave}
                >
                  <tr>
                    <StakePoolsTableHeader
                      availableTableHeaders={availableTableHeaders}
                      stakePoolsSortBy={stakePoolsSortBy}
                      stakePoolsOrder={stakePoolsOrder}
                      onHandleSort={this.handleSort}
                    />
                  </tr>
                </thead>
                <tbody
                  className={
                    currentLocale === 'ja-JP' ? styles.japaneseHeader : null
                  }
                >
                  <StakePoolsTableBody
                    sortedStakePoolList={sortedStakePoolList}
                    numberOfRankedStakePools={numberOfRankedStakePools}
                    currentTheme={currentTheme}
                    onOpenExternalLink={onOpenExternalLink}
                    showWithSelectButton={showWithSelectButton}
                    containerClassName={containerClassName}
                    onSelect={onSelect}
                    selectedPoolId={selectedPoolId}
                    setListActive={setListActive}
                    isListActive={isListActive}
                    listName={listName}
                    stakePoolsList={stakePoolsList}
                    stakePoolsSortBy={stakePoolsSortBy}
                    stakePoolsOrder={stakePoolsOrder}
                  />
                </tbody>
              </table>
            </BorderedBox>
          )}
        </div>
      </div>
    );
  }
}
