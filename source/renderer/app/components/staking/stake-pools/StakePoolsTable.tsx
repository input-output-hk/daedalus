import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { orderBy } from 'lodash';
import classNames from 'classnames';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakePoolsTable.scss' or its... Remove this comment to see the full error message
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
      '!!!<p>A hierarchical ranking based on the potential rewards you will earn if you delegate the intended amount of stake to this pool, assuming that it reaches saturation.</p><p>*Stake pools with the potential rewards estimated at zero have the same ranking. Please set the stake slider to a higher value for more pools to get potential rewards estimated at more than zero.</p>',
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
  stakePoolsList: Array<StakePool>;
  listName?: string;
  isListActive?: boolean;
  currentTheme: string;
  setListActive?: (...args: Array<any>) => any;
  showWithSelectButton?: boolean;
  onSelect?: (...args: Array<any>) => any;
  containerClassName: string;
  numberOfRankedStakePools: number;
  selectedPoolId?: number | null | undefined;
  onOpenExternalLink: (...args: Array<any>) => any;
  currentLocale: string;
  onTableHeaderMouseEnter: (...args: Array<any>) => any;
  onTableHeaderMouseLeave: (...args: Array<any>) => any;
};
type State = {
  isPreloading: boolean;
  stakePoolsOrder: string;
  stakePoolsSortBy: string;
};
const initialState = {
  isPreloading: true,
  stakePoolsOrder: 'asc',
  stakePoolsSortBy: 'ranking',
};

@observer
class StakePoolsTable extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  static defaultProps = {
    isListActive: true,
    showWithSelectButton: false,
  };
  state = { ...initialState };
  scrollableDomElement: HTMLElement | null | undefined = null;
  searchInput: HTMLElement | null | undefined = null;
  _isMounted = false;

  componentDidMount() {
    this._isMounted = true;
    setTimeout(() => {
      if (this._isMounted)
        this.setState({
          isPreloading: false,
        });
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
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      [stakePoolsOrder, stakePoolsOrder, stakePoolsOrder, stakePoolsOrder]
    );
    const availableTableHeaders = [
      {
        name: 'ranking',
        title: (
          <PopOver
            key="ranking"
            placement="bottom"
            content={
              <div className={styles.tooltipWithHTMLContent}>
                <FormattedHTMLMessage {...messages.tableHeaderRankTooltip} />
              </div>
            }
          >
            {intl.formatMessage(messages.tableHeaderRank)}
          </PopOver>
        ),
      },
      {
        name: 'ticker',
        title: intl.formatMessage(messages.tableHeaderTicker),
      },
      {
        name: 'saturation',
        title: (
          <PopOver
            key="saturation"
            placement="bottom"
            content={intl.formatMessage(messages.tableHeaderSaturationTooltip)}
          >
            {intl.formatMessage(messages.tableHeaderSaturation)}
          </PopOver>
        ),
      },
      {
        name: 'cost',
        title: (
          <PopOver
            key="cost"
            placement="bottom"
            content={intl.formatMessage(messages.tableHeaderCostTooltip)}
          >
            {intl.formatMessage(messages.tableHeaderCost)}
          </PopOver>
        ),
      },
      {
        name: 'profitMargin',
        title: (
          <PopOver
            key="profitMargin"
            placement="bottom"
            content={intl.formatMessage(messages.tableHeaderMarginTooltip)}
          >
            {intl.formatMessage(messages.tableHeaderMargin)}
          </PopOver>
        ),
      },
      {
        name: 'producedBlocks',
        title: (
          <PopOver
            key="producedBlocks"
            placement="bottom"
            content={intl.formatMessage(
              messages.tableHeaderProducedBlocksTooltip
            )}
          >
            {intl.formatMessage(messages.tableHeaderProducedBlocks)}
          </PopOver>
        ),
      },
      {
        name: 'nonMyopicMemberRewards',
        title: (
          <PopOver
            key="nonMyopicMemberRewards"
            placement="bottom"
            content={intl.formatMessage(
              messages.tableHeaderPotentialRewardsTooltip
            )}
          >
            {intl.formatMessage(messages.tableHeaderPotentialRewards)}
          </PopOver>
        ),
      },
      {
        name: 'pledge',
        title: (
          <PopOver
            key="pledge"
            placement="bottom"
            content={intl.formatMessage(messages.tableHeaderPledgeTooltip)}
          >
            {intl.formatMessage(messages.tableHeaderPledge)}
          </PopOver>
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
                    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
                    sortedStakePoolList={sortedStakePoolList}
                    numberOfRankedStakePools={numberOfRankedStakePools}
                    currentTheme={currentTheme}
                    onOpenExternalLink={onOpenExternalLink}
                    showWithSelectButton={showWithSelectButton}
                    containerClassName={containerClassName}
                    onSelect={onSelect}
                    selectedPoolId={selectedPoolId}
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

export { StakePoolsTable };
