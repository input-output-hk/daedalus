// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { orderBy } from 'lodash';
import classNames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import styles from './StakePoolsTable.scss';
import StakePool from '../../../domains/StakePool';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import BorderedBox from '../../widgets/BorderedBox';
import {
  bigNumbersToFormattedNumbers,
} from '../../../utils/formatters';
import globalMessages from '../../../i18n/global-messages';
import { StakePoolsTableHeader } from './StakePoolsTableHeader';
import { StakePoolsTableBody } from './StakePoolsTableBody';

const messages = defineMessages({
  tableHeaderRank: {
    id: 'staking.stakePools.tableHeader.rank',
    defaultMessage: '!!!Rank',
    description: 'Table header "Rank" label on stake pools list view page',
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
  tableHeaderProducedBlocks: {
    id: 'staking.stakePools.tableHeader.producedBlocks',
    defaultMessage: '!!!Produced Blocks',
    description:
      'Table header "Produced Blocks" label on stake pools list view page',
  },
  tableHeaderPotentialRewards: {
    id: 'staking.stakePools.tableHeader.potentialRewards',
    defaultMessage: '!!!Potential rewards',
    description:
      'Table header "Potential rewards" label on stake pools list view page',
  },
  tableHeaderPledge: {
    id: 'staking.stakePools.tableHeader.pledge',
    defaultMessage: '!!!Pledge',
    description: 'Table header "Pledge" label on stake pools list view page',
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
  maintainFixed?: boolean,
  isScrolled?: boolean,
};

type State = {
  isPreloading: boolean,
  stakePoolsOrder: string,
  stakePoolsSortBy: string,
  isFixedTableHeaderActive: boolean,
  isFixedSearchBarActive: boolean,
  fixedTableHeaderPosition: number,
  fixedSearchBarPosition: number,
};

const initialState = {
  isPreloading: true,
  stakePoolsOrder: 'asc',
  stakePoolsSortBy: 'ranking',
  isFixedTableHeaderActive: false,
  isFixedSearchBarActive: false,
  fixedTableHeaderPosition: 250,
  fixedSearchBarPosition: 186,
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

  state = {
    ...initialState,
  };

  scrollableDomElement: ?HTMLElement = null;

  searchInput: ?HTMLElement = null;

  _isMounted = false;

  componentDidMount() {
    this._isMounted = true;
    setTimeout(() => {
      if (this._isMounted) this.setState({isPreloading: false});
    }, 0);
  }

  componentWillUnmount() {
    this._isMounted = false;
    this.scrollableDomElement = document.querySelector(
      `.${this.props.containerClassName}`
    );
  }

  handleSort = (newSortBy: string) => {
    const {stakePoolsOrder, stakePoolsSortBy} = this.state;
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
      maintainFixed,
      isScrolled,
      onSelect,
      selectedPoolId,
      setListActive,
      isListActive,
    } = this.props;
    const {
      isPreloading,
      stakePoolsSortBy,
      stakePoolsOrder,
      isFixedTableHeaderActive,
    } = this.state;
    const {intl} = this.context;
    const componentClasses = classNames([styles.component, listName]);

    if (stakePoolsList.length > PRELOADER_THRESHOLD && isPreloading)
      return (
        <div className={styles.preloadingBlockWrapper}>
          <LoadingSpinner big/>
        </div>
      );
    let tableHeaderClasses: string = '';
    if (isScrolled) {
      tableHeaderClasses = classNames([
        styles.tableHeader,
        isScrolled && isFixedTableHeaderActive ? styles.fixedTableHeader : null,
      ]);
    } else {
      tableHeaderClasses = classNames([
        styles.tableHeader,
        isScrolled && (isFixedTableHeaderActive || maintainFixed)
          ? styles.fixedTableHeader
          : null,
      ]);
    }

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
          const formattedPledgeValue = bigNumbersToFormattedNumbers(
            stakePool.pledge
          );
          calculatedPledge = Number(
            parseFloat(formattedPledgeValue).toFixed(2)
          );
        }
        if (stakePoolsSortBy === 'cost') {
          const formattedCostValue = bigNumbersToFormattedNumbers(
            stakePool.cost
          );
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
        title: intl.formatMessage(messages.tableHeaderRank),
      },
      {
        name: 'ticker',
        title: intl.formatMessage(messages.tableHeaderTicker),
      },
      {
        name: 'saturation',
        title: intl.formatMessage(messages.tableHeaderSaturation),
      },
      {
        name: 'cost',
        title: intl.formatMessage(messages.tableHeaderCost),
      },
      {
        name: 'profitMargin',
        title: intl.formatMessage(messages.tableHeaderMargin),
      },
      {
        name: 'producedBlocks',
        title: intl.formatMessage(messages.tableHeaderProducedBlocks),
      },
      {
        name: 'nonMyopicMemberRewards',
        title: intl.formatMessage(messages.tableHeaderPotentialRewards),
      },
      {
        name: 'pledge',
        title: intl.formatMessage(messages.tableHeaderPledge),
      },
      {
        name: 'retiring',
        title: intl.formatMessage(messages.tableHeaderRetiring),
      },
    ];

    return (
      <div>
        <div className={componentClasses}>
          <BorderedBox>
            {sortedStakePoolList.length > 0 && (
              <table>
                <thead className={tableHeaderClasses}>
                <tr>
                  <StakePoolsTableHeader
                    availableTableHeaders={availableTableHeaders}
                    stakePoolsSortBy={stakePoolsSortBy}
                    stakePoolsOrder={stakePoolsOrder}
                    onHandleSort={this.handleSort}
                  />
                </tr>
                </thead>
                <tbody>
                <StakePoolsTableBody
                  sortedStakePoolList={sortedStakePoolList}
                  ada={intl.formatMessage(globalMessages.unitAda)}
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
            )}
          </BorderedBox>
        </div>
      </div>
    );
  }
}
