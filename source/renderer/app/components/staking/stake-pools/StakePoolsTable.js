// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { debounce, get, map, orderBy } from 'lodash';
import classNames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import moment from 'moment';
import BigNumber from 'bignumber.js';
import styles from './StakePoolsTable.scss';
import StakePool from '../../../domains/StakePool';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import BorderedBox from '../../widgets/BorderedBox';
import sortIcon from '../../../assets/images/ascending.inline.svg';
import {
  formattedLovelaceToAmount,
  formattedWalletAmount,
  shortNumber,
} from '../../../utils/formatters';
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
import TooltipPool from '../widgets/TooltipPool';
import { getRelativePosition } from '../../../utils/domManipulation';
import globalMessages from '../../../i18n/global-messages';

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

const defaultTableOrdering = {
  ranking: 'asc',
  ticker: 'asc',
  saturation: 'asc',
  cost: 'asc',
  profitMargin: 'asc',
  producedBlocks: 'desc',
  potentialRewards: 'desc',
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
  highlightedPoolId?: ?number,
  isPreloading: boolean,
  stakePoolsOrder: string,
  stakePoolsSortBy: string,
  top: number,
  left: number,
  isFixedTableHeaderActive: boolean,
  isFixedSearchBarActive: boolean,
  fixedTableHeaderPosition: number,
  fixedSearchBarPosition: number,
  selectedRow: number | null,
};

const initialState = {
  highlightedPoolId: null,
  isPreloading: true,
  stakePoolsOrder: 'asc',
  stakePoolsSortBy: 'ranking',
  top: 0,
  left: 0,
  isFixedTableHeaderActive: false,
  isFixedSearchBarActive: false,
  fixedTableHeaderPosition: 250,
  fixedSearchBarPosition: 186,
  selectedRow: null,
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

  constructor(props: Props) {
    super(props);
    window.addEventListener('resize', this.handleResize);
  }

  state = {
    ...initialState,
  };

  scrollableDomElement: ?HTMLElement = null;

  searchInput: ?HTMLElement = null;

  _isMounted = false;

  scrollableDomElement: ?HTMLElement = null;

  searchInput: ?HTMLElement = null;

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

  handleResize = () =>
    debounce(this.handleCloseTooltip, 200, { leading: true, trailing: false });

  handleOpenTooltip = (poolId: SyntheticMouseEvent<HTMLElement>) => {
    const {
      isListActive,
      setListActive,
      listName,
      stakePoolsList,
      containerClassName,
    } = this.props;
    const { stakePoolsSortBy, stakePoolsOrder } = this.state;
    if (poolId.target) {
      poolId.persist();
      const targetElement = poolId.target;
      if (targetElement instanceof HTMLElement) {
        const { top, left } = getRelativePosition(
          targetElement,
          `.${containerClassName}`
        );
        this.setState({ top, left });
        const parentEl = targetElement.parentElement;
        if (parentEl && parentEl.parentElement) {
          const index = get(parentEl.parentElement, 'sectionRowIndex', null);
          this.setState({ selectedRow: index });
        }
      }
    }
    if (isListActive === false && setListActive) setListActive(listName);
    const sortedStakePoolList = orderBy(
      stakePoolsList,
      stakePoolsSortBy,
      stakePoolsOrder
    );
    const targetEl = poolId.currentTarget;
    const { parentElement } = targetEl;
    if (parentElement) {
      const currentTargetChildren = get(
        parentElement.parentElement,
        'sectionRowIndex',
        null
      );
      const highlightedPoolId = sortedStakePoolList[currentTargetChildren]
        ? sortedStakePoolList[currentTargetChildren].id
        : null;
      return this.setState({
        highlightedPoolId,
      });
    }
    return null;
  };

  bigNumbersToFormattedNumbers = (
    value: BigNumber,
    shorterNumber?: boolean
  ) => {
    const formattedValue = formattedWalletAmount(value, false, !shorterNumber);
    const splitValues = formattedValue.split(',');
    let result = '';
    splitValues.map((item) => {
      result += item;
      return true;
    });
    return result;
  };

  handleCloseTooltip = (item: SyntheticMouseEvent<HTMLElement>) => {
    const { isListActive, setListActive } = this.props;
    let selectedRow = null;
    if (item) {
      const { target } = item;
      const parent = get(target, 'parentElement', null);
      selectedRow = get(parent, 'sectionRowIndex', null);
    }
    this.setState({
      ...initialState,
      selectedRow,
      isPreloading: false,
    });
    if (isListActive !== false && setListActive) setListActive(null);
  };

  handleSelect = (stakePoolId: number) => {
    const { onSelect } = this.props;
    const selectedPoolId =
      this.props.selectedPoolId === stakePoolId ? null : stakePoolId;
    if (onSelect) {
      onSelect(selectedPoolId);
    }
  };

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

  getIsHighlighted = (id: string) =>
    this.props.isListActive !== false && id === this.state.highlightedPoolId;

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
    } = this.props;
    const {
      isPreloading,
      stakePoolsSortBy,
      stakePoolsOrder,
      isFixedTableHeaderActive,
    } = this.state;
    const { intl } = this.context;
    const componentClasses = classNames([styles.component, listName]);

    if (stakePoolsList.length > PRELOADER_THRESHOLD && isPreloading)
      return (
        <div className={styles.preloadingBlockWrapper}>
          <LoadingSpinner big />
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
          const formattedPledgeValue = this.bigNumbersToFormattedNumbers(
            stakePool.pledge
          );
          calculatedPledge = Number(
            parseFloat(formattedPledgeValue).toFixed(2)
          );
        }
        if (stakePoolsSortBy === 'cost') {
          const formattedCostValue = this.bigNumbersToFormattedNumbers(
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
        name: 'potentialRewards',
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
          <BorderedBox className={styles.boxedContainer}>
            {sortedStakePoolList.length > 0 && (
              <table>
                <thead className={tableHeaderClasses}>
                  <tr>
                    {map(availableTableHeaders, (tableHeader) => {
                      const isSorted =
                        tableHeader.name === stakePoolsSortBy ||
                        (tableHeader.name === 'ticker' &&
                          stakePoolsSortBy === 'ticker');
                      const defaultOrdering =
                        defaultTableOrdering[tableHeader.name];
                      const sortIconClasses = classNames([
                        styles.sortIcon,
                        isSorted ? styles.sorted : null,
                        isSorted && stakePoolsOrder === 'asc'
                          ? styles.ascending
                          : null,
                        isSorted && styles[`${stakePoolsOrder}CurrentOrdering`],
                        styles[`${defaultOrdering}DefaultOrdering`],
                      ]);

                      return (
                        <th
                          key={tableHeader.name}
                          onClick={() => this.handleSort(tableHeader.name)}
                        >
                          {tableHeader.title}
                          <SVGInline
                            svg={sortIcon}
                            className={sortIconClasses}
                          />
                        </th>
                      );
                    })}
                  </tr>
                </thead>
                <tbody>
                  {map(sortedStakePoolList, (stakePool, key) => {
                    const rank = get(stakePool, 'ranking', '');
                    const ticker = get(stakePool, 'ticker', '');
                    const saturation = get(stakePool, 'saturation', '');
                    const cost = new BigNumber(get(stakePool, 'cost', ''));
                    const margin = get(stakePool, 'profitMargin', '');
                    const producedBlocks = get(stakePool, 'producedBlocks', '');
                    const pledge = new BigNumber(get(stakePool, 'pledge', ''));
                    const retiring = get(stakePool, 'retiring', '');
                    const memberRewards = get(
                      stakePool,
                      'nonMyopicMemberRewards',
                      ''
                    );
                    const potentialRewards = memberRewards
                      ? `${shortNumber(
                          formattedLovelaceToAmount(memberRewards)
                        )} ${intl.formatMessage(globalMessages.unitAda)}`
                      : '-';
                    const isOversaturated = saturation / 100 >= 1;
                    const saturationValue =
                      isOversaturated || !saturation
                        ? parseInt(saturation, 10)
                        : parseInt(saturation, 10);
                    const calculatedDateRange = moment(retiring).diff(
                      moment(),
                      'days'
                    );

                    const pledgeValue = this.bigNumbersToFormattedNumbers(
                      pledge,
                      true
                    );
                    const pledgeCalculatedValue = Number(pledgeValue)
                      ? Number(pledgeValue).toFixed(2)
                      : pledgeValue;
                    const costValue = this.bigNumbersToFormattedNumbers(cost);

                    const saturationBarClassnames = classNames([
                      styles.progress,
                      styles[getSaturationColor(saturation)],
                    ]);

                    const isHighlighted = this.getIsHighlighted(stakePool.id);
                    const color = getColorFromRange(
                      rank,
                      numberOfRankedStakePools
                    );
                    const { top, left, selectedRow } = this.state;

                    return (
                      <tr
                        key={key}
                        className={
                          selectedRow && selectedRow === key
                            ? styles.selected
                            : null
                        }
                      >
                        <td>
                          {rank}
                          {isHighlighted && (
                            <TooltipPool
                              stakePool={stakePool}
                              isVisible
                              onClick={this.handleCloseTooltip}
                              currentTheme={currentTheme}
                              onOpenExternalLink={onOpenExternalLink}
                              top={top}
                              left={left}
                              fromStakePool
                              color={color}
                              onSelect={this.handleSelect}
                              showWithSelectButton={showWithSelectButton}
                              containerClassName={containerClassName}
                              numberOfRankedStakePools={
                                numberOfRankedStakePools
                              }
                            />
                          )}
                        </td>
                        <td>
                          <span
                            className={styles.ticker}
                            role="presentation"
                            onClick={this.handleOpenTooltip}
                          >
                            {ticker}
                          </span>
                        </td>
                        <td>
                          <div className={styles.currentEpochProgressBar}>
                            <div className={styles.progressBarContainer}>
                              <div
                                className={saturationBarClassnames}
                                style={{ width: `${saturationValue}%` }}
                              />
                              <div className={styles.progressLabel}>
                                {saturationValue}%
                              </div>
                            </div>
                          </div>
                        </td>
                        <td>{Number(costValue).toFixed(2)}</td>
                        <td>{margin}%</td>
                        <td>{producedBlocks}</td>
                        <td>{potentialRewards}</td>
                        <td>{pledgeCalculatedValue}</td>
                        <td>
                          {retiring && calculatedDateRange ? (
                            <span className={styles.retiring}>
                              {calculatedDateRange === 1
                                ? `${calculatedDateRange} day`
                                : `${calculatedDateRange} days`}
                            </span>
                          ) : (
                            <span>-</span>
                          )}
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            )}
          </BorderedBox>
        </div>
      </div>
    );
  }
}
