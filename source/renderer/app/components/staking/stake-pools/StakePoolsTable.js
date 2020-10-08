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
import { StakingPageScrollContext } from '../layouts/StakingWithNavigation';
import BorderedBox from '../../widgets/BorderedBox';
import sortIcon from '../../../assets/images/ascending.inline.svg';
import { formattedWalletAmount } from '../../../utils/formatters';
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
import TooltipPool from '../widgets/TooltipPool';
import { getRelativePosition } from '../../../utils/domManipulation';

const messages = defineMessages({
  tableHeaderRank: {
    id: 'staking.stakePools.tableHeader.rank',
    defaultMessage: '!!!Rank',
    description: 'Table header "Rank" label on stake pools list view page',
  },
  tableHeaderName: {
    id: 'staking.stakePools.tableHeader.name',
    defaultMessage: '!!!Name',
    description: 'Table header "Name" label on stake pools list view page',
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
  numberOfStakePools: number,
  selectedPoolId?: ?number,
  onOpenExternalLink: Function,
  highlightOnHover?: boolean,
};

type State = {
  highlightedPoolId?: ?number,
  isPreloading: boolean,
  stakePoolsOrder: string,
  stakePoolsSortBy: string,
  top: number,
  left: number,
};

const initialState = {
  highlightedPoolId: null,
  isPreloading: true,
  stakePoolsOrder: 'asc',
  stakePoolsSortBy: 'ranking',
  top: 0,
  left: 0,
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

  _isMounted = false;

  componentDidMount() {
    this._isMounted = true;
    setTimeout(() => {
      if (this._isMounted) this.setState({ isPreloading: false });
    }, 0);
  }

  componentWillUnmount() {
    this._isMounted = false;
    window.removeEventListener('resize', this.handleClose);
  }

  handleResize = () =>
    debounce(this.handleClose, 200, { leading: true, trailing: false });

  searchInput: ?HTMLElement = null;

  handleOpenThumbnail = (poolId: SyntheticMouseEvent<HTMLElement>) => {
    const { isListActive, setListActive, listName, stakePoolsList, containerClassName } = this.props;
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
      }
    }
    if (isListActive === false && setListActive) setListActive(listName);
    const sortedStakePoolList = orderBy(
      stakePoolsList,
      stakePoolsSortBy,
      stakePoolsOrder
    );
    const currentTargetChildren = poolId.currentTarget.childNodes;
    const highlightedPoolId = currentTargetChildren.length && currentTargetChildren[0].innerText &&
    sortedStakePoolList[currentTargetChildren[0].innerText] ?
      sortedStakePoolList[currentTargetChildren[0].innerText].id : null;
    return this.setState({
      highlightedPoolId,
    });
  };

  handleClose = () => {
    this.setState({
      ...initialState,
      isPreloading: false,
    });
  };

  handleSelect = (stakePoolId: number) => {
    const { onSelect } = this.props;
    const selectedPoolId =
      this.props.selectedPoolId === stakePoolId ? null : stakePoolId;
    if (onSelect) {
      onSelect(selectedPoolId);
    }
  };

  getIsHighlighted = (id: string) =>
    this.props.isListActive !== false && id === this.state.highlightedPoolId;

  render() {
    const {
      currentTheme,
      highlightOnHover,
      onOpenExternalLink,
      showWithSelectButton,
      stakePoolsList,
      containerClassName,
      numberOfStakePools,
      listName,
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
      stakePoolsList,
      stakePoolsSortBy,
      stakePoolsOrder
    );

    const availableTableHeaders = [
      {
        name: 'ranking',
        title: intl.formatMessage(messages.tableHeaderRank),
      },
      {
        name: 'name',
        title: intl.formatMessage(messages.tableHeaderName),
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
        name: 'pledge',
        title: intl.formatMessage(messages.tableHeaderPledge),
      },
      {
        name: 'retiring',
        title: intl.formatMessage(messages.tableHeaderRetiring),
      },
    ];

    return (
      <StakingPageScrollContext.Consumer>
        {() => (
          <div className={componentClasses}>
            <BorderedBox>
              {sortedStakePoolList.length > 0 && (
                <table>
                  <thead>
                    <tr>
                      {map(availableTableHeaders, tableHeader => {
                        const isSorted =
                          tableHeader.name === stakePoolsSortBy ||
                          (tableHeader.name === 'name' &&
                            stakePoolsSortBy === 'ticker');
                        const sortIconClasses = classNames([
                          styles.sortIcon,
                          isSorted ? styles.sorted : null,
                          isSorted && stakePoolsOrder === 'asc'
                            ? styles.ascending
                            : null,
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
                      const description = get(stakePool, 'description', '');
                      const saturation = get(stakePool, 'saturation', '');
                      const cost = new BigNumber(get(stakePool, 'cost', ''));
                      const margin = get(stakePool, 'profitMargin', '');
                      const producedBlocks = get(
                        stakePool,
                        'producedBlocks',
                        ''
                      );
                      const pledge = new BigNumber(
                        get(stakePool, 'pledge', '')
                      );
                      const retiring = get(stakePool, 'retiring', '');
                      const isOversaturated = saturation / 100 >= 1;
                      const saturationValue =
                        isOversaturated || !saturation
                          ? parseInt(saturation, 10)
                          : parseFloat(saturation).toFixed(2);
                      const calculatedDateRange = moment(retiring).diff(
                        moment(),
                        'days'
                      );

                      const saturationBarClassnames = classNames([
                        styles.progress,
                        styles[getSaturationColor(saturation)],
                      ]);

                      const isHighlighted = this.getIsHighlighted(stakePool.id);
                      const color = getColorFromRange(rank, numberOfStakePools);
                      const { top, left } = this.state;

                      return (
                        <tr key={key} onClick={!highlightOnHover && this.handleOpenThumbnail}>
                          <td>
                            {rank}
                            {isHighlighted && (
                              <TooltipPool
                                stakePool={stakePool}
                                isVisible
                                onClick={this.handleClose}
                                currentTheme={currentTheme}
                                onOpenExternalLink={onOpenExternalLink}
                                top={top}
                                left={left}
                                bottom={20}
                                color={color}
                                onSelect={this.handleSelect}
                                showWithSelectButton={showWithSelectButton}
                                containerClassName={containerClassName}
                                numberOfStakePools={numberOfStakePools}
                              />
                            )}
                          </td>
                          <td>
                            <span className={styles.ticker}>[{ticker}]</span>{' '}
                            {description}
                          </td>
                          <td>
                            <div className={styles.currentEpochProgressBar}>
                              <div className={styles.progressBarContainer}>
                                <div
                                  className={saturationBarClassnames}
                                  style={{width: `${saturationValue}%`}}
                                />
                                <div className={styles.progressLabel}>
                                  {saturationValue}%
                                </div>
                              </div>
                            </div>
                          </td>
                          <td>{`${formattedWalletAmount(
                            cost,
                            false,
                            true
                          )}`}</td>
                          <td>{margin}%</td>
                          <td>{producedBlocks}</td>
                          <td>{`${formattedWalletAmount(
                            pledge,
                            false,
                            true
                          )}`}</td>
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
        )}
      </StakingPageScrollContext.Consumer>
    );
  }

  handleSort = (newSortBy: string) => {
    const { stakePoolsOrder, stakePoolsSortBy } = this.state;
    let newOrder;
    if (stakePoolsSortBy === newSortBy || newSortBy === 'name') {
      newOrder = stakePoolsOrder === 'asc' ? 'desc' : 'asc';
    } else {
      newOrder = 'desc';
    }
    if (stakePoolsSortBy !== 'ticker' && stakePoolsSortBy !== newSortBy) {
      newOrder = 'asc';
    }
    if (newSortBy === 'name') {
      newSortBy = 'ticker';
    }
    this.setState({
      stakePoolsOrder: newOrder,
      stakePoolsSortBy: newSortBy,
    });
  };
}
