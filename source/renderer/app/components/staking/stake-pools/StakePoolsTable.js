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
import { getSaturationColor } from '../../../utils/colors';

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
    description: 'Table header "Saturation" label on stake pools list view page',
  },
  tableHeaderPerformance: {
    id: 'staking.stakePools.tableHeader.performance',
    defaultMessage: '!!!Performance',
    description: 'Table header "Performance" label on stake pools list view page',
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
    description: 'Table header "Produced Blocks" label on stake pools list view page',
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
};

type State = {
  highlightedPoolId?: ?number,
  isPreloading: boolean,
  stakePoolsOrder: string,
  stakePoolsSortBy: string,
};

const initialState = {
  highlightedPoolId: null,
  isPreloading: true,
  stakePoolsOrder: 'asc',
  stakePoolsSortBy: 'ranking',
};

@observer
export class StakePoolsTable extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
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

  handleClose = () => {
    this.setState({
      ...initialState,
      isPreloading: false,
    });
  };

  render() {
    const {
      stakePoolsList,
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

    const sortedStakePoolList = orderBy(stakePoolsList, stakePoolsSortBy, stakePoolsOrder);

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
                      const isSorted = tableHeader.name === stakePoolsSortBy;
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
                          onClick={() =>
                            this.handleSort(tableHeader.name)
                          }
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
                    const cost = get(stakePool, 'cost', '');
                    const margin = get(stakePool, 'profitMargin', '');
                    const producedBlocks = get(stakePool, 'producedBlocks', '');
                    const pledge = new BigNumber(get(stakePool, 'pledge', ''));
                    const retiring = get(stakePool, 'retiring', '');
                    const isOversaturated = (saturation / 100) >= 1;
                    const saturationValue = (isOversaturated || !saturation) ? parseInt(saturation, 10) : parseFloat(saturation).toFixed(2);
                    const calculatedDateRange = moment().diff(moment(retiring), 'days');

                    const saturationBarClassnames = classNames([
                      styles.progress,
                      styles[getSaturationColor(saturation)],
                    ]);

                    return (
                      <tr key={key}>
                        <td>{rank}</td>
                        <td><span className={styles.ticker}>[{ticker}]</span> {description}</td>
                        <td>
                          <div className={styles.currentEpochProgressBar}>
                            <div className={styles.progressBarContainer}>
                              <div
                                className={saturationBarClassnames}
                                style={{width: `${saturationValue}%`}}
                               />
                              <div className={styles.progressLabel}>{saturationValue}%</div>
                            </div>
                          </div>
                        </td>
                        <td>{`${formattedWalletAmount(cost, false, false)}`}</td>
                        <td>{margin}%</td>
                        <td>{producedBlocks}</td>
                        <td>{`${formattedWalletAmount(pledge, false, true)}`}</td>
                        <td>
                          {retiring ? (
                            <span className={styles.retiring}>
                              {`${calculatedDateRange} days`}
                            </span>
                          ) : (<span>-</span>)}
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
    if (newSortBy === 'name') {
      newSortBy = 'ticker';
    }
    this.setState({
      stakePoolsOrder: newOrder,
      stakePoolsSortBy: newSortBy,
    });
  };
}
