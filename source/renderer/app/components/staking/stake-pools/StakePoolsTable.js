// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { debounce, get, map, orderBy } from 'lodash';
import classNames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import styles from './StakePoolsTable.scss';
import StakePool from '../../../domains/StakePool';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import { StakingPageScrollContext } from '../layouts/StakingWithNavigation';
import BorderedBox from '../../widgets/BorderedBox';
import sortIcon from '../../../assets/images/ascending.inline.svg';

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
});

// Maximum number of stake pools for which we do not need to use the preloading
const PRELOADER_THRESHOLD = 100;

type Props = {
  stakePoolsList: Array<StakePool>,
  onSelect?: Function,
  selectedPoolId?: ?number,
  listName?: string,
  isListActive?: boolean,
  setListActive?: Function,
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
  stakePoolsOrder: 'desc',
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

  getIsHighlighted = (id: string) =>
    this.props.isListActive !== false && id === this.state.highlightedPoolId;

  handleOpenThumbnail = (highlightedPoolId: number) => {
    const { isListActive, setListActive, listName } = this.props;
    if (isListActive === false && setListActive) setListActive(listName);
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
        name: 'performance',
        title: intl.formatMessage(messages.tableHeaderPerformance),
      },
      {
        name: 'uptime',
        title: intl.formatMessage(messages.tableHeaderUptime),
      },
      {
        name: 'profitMargin',
        title: intl.formatMessage(messages.tableHeaderMargin),
      },
      {
        name: 'roi',
        title: intl.formatMessage(messages.tableHeaderRoi),
      },
      {
        name: 'cost',
        title: intl.formatMessage(messages.tableHeaderCost),
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
                    const name = `[${ticker}] ${description}`;
                    const saturation = get(stakePool, 'saturation', '');
                    const performance = get(stakePool, 'performance', '');
                    const uptime = get(stakePool, 'uptime', '');
                    const margin = get(stakePool, 'profitMargin', '');
                    const roi = get(stakePool, 'roi', '');
                    // const cost = get(stakePool, 'cost', '');
                    const cost = 0;
                    return (
                      <tr key={key}>
                        <td>{rank}</td>
                        <td>{name}</td>
                        <td>{saturation}</td>
                        <td>{performance}</td>
                        <td>{uptime}</td>
                        <td>{margin}</td>
                        <td>{roi}</td>
                        <td>{cost}</td>
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
    if (stakePoolsSortBy === newSortBy) {
      newOrder = stakePoolsOrder === 'asc' ? 'desc' : 'asc';
    } else {
      newOrder = 'desc';
    }
    this.setState({
      stakePoolsOrder: newOrder,
      stakePoolsSortBy: newSortBy,
    });
  };
}
