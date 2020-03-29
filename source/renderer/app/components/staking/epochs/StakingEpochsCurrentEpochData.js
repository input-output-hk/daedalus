// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { get, map } from 'lodash';
import StakingEpochsNoData from './StakingEpochsNoData';
import StakingEpochsDataTable from './StakingEpochsDataTable';
import {
  getTableHeadersForCurrentEpoch,
  noDataExisting,
  hasDataExisting,
  sortData,
} from './helpers.js';
import type { EpochData } from '../../../api/staking/types';
import styles from './StakingEpochs.scss';

const messages = defineMessages({
  tableHeaderPool: {
    id: 'staking.epochs.currentEpoch.tableHeader.pool',
    defaultMessage: '!!!Stake pool',
    description: 'Table header "Stake pool" label on staking epochs page',
  },
  tableHeaderSlotsElected: {
    id: 'staking.epochs.currentEpoch.tableHeader.slotsElected',
    defaultMessage: '!!!Slots elected',
    description: 'Table header "Slots elected" label on staking epochs page',
  },
});

type Props = {
  currentEpochData: EpochData,
  isLoading: boolean,
};

type State = {
  currentEpochDataOrder: string,
  currentEpochDataSortBy: string,
};

@observer
export default class StakingEpochsCurrentEpochData extends Component<
  Props,
  State
> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    isLoading: false,
  };

  constructor() {
    super();
    this.state = {
      currentEpochDataOrder: 'desc',
      currentEpochDataSortBy: 'pool',
    };
  }

  handleDataSort = (newSortBy: string) => {
    const { currentEpochDataOrder, currentEpochDataSortBy } = this.state;
    let newOrder;

    if (currentEpochDataSortBy === newSortBy) {
      newOrder = currentEpochDataOrder === 'asc' ? 'desc' : 'asc';
    } else {
      newOrder = 'desc';
    }
    this.setState({
      currentEpochDataSortBy: newSortBy,
      currentEpochDataOrder: newOrder,
    });
  };

  render() {
    const { currentEpochData, isLoading } = this.props;
    const { currentEpochDataOrder, currentEpochDataSortBy } = this.state;
    const { intl } = this.context;
    const noData = noDataExisting(isLoading, currentEpochData);
    const hasData = hasDataExisting(isLoading, currentEpochData);
    const tableHeaders = getTableHeadersForCurrentEpoch(intl, messages);
    let sortedData = null;

    if (noData) {
      return <StakingEpochsNoData />;
    }

    if (hasData) {
      sortedData = sortData(
        currentEpochData,
        currentEpochDataOrder,
        currentEpochDataSortBy
      );
    }

    const tableBody = (
      <tbody>
        {map(sortedData, (row, key) => {
          const poolTicker = get(row, ['pool', 'ticker'], '');
          const poolName = get(row, ['pool', 'name'], '');
          const slotsElected = get(row, 'slotsElected', [0]);

          return (
            <tr key={key}>
              <td>
                <p>
                  <span className={styles.stakePoolReference}>
                    [{poolTicker}]
                  </span>{' '}
                  {poolName}
                </p>
              </td>
              <td>
                <span
                  className={styles.mediumText}
                >{`${slotsElected[0]}%`}</span>
              </td>
            </tr>
          );
        })}
      </tbody>
    );

    return (
      <StakingEpochsDataTable
        tableHeaders={tableHeaders}
        tableBody={tableBody}
        order={currentEpochDataOrder}
        sortBy={currentEpochDataSortBy}
        handleDataSort={this.handleDataSort}
      />
    );
  }
}
