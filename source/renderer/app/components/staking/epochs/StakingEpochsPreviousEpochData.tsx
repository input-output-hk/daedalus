import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { get, map } from 'lodash';
import StakingEpochsNoData from './StakingEpochsNoData';
import StakingEpochsDataTable from './StakingEpochsDataTable';
import {
  getTableHeadersForPreviousEpoch,
  noDataExisting,
  hasDataExisting,
  sortData,
} from './helpers';
import type { EpochData } from '../../../api/staking/types';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakingEpochs.scss' or its c... Remove this comment to see the full error message
import styles from './StakingEpochs.scss';
import globalMessages from '../../../i18n/global-messages';

const messages = defineMessages({
  tableHeaderPool: {
    id: 'staking.epochs.previousEpoch.tableHeader.pool',
    defaultMessage: '!!!Stake pool',
    description: 'Table header "Stake pool" label on staking epochs page',
  },
  tableHeaderSlotsElected: {
    id: 'staking.epochs.previousEpoch.tableHeader.slotsElected',
    defaultMessage: '!!!Slots elected',
    description: 'Table header "Slots elected" label on staking epochs page',
  },
  tableHeaderPerformance: {
    id: 'staking.epochs.tableHeader.performance',
    defaultMessage: '!!!Performance',
    description: 'Table header "Performance" label on staking epochs page',
  },
  tableHeaderSharedRewards: {
    id: 'staking.epochs.tableHeader.sharedRewards',
    defaultMessage: '!!!Shared rewards',
    description: 'Table header "Shared rewards" label on staking epochs page',
  },
  tableBodySlots: {
    id: 'staking.epochs.tableBody.slots',
    defaultMessage: '!!!slots',
    description: '"slots" text in table body on staking epochs page',
  },
  tableBodyOf: {
    id: 'staking.epochs.tableBody.of',
    defaultMessage: '!!!of',
    description: '"of" text in table body on staking epochs page',
  },
});
type Props = {
  previousEpochData: EpochData;
  isLoading: boolean;
};
type State = {
  previousEpochDataOrder: string;
  previousEpochDataSortBy: string;
};

@observer
class StakingEpochsPreviousEpochData extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  static defaultProps = {
    isLoading: false,
  };

  constructor() {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1-2 arguments, but got 0.
    super();
    this.state = {
      previousEpochDataOrder: 'desc',
      previousEpochDataSortBy: 'pool',
    };
  }

  handleDataSort = (newSortBy: string) => {
    const { previousEpochDataOrder, previousEpochDataSortBy } = this.state;
    let newOrder;

    if (previousEpochDataSortBy === newSortBy) {
      newOrder = previousEpochDataOrder === 'asc' ? 'desc' : 'asc';
    } else {
      newOrder = 'desc';
    }

    this.setState({
      previousEpochDataSortBy: newSortBy,
      previousEpochDataOrder: newOrder,
    });
  };

  render() {
    const { previousEpochData, isLoading } = this.props;
    const { previousEpochDataOrder, previousEpochDataSortBy } = this.state;
    const { intl } = this.context;
    const noData = noDataExisting(isLoading, previousEpochData);
    const hasData = hasDataExisting(isLoading, previousEpochData);
    const tableHeaders = getTableHeadersForPreviousEpoch(intl, messages);
    let sortedData = null;

    if (noData) {
      return <StakingEpochsNoData />;
    }

    if (hasData) {
      sortedData = sortData(
        previousEpochData,
        previousEpochDataOrder,
        previousEpochDataSortBy
      );
    }

    const tableBody = (
      <tbody>
        {map(sortedData, (row, key) => {
          const poolTicker = get(row, ['pool', 'ticker'], '');
          const poolName = get(row, ['pool', 'name'], '');
          const slotsElected = get(row, 'slotsElected', [0, 0]);
          const performance = get(row, 'performance', [0, 0, 0]);
          const sharedRewards = get(row, 'sharedRewards', [0, 0]);
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
                <span className={styles.mediumText}>{slotsElected[0]}</span>
                <span>{` ${intl.formatMessage(
                  messages.tableBodySlots
                )} - `}</span>
                <span
                  className={styles.mediumText}
                >{`${slotsElected[1]}%`}</span>
              </td>
              <td>
                <span>{`${performance[0]} ${intl.formatMessage(
                  messages.tableBodyOf
                )} ${performance[1]} - `}</span>
                <span
                  className={styles.mediumText}
                >{`${performance[2]}%`}</span>
              </td>
              <td>
                <span className={styles.mediumText}>{sharedRewards[0]}</span>
                <span className={styles.uppercaseText}>{` ${intl.formatMessage(
                  globalMessages.adaUnit
                )} `}</span>
                <span>{`${intl.formatMessage(messages.tableBodyOf)} `}</span>
                <span className={styles.mediumText}>{sharedRewards[1]}</span>
                <span className={styles.uppercaseText}>{` ${intl.formatMessage(
                  globalMessages.adaUnit
                )}`}</span>
              </td>
            </tr>
          );
        })}
      </tbody>
    );
    return (
      <StakingEpochsDataTable
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        tableHeaders={tableHeaders}
        tableBody={tableBody}
        order={previousEpochDataOrder}
        sortBy={previousEpochDataSortBy}
        handleDataSort={this.handleDataSort}
      />
    );
  }
}

export default StakingEpochsPreviousEpochData;
