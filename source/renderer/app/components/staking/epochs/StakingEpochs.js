// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { get, map } from 'lodash';
import classNames from 'classnames';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import BorderedBox from '../../widgets/BorderedBox';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import sortIcon from '../../../assets/images/ascending.inline.svg';
import {
  getTableHeadersForCurrentEpoch,
  getTableHeadersForPreviousEpoch,
  noDataExisting,
  hasDataExisting,
  sortData,
} from './helpers.js';
import styles from './StakingEpochs.scss';

const messages = defineMessages({
  currentEpochHeading: {
    id: 'staking.epochs.currentHeading',
    defaultMessage: '!!!current epoch',
    description: 'Headline for the current epoch.',
  },
  previousEpochHeading: {
    id: 'staking.epochs.previousHeading',
    defaultMessage: '!!!previous epoch',
    description: 'Headline for the previous epoch.',
  },
  noResults: {
    id: 'staking.epochs.no.results',
    defaultMessage: '!!!No results',
    description: '"No results" results label on staking epochs page.',
  },
  tableHeaderPool: {
    id: 'staking.epochs.tableHeader.pool',
    defaultMessage: '!!!Stake pool',
    description: 'Table header "Stake pool" label on staking epochs page',
  },
  tableHeaderSlotsElected: {
    id: 'staking.epochs.tableHeader.slotsElected',
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
});

type Props = {
  currentEpochName: string,
  currentEpochData: any,
  previousEpochName: string,
  previousEpochData: any,
  isLoading: boolean,
};

type State = {
  selectedEpoch: string,
  currentEpochDataOrder: string,
  previousEpochDataOrder: string,
  currentEpochDataSortBy: string,
  previousEpochDataSortBy: string,
};

@observer
export default class StakingEpochs extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    isLoading: false,
  };

  constructor() {
    super();
    this.state = {
      selectedEpoch: 'currentEpoch',
      currentEpochDataOrder: 'desc',
      previousEpochDataOrder: 'desc',
      currentEpochDataSortBy: 'pool',
      previousEpochDataSortBy: 'pool',
    };
  }

  onSelectedEpochChange = (selectedEpoch: string) =>
    this.setState({ selectedEpoch });

  handleDataSort = (selectedEpoch: string, newSortBy: string) => {
    const {
      currentEpochDataOrder,
      currentEpochDataSortBy,
      previousEpochDataOrder,
      previousEpochDataSortBy,
    } = this.state;
    let newOrder;

    if (selectedEpoch === 'currentEpoch') {
      if (currentEpochDataSortBy === newSortBy) {
        newOrder = currentEpochDataOrder === 'asc' ? 'desc' : 'asc';
      } else {
        newOrder = 'desc';
      }
      this.setState({
        currentEpochDataSortBy: newSortBy,
        currentEpochDataOrder: newOrder,
      });
    } else {
      if (previousEpochDataSortBy === newSortBy) {
        newOrder = previousEpochDataOrder === 'asc' ? 'desc' : 'asc';
      } else {
        newOrder = 'desc';
      }
      this.setState({
        previousEpochDataSortBy: newSortBy,
        previousEpochDataOrder: newOrder,
      });
    }
  };

  renderNoData = () => {
    const { intl } = this.context;

    return (
      <div className={styles.noResultsLabel}>
        {intl.formatMessage(messages.noResults)}
      </div>
    );
  };

  renderData = (
    selectedEpoch: string,
    tableHeaders: any,
    sortedData: any,
    order: string,
    sortBy: string
  ) => {
    let tableBody = null;

    if (selectedEpoch === 'currentEpoch') {
      tableBody = (
        <tbody>
          {map(sortedData, (row, key) => {
            const poolCategory = get(row, ['pool', 'category'], '');
            const poolTitle = get(row, ['pool', 'title'], '');
            const slotsElected = get(row, 'slotsElected', '');

            return (
              <tr key={key}>
                <td>
                  <p>
                    <span className={styles.stakePoolReference}>
                      [{poolCategory}]
                    </span>{' '}
                    {poolTitle}
                  </p>
                </td>
                <td>{slotsElected}</td>
              </tr>
            );
          })}
        </tbody>
      );
    } else {
      tableBody = (
        <tbody>
          {map(sortedData, (row, key) => {
            const poolCategory = get(row, ['pool', 'category'], '');
            const poolTitle = get(row, ['pool', 'title'], '');
            const slotsElected = get(row, 'slotsElected', '');
            const performance = get(row, 'performance', '');
            const sharedRewards = get(row, 'sharedRewards', '');

            return (
              <tr key={key}>
                <td>
                  <p>
                    <span className={styles.stakePoolReference}>
                      [{poolCategory}]
                    </span>{' '}
                    {poolTitle}
                  </p>
                </td>
                <td>{slotsElected}</td>
                <td>{performance}</td>
                <td>{sharedRewards}</td>
              </tr>
            );
          })}
        </tbody>
      );
    }

    return (
      <table>
        <thead>
          <tr>
            {map(tableHeaders, tableHeader => {
              const isSorted = tableHeader.name === sortBy;
              const sortIconClasses = classNames([
                styles.sortIcon,
                isSorted ? styles.sorted : null,
                isSorted && order === 'asc' ? styles.ascending : null,
              ]);

              return (
                <th
                  key={tableHeader.name}
                  onClick={() =>
                    this.handleDataSort(selectedEpoch, tableHeader.name)
                  }
                >
                  {tableHeader.title}
                  <SVGInline svg={sortIcon} className={sortIconClasses} />
                </th>
              );
            })}
          </tr>
        </thead>
        {tableBody}
      </table>
    );
  };

  renderCurrentEpochData = () => {
    const { currentEpochData, isLoading } = this.props;
    const {
      selectedEpoch,
      currentEpochDataOrder,
      currentEpochDataSortBy,
    } = this.state;
    const { intl } = this.context;
    const noData = noDataExisting(isLoading, currentEpochData);
    const hasData = hasDataExisting(isLoading, currentEpochData);
    const tableHeaders = getTableHeadersForCurrentEpoch(intl, messages);
    let sortedData = null;

    if (noData) {
      return this.renderNoData();
    }

    if (hasData) {
      sortedData = sortData(
        currentEpochData,
        currentEpochDataOrder,
        currentEpochDataSortBy
      );
    }

    return this.renderData(
      selectedEpoch,
      tableHeaders,
      sortedData,
      currentEpochDataOrder,
      currentEpochDataSortBy
    );
  };

  renderPreviousEpochData = () => {
    const { previousEpochData, isLoading } = this.props;
    const {
      selectedEpoch,
      previousEpochDataOrder,
      previousEpochDataSortBy,
    } = this.state;
    const { intl } = this.context;
    const noData = noDataExisting(isLoading, previousEpochData);
    const hasData = hasDataExisting(isLoading, previousEpochData);
    const tableHeaders = getTableHeadersForPreviousEpoch(intl, messages);
    let sortedData = null;

    if (noData) {
      return this.renderNoData();
    }

    if (hasData) {
      sortedData = sortData(
        previousEpochData,
        previousEpochDataOrder,
        previousEpochDataSortBy
      );
    }

    return this.renderData(
      selectedEpoch,
      tableHeaders,
      sortedData,
      previousEpochDataOrder,
      previousEpochDataSortBy
    );
  };

  render() {
    const { currentEpochName, previousEpochName, isLoading } = this.props;
    const { selectedEpoch } = this.state;
    const { intl } = this.context;
    const epochSelectOptions = [
      {
        label: `${currentEpochName} (${intl.formatMessage(
          messages.currentEpochHeading
        )})`,
        value: 'currentEpoch',
      },
      {
        label: `${previousEpochName} (${intl.formatMessage(
          messages.previousEpochHeading
        )})`,
        value: 'previousEpoch',
      },
    ];

    return (
      <div className={styles.component}>
        <div className={styles.headerWrapper}>
          <Select
            className={styles.epochSelector}
            options={epochSelectOptions}
            value={selectedEpoch}
            onChange={this.onSelectedEpochChange}
            skin={SelectSkin}
          />
        </div>
        <BorderedBox>
          {selectedEpoch === 'currentEpoch' && this.renderCurrentEpochData()}
          {selectedEpoch === 'previousEpoch' && this.renderPreviousEpochData()}
          {isLoading && (
            <div className={styles.loadingSpinnerWrapper}>
              <LoadingSpinner />
            </div>
          )}
        </BorderedBox>
      </div>
    );
  }
}
