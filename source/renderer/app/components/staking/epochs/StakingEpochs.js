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
  SELECTED_EPOCH_OPTIONS,
  getTableHeadersForCurrentEpoch,
  getTableHeadersForPreviousEpoch,
  noDataExisting,
  hasDataExisting,
  sortData,
  humanizeDurationToShort,
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
  tableBodyAda: {
    id: 'environment.currency.ada',
    defaultMessage: '!!!Ada',
    description: '"Ada" text in table body on staking epochs page',
  },
});

type Props = {
  currentEpochName: string,
  currentEpochData: any,
  currentEpochEndDateTime: string,
  currentEpochProgress: number,
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

const { CURRENT_EPOCH, PREVIOUS_EPOCH } = SELECTED_EPOCH_OPTIONS;

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
      selectedEpoch: CURRENT_EPOCH,
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

    if (selectedEpoch === CURRENT_EPOCH) {
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

  renderDataTable = (
    selectedEpoch: string,
    tableHeaders: any,
    tableBody: any,
    order: string,
    sortBy: string
  ) => (
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

    const tableBody = (
      <tbody>
        {map(sortedData, (row, key) => {
          const poolCategory = get(row, ['pool', 'category'], '');
          const poolTitle = get(row, ['pool', 'title'], '');
          const slotsElected = get(row, 'slotsElected', 0);

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
              <td>
                <span
                  className={styles.semiboldText}
                >{`${slotsElected}%`}</span>
              </td>
            </tr>
          );
        })}
      </tbody>
    );

    return this.renderDataTable(
      selectedEpoch,
      tableHeaders,
      tableBody,
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

    const tableBody = (
      <tbody>
        {map(sortedData, (row, key) => {
          const poolCategory = get(row, ['pool', 'category'], '');
          const poolTitle = get(row, ['pool', 'title'], '');
          const slotsElected = get(row, 'slotsElected', [0, 0]);
          const performance = get(row, 'performance', [0, 0, 0]);
          const sharedRewards = get(row, 'sharedRewards', [0, 0]);

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
              <td>
                <span className={styles.semiboldText}>{slotsElected[0]}</span>
                <span>{` ${intl.formatMessage(
                  messages.tableBodySlots
                )} - `}</span>
                <span className={styles.semiboldText}>{`${
                  slotsElected[1]
                }%`}</span>
              </td>
              <td>
                <span>{`${performance[0]} ${intl.formatMessage(
                  messages.tableBodyOf
                )} ${performance[1]} - `}</span>
                <span className={styles.semiboldText}>{`${
                  performance[2]
                }%`}</span>
              </td>
              <td>
                <span className={styles.semiboldText}>{sharedRewards[0]}</span>
                <span className={styles.uppercaseText}>{` ${intl.formatMessage(
                  messages.tableBodyAda
                )} `}</span>
                <span>{`${intl.formatMessage(messages.tableBodyOf)} `}</span>
                <span className={styles.semiboldText}>{sharedRewards[1]}</span>
                <span className={styles.uppercaseText}>{` ${intl.formatMessage(
                  messages.tableBodyAda
                )}`}</span>
              </td>
            </tr>
          );
        })}
      </tbody>
    );

    return this.renderDataTable(
      selectedEpoch,
      tableHeaders,
      tableBody,
      previousEpochDataOrder,
      previousEpochDataSortBy
    );
  };

  render() {
    const {
      currentEpochName,
      currentEpochEndDateTime,
      currentEpochProgress,
      previousEpochName,
      isLoading,
    } = this.props;
    const { selectedEpoch } = this.state;
    const { intl } = this.context;
    const epochSelectOptions = [
      {
        label: `${currentEpochName} (${intl.formatMessage(
          messages.currentEpochHeading
        )})`,
        value: CURRENT_EPOCH,
      },
      {
        label: `${previousEpochName} (${intl.formatMessage(
          messages.previousEpochHeading
        )})`,
        value: PREVIOUS_EPOCH,
      },
    ];
    const duration = humanizeDurationToShort(currentEpochEndDateTime);

    if (!currentEpochName) {
      return null;
    }

    return (
      <div className={styles.component}>
        <BorderedBox>
          <div className={styles.headerWrapper}>
            <Select
              className={styles.epochSelector}
              options={epochSelectOptions}
              value={selectedEpoch}
              onChange={this.onSelectedEpochChange}
              skin={SelectSkin}
            />
          </div>
          {selectedEpoch === CURRENT_EPOCH && (
            <div>
              <div className={styles.currentEpochProgressBar}>
                <div className={styles.progressBarContainer}>
                  <div
                    className={styles.progress}
                    style={{ width: `${currentEpochProgress}%` }}
                  >
                    <div
                      className={styles.overlapProgressLabel}
                      style={{ left: `${10000 / currentEpochProgress}%` }}
                    >
                      {duration}
                    </div>
                  </div>
                  <div className={styles.progressLabel}>{duration}</div>
                </div>
              </div>
              {this.renderCurrentEpochData()}
            </div>
          )}
          {selectedEpoch === PREVIOUS_EPOCH && this.renderPreviousEpochData()}
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
