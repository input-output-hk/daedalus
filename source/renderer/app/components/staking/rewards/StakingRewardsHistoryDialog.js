// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import BigNumber from 'bignumber.js';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import type {
  RewardForIncentivizedTestnet,
  RewardsHistoryItem,
} from '../../../api/staking/types';
import styles from './StakingRewardsHistoryDialog.scss';
import globalMessages from '../../../i18n/global-messages';
import Table from '../../widgets/Table';
import StakePool from '../../../domains/StakePool';
import DatePicker from '../../widgets/forms/DatePicker';

const messages = defineMessages({
  title: {
    id: 'staking.rewards.dialog.title',
    defaultMessage: '!!!Rewards details',
    description:
      'Title "Earned delegation rewards" label on the staking rewards page.',
  },
  exportCsvLabel: {
    id: 'staking.rewards.dialog.csv.label',
    defaultMessage: '!!!Export CSV',
    description:
      'Title "Earned delegation rewards" label on the staking rewards page.',
  },
});

type Props = {
  reward: RewardForIncentivizedTestnet,
  rewardsHistory?: Array<RewardsHistoryItem>,
  currentDateFormat: string,
  currentLocale: string,
  onClose: Function,
  isFetchingRewardsHistory: boolean,
};

type State = {
  isEditingDate: boolean,
  startDate: Date,
  endDate: ?Date,
};

@observer
export default class StakingRewardsHistoryDialog extends Component<
  Props,
  State
> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isEditingDate: false,
    startDate: new Date(),
    endDate: null,
  };

  render() {
    const { intl } = this.context;
    const {
      reward,
      onClose,
      rewardsHistory,
      currentDateFormat,
      currentLocale,
      isFetchingRewardsHistory,
    } = this.props;
    const { isEditingDate, startDate, endDate } = this.state;
    const { walletName, rewardsAddress } = reward || {};
    const actions = [
      {
        label: intl.formatMessage(globalMessages.cancel),
        onClick: onClose,
      },
      {
        label: 'Export CSV',
        primary: true,
        onClick: () => {},
      },
    ];

    const tableColumns = [
      {
        title: 'Date',
        id: 'date',
        render: (date: string) => moment(date).format(currentDateFormat),
      },
      {
        title: 'Epoch',
        id: 'epoch',
      },
      {
        title: 'Stake Pool',
        id: 'pool',
        sortValue: (pool: ?StakePool) =>
          pool ? `${pool.ticker}${pool.name}` : null,
        render: (pool: ?StakePool) =>
          pool ? (
            <Fragment>
              <span>[{pool.ticker}]</span>
              {pool.name}
            </Fragment>
          ) : null,
      },
      {
        title: 'Reward (ADA)',
        id: 'amount',
        type: 'bigNumber',
        render: (rewardAmount: BigNumber) => rewardAmount.toFormat(6),
      },
    ];

    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.title)}
        subtitle={walletName}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <div className={styles.label}>Rewards address</div>
        <p>{rewardsAddress}</p>
        <div className={styles.label}>Date range</div>
        <input
          value={`${new Date(startDate).toISOString()} - ${
            endDate ? new Date(endDate).toISOString() : ''
          }`}
        />
        <button
          style={{ color: 'white ' }}
          onClick={() => this.setState({ isEditingDate: true })}
        >
          EDIT DATE RANGE
        </button>
        {isEditingDate && (
          <DatePicker
            startDate={startDate}
            endDate={endDate}
            onChange={({ startDate, endDate }) => {
              const isEditingDate = startDate === endDate;
              this.setState({
                startDate,
                endDate,
                isEditingDate,
              });
            }}
            currentLocale={currentLocale}
            currentDateFormat={currentDateFormat}
          />
        )}
        {isFetchingRewardsHistory && <div>LOADING</div>}
        {!isFetchingRewardsHistory && rewardsHistory && (
          <Table
            columns={tableColumns}
            rows={rewardsHistory}
            className={styles.table}
            maxHeight={265}
            isCompact
          />
        )}
      </Dialog>
    );
  }
}
