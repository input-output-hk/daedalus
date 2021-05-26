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
  onClose: Function,
};

@observer
export default class StakingRewardsHistoryDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { reward, onClose, rewardsHistory, currentDateFormat } = this.props;
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
        {/* <input value="..." /> */}
        {rewardsHistory && (
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
