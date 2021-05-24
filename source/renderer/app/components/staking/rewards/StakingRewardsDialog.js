// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import type {
  RewardForIncentivizedTestnet,
  RewardsHistoryItem,
} from '../../../api/staking/types';
import styles from './StakingRewardsDialog.scss';
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
export default class StakingRewardsDialog extends Component<Props> {
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
        Header: 'Date',
        acessor: 'date',
      },
      {
        Header: 'Epoch',
        acessor: 'epoch',
      },
      {
        Header: 'Stake Pool',
        acessor: 'pool',
      },
      {
        Header: 'Reward (ADA)',
        acessor: 'reward',
      },
    ];

    const getPoolRowItem = (pool: ?StakePool) =>
      pool ? (
        <div>
          <p>[{pool.ticker}]</p>
          {pool.name}
        </div>
      ) : null;

    const data =
      rewardsHistory &&
      rewardsHistory.map((rewardItem) => ({
        date: moment(rewardItem.date).format(currentDateFormat),
        epoch: rewardItem.epoch,
        pool: getPoolRowItem(rewardItem.pool),
        reward: rewardItem.reward.toFormat(2),
      }));

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
        <input value="..." />
        <Table
          columns={tableColumns}
          data={data || []}
          className={styles.table}
          isLoading={!rewardsHistory}
          onClickRow={(a, b, c) => {
            console.log('onClickRow');
            console.log('a', a);
            console.log('b', b);
            console.log('c', c);
          }}
          onClickCell={(a, b, c) => {
            console.log('onClickCell');
            console.log('a', a);
            console.log('b', b);
            console.log('c', c);
          }}
        />
      </Dialog>
    );
  }
}
