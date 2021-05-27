// @flow
import React, { Component, Fragment } from 'react';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import classnames from 'classnames';
import SVGInline from 'react-svg-inline';
import CopyToClipboard from 'react-copy-to-clipboard';
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
import copyIcon from '../../../assets/images/copy-asset.inline.svg';
import copyCheckmarkIcon from '../../../assets/images/check-w.inline.svg';
import { ITEM_COPY_FEEDBACK } from '../../../config/timingConfig';

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
  currentDateFormat: string,
  currentLocale: string,
  isFetchingRewardsHistory: boolean,
  onClose: Function,
  onCopy?: Function,
  onExportCSV: Function,
  onSetDateRange: Function,
  reward: RewardForIncentivizedTestnet,
  rewardsHistory?: Array<RewardsHistoryItem>,
  startDate: Date,
  endDate: ?Date,
};

type State = {
  isEditingDate: boolean,
  itemCopied: boolean,
};

@observer
export default class StakingRewardsHistoryDialog extends Component<
  Props,
  State
> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  copyNotificationTimeout: TimeoutID;

  state = {
    isEditingDate: false,
    itemCopied: false,
  };

  handleCopy = () => {
    const { onCopy, reward } = this.props;
    const { rewardsAddress } = reward || {};
    onCopy(rewardsAddress);
    this.setState({
      itemCopied: true,
    });
    clearTimeout(this.copyNotificationTimeout);
    this.copyNotificationTimeout = setTimeout(() => {
      this.setState({ itemCopied: false });
    }, ITEM_COPY_FEEDBACK);
  };

  render() {
    const { intl } = this.context;
    const {
      currentDateFormat,
      currentLocale,
      endDate,
      isFetchingRewardsHistory,
      onClose,
      onExportCSV,
      onSetDateRange,
      reward,
      rewardsHistory,
      startDate,
    } = this.props;
    const { isEditingDate, itemCopied } = this.state;
    const { walletName, rewardsAddress } = reward || {};
    const icon = itemCopied ? copyCheckmarkIcon : copyIcon;
    const copyIconWrapperStyles = classnames([
      styles.copyIconWrapper,
      itemCopied ? styles.visible : null,
    ]);
    const actions = [
      {
        label: intl.formatMessage(globalMessages.cancel),
        onClick: onClose,
      },
      {
        label: 'Export CSV',
        primary: true,
        onClick: onExportCSV,
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

    const componentStyles = classnames([
      styles.component,
      'StakingRewardsHistoryDialog',
    ]);

    return (
      <Dialog
        className={componentStyles}
        title={intl.formatMessage(messages.title)}
        subtitle={walletName}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <div className={styles.label}>Rewards address</div>

        <CopyToClipboard text={rewardsAddress} onCopy={this.handleCopy}>
          <p className={styles.rewardsAddress}>
            {rewardsAddress}
            <span className={copyIconWrapperStyles}>
              <PopOver
                content={intl.formatMessage(globalMessages.copy)}
                appendTo="parent"
                visible
              >
                <SVGInline svg={icon} className={styles.copyIcon} />
              </PopOver>
            </span>
          </p>
        </CopyToClipboard>

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
            onChange={onSetDateRange}
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
