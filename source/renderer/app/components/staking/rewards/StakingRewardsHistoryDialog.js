// @flow
import React, { Component } from 'react';
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
import { PoolPopOver } from '../widgets/PoolPopOver';
import globalMessages from '../../../i18n/global-messages';
import Table from '../../widgets/Table';
import StakePool from '../../../domains/StakePool';
import copyIcon from '../../../assets/images/copy-asset.inline.svg';
import copyCheckmarkIcon from '../../../assets/images/check-w.inline.svg';
import { ITEM_COPY_FEEDBACK } from '../../../config/timingConfig';
import LoadingSpinner from '../../widgets/LoadingSpinner';

const messages = defineMessages({
  title: {
    id: 'staking.rewardsHistory.dialog.title',
    defaultMessage: '!!!Rewards details',
    description:
      'Title "Earned delegation rewards" label on the rewards history dialog.',
  },
  exportCsvLabel: {
    id: 'staking.rewardsHistory.dialog.csv.label',
    defaultMessage: '!!!Export CSV',
    description: 'TexportCsvLabel on the rewards history dialog.',
  },
  rewardsAddress: {
    id: 'staking.rewardsHistory.dialog.rewardsAddress',
    defaultMessage: '!!!Rewards address',
    description: '"Rewards address" label on the rewards history dialog.',
  },
});

type Props = {
  currentDateFormat: string,
  currentTheme: string,
  isFetchingRewardsHistory: boolean,
  onClose: Function,
  onCopy?: Function,
  onExportCSV: Function,
  onOpenExternalLink: Function,
  reward: RewardForIncentivizedTestnet,
  rewardsHistory: Array<RewardsHistoryItem>,
};

type State = {
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
    itemCopied: false,
  };

  handleCopy = () => {
    const { onCopy, reward } = this.props;
    const { rewardsAddress } = reward || {};
    this.setState({
      itemCopied: true,
    });
    clearTimeout(this.copyNotificationTimeout);
    this.copyNotificationTimeout = setTimeout(() => {
      this.setState({ itemCopied: false });
    }, ITEM_COPY_FEEDBACK);
    if (onCopy) onCopy(rewardsAddress);
  };

  render() {
    const { intl } = this.context;
    const {
      currentTheme,
      isFetchingRewardsHistory,
      onClose,
      onExportCSV,
      onOpenExternalLink,
      reward,
      rewardsHistory,
    } = this.props;
    const { itemCopied } = this.state;
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
        onClick: () => onExportCSV({ rewardsAddress, walletName }),
        disabled: isFetchingRewardsHistory,
      },
    ];

    const tableColumns = [
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
            <PoolPopOver
              containerClassName="StakingRewardsHistoryDialog_table"
              currentTheme={currentTheme}
              numberOfRankedStakePools={0}
              onOpenExternalLink={onOpenExternalLink}
              openOnHover
              stakePool={pool}
            >
              <span className={styles.stakePoolTicker}>[{pool.ticker}] </span>
              {pool.name}
            </PoolPopOver>
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
        <div className={styles.label}>
          {intl.formatMessage(messages.rewardsAddress)}
        </div>

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

        {isFetchingRewardsHistory ? (
          <div className={styles.loadingSpinner}>
            <LoadingSpinner big />
          </div>
        ) : (
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
