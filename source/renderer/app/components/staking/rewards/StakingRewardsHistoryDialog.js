// @flow
import React, { Component } from 'react';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { Link } from 'react-polymorph/lib/components/Link';
import classnames from 'classnames';
import SVGInline from 'react-svg-inline';
import CopyToClipboard from 'react-copy-to-clipboard';
import { observer } from 'mobx-react';
import BigNumber from 'bignumber.js';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import type { Reward, RewardsHistoryItem } from '../../../api/staking/types';
import styles from './StakingRewardsHistoryDialog.scss';
import { PoolPopOver } from '../widgets/PoolPopOver';
import globalMessages from '../../../i18n/global-messages';
import Table from '../../widgets/Table';
import StakePool from '../../../domains/StakePool';
import copyIcon from '../../../assets/images/copy-asset.inline.svg';
import copyCheckmarkIcon from '../../../assets/images/check-w.inline.svg';
import { ITEM_COPY_FEEDBACK } from '../../../config/timingConfig';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import Ellipsis from '../../widgets/Ellipsis';

const messages = defineMessages({
  title: {
    id: 'staking.rewardsHistory.dialog.title',
    defaultMessage: '!!!Rewards details',
    description:
      'Title "Earned delegation rewards" label on the rewards history dialog.',
  },
  columnEpoch: {
    id: 'staking.rewardsHistory.dialog.column.epoch',
    defaultMessage: '!!!Epoch',
    description: 'Transactions CSV column - Epoch',
  },
  columnStakePool: {
    id: 'staking.rewardsHistory.dialog.column.stakePool',
    defaultMessage: '!!!Stake pool',
    description: 'Transactions CSV column - Pool',
  },
  columnAmount: {
    id: 'staking.rewardsHistory.dialog.column.amount',
    defaultMessage: '!!!Amount',
    description: 'Transactions CSV column - Amount',
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
  unknownStakePoolTooltip: {
    id: 'staking.rewardsHistory.dialog.unknownStakePoolTooltip',
    defaultMessage:
      '!!!Stake pool details are unavailable for this stake pool ID.',
    description: '"Unknown Stake Pool" tooltip on the rewards history dialog.',
  },
  noDataTitle: {
    id: 'staking.rewardsHistory.dialog.noData.title',
    defaultMessage: '!!!Rewards details cannot be displayed at this time.',
    description: '"No Data" on the rewards history dialog.',
  },
  noDataDescription: {
    id: 'staking.rewardsHistory.dialog.noData.description',
    defaultMessage:
      '!!!Please try again later. If this issue persists, please {link}.',
    description: '"No Data" on the rewards history dialog.',
  },
  noDataLinkLabel: {
    id: 'staking.rewardsHistory.dialog.noData.linkLabel',
    defaultMessage: '!!!submit a support request',
    description: '"No Data Link Url" on the rewards history dialog.',
  },
  noDataLinkUrl: {
    id: 'staking.rewardsHistory.dialog.noData.linkUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/requests/new/',
    description: '"No Data Link Url" on the rewards history dialog.',
  },
});

type Props = {
  currentTheme: string,
  isFetchingRewardsHistory: boolean,
  onClose: Function,
  onExportCSV: Function,
  onOpenExternalLink: Function,
  onNoDataClick: Function,
  reward: Reward,
  rewardsHistory: Array<RewardsHistoryItem>,
  onCopyAddress: Function,

  // TEMP
  lastEpoch: number,
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
    const { onCopyAddress, reward } = this.props;
    const { rewardsAddress } = reward || {};
    this.setState({
      itemCopied: true,
    });
    clearTimeout(this.copyNotificationTimeout);
    this.copyNotificationTimeout = setTimeout(() => {
      this.setState({ itemCopied: false });
    }, ITEM_COPY_FEEDBACK);
    onCopyAddress(rewardsAddress);
  };

  renderStakePool = (pool: StakePool) => {
    const { currentTheme, onOpenExternalLink } = this.props;
    return (
      <div>
        <PoolPopOver
          containerClassName="StakingRewardsHistoryDialog_table"
          currentTheme={currentTheme}
          numberOfRankedStakePools={0}
          onOpenExternalLink={onOpenExternalLink}
          openOnHover
          stakePool={pool}
          hideRanking
        >
          <span className={styles.stakePoolTicker}>[{pool.ticker}] </span>
        </PoolPopOver>
        &nbsp;
        {pool.name}
      </div>
    );
  };

  renderStakePoolId = (poolID: string) => (
    <PopOver
      content={this.context.intl.formatMessage(
        messages.unknownStakePoolTooltip
      )}
    >
      <Ellipsis string={poolID} />
    </PopOver>
  );

  renderNoDataContent = () => {
    const { intl } = this.context;
    const { onNoDataClick } = this.props;
    const onClickNoDataLink = () =>
      onNoDataClick(intl.formatMessage(messages.noDataLinkUrl));
    const noDataLink = (
      <Link
        className={styles.link}
        onClick={onClickNoDataLink}
        label={intl.formatMessage(messages.noDataLinkLabel)}
      />
    );
    return (
      <div className={styles.noData}>
        <p>{intl.formatMessage(messages.noDataTitle)}</p>
        <FormattedMessage
          {...messages.noDataDescription}
          values={{ link: noDataLink }}
        />
      </div>
    );
  };

  renderContent = () => {
    const { intl } = this.context;
    const { isFetchingRewardsHistory, rewardsHistory } = this.props;

    const tableColumns = [
      {
        title: intl.formatMessage(messages.columnEpoch),
        id: 'epoch',
      },
      {
        title: intl.formatMessage(messages.columnStakePool),
        id: 'pool',
        sortValue: (pool: StakePool) =>
          pool.ticker ? `${pool.ticker}${pool.name}` : pool.id,
        render: (pool: $Shape<StakePool>) =>
          pool.name
            ? this.renderStakePool(pool)
            : this.renderStakePoolId(pool.id),
      },
      {
        title: intl.formatMessage(messages.columnAmount),
        id: 'amount',
        type: 'bigNumber',
        render: (rewardAmount: BigNumber) => rewardAmount.toFormat(6),
      },
    ];

    if (isFetchingRewardsHistory) {
      return (
        <div className={styles.loadingSpinner}>
          <LoadingSpinner big />
        </div>
      );
    }

    if (!rewardsHistory.length) {
      return this.renderNoDataContent();
    }

    return (
      <Table
        columns={tableColumns}
        rows={rewardsHistory}
        className={styles.table}
        maxHeight={265}
        isCompact
        initialSortDirection="asc"
      />
    );
  };

  render() {
    const { intl } = this.context;
    const {
      isFetchingRewardsHistory,
      onClose,
      onExportCSV,
      reward,
      rewardsHistory,
    } = this.props;
    const { itemCopied } = this.state;
    const { walletName, rewardsAddress } = reward || {};
    const icon = itemCopied ? copyCheckmarkIcon : copyIcon;
    const copyIconWrapperStyles = classnames([
      styles.copyIconWrapper,
      itemCopied ? styles.visible : null,
      itemCopied ? styles.itemCopied : null,
    ]);
    const actions = [
      {
        label: intl.formatMessage(globalMessages.close),
        onClick: onClose,
      },
      {
        label: intl.formatMessage(messages.exportCsvLabel),
        primary: true,
        onClick: () => onExportCSV({ rewardsAddress, walletName }),
        disabled: isFetchingRewardsHistory || !rewardsHistory.length,
      },
    ];

    const total1 = reward.reward;
    console.log('total1', total1.toFormat(6));
    const total2 = rewardsHistory.reduce((total, item) => {
      total = total.plus(item.amount);
      return total;
    }, new BigNumber(0));
    console.log('total2', total2.toFormat(6));

    window.BigNumberz = BigNumber;
    const difference = total2.minus(total1);
    let foundItem;

    if (!total1.isEqualTo(total2)) {
      console.log('difference', difference.toFormat(6));
      foundItem = rewardsHistory.reduce((found, item) => {
        if (item.amount.isEqualTo(difference)) {
          return item;
        }
        return found;
      }, null);
    }

    const componentStyles = classnames([
      styles.component,
      'StakingRewardsHistoryDialog',
    ]);

    const copyMessage = itemCopied
      ? globalMessages.copied
      : globalMessages.copy;

    return (
      <Dialog
        className={componentStyles}
        title={intl.formatMessage(messages.title)}
        subtitle={walletName}
        actions={actions}
        onClose={onClose}
        closeButton={<DialogCloseButton />}
        closeOnOverlayClick={false}
      >
        <div className={styles.label}>DEBUG</div>

        <ul>
          <li>Total from withdraw: {total1.toFormat(6)}</li>
          <li>Total from history: {total2.toFormat(6)}</li>
          {!total1.isEqualTo(total2) && (
            <>
              <li>Differentce: {difference.toFormat(6)}</li>
              {foundItem && <li>Item epoch: {foundItem.epoch}</li>}
            </>
          )}
        </ul>

        {/*

        <div className={styles.label}>
          {intl.formatMessage(messages.rewardsAddress)}
        </div>
        <CopyToClipboard text={rewardsAddress} onCopy={this.handleCopy}>
          <p className={styles.rewardsAddress}>
            {rewardsAddress}
            <span className={copyIconWrapperStyles}>
              <PopOver
                content={intl.formatMessage(copyMessage)}
                className={styles.copyPopOver}
                appendTo="parent"
                visible
              >
                <SVGInline svg={icon} className={styles.copyIcon} />
              </PopOver>
            </span>
          </p>
        </CopyToClipboard> */}

        {this.renderContent()}
      </Dialog>
    );
  }
}
