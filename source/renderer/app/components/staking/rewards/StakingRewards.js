// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { get, map } from 'lodash';
import classNames from 'classnames';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import {
  bigNumberComparator,
  stringComparator,
} from '../../../utils/sortComparators';
import BorderedBox from '../../widgets/BorderedBox';
import tinySpinnerIcon from '../../../assets/images/spinner-tiny.inline.svg';
import sortIcon from '../../../assets/images/ascending.inline.svg';
import type { Reward } from '../../../api/staking/types';
import styles from './StakingRewards.scss';
import globalMessages from '../../../i18n/global-messages';
import Ellipsis from '../../widgets/Ellipsis';
import LoadingStakePools from '../widgets/LoadingStakePools';

const messages = defineMessages({
  title: {
    id: 'staking.rewards.title',
    defaultMessage: '!!!Earned delegation rewards',
    description:
      'Title "Earned delegation rewards" label on the staking rewards page.',
  },
  noRewards: {
    id: 'staking.rewards.no.rewards',
    defaultMessage: '!!!No rewards',
    description: '"No rewards" rewards label on staking rewards page.',
  },
  tableHeaderWallet: {
    id: 'staking.rewards.tableHeader.wallet',
    defaultMessage: '!!!Wallet',
    description: 'Table header "Wallet" label on staking rewards page',
  },
  tableHeaderReward: {
    id: 'staking.rewards.tableHeader.reward',
    defaultMessage: '!!!Total (ADA)',
    description: 'Table header "Reward" label on staking rewards page',
  },
  tableHeaderRewardsAddress: {
    id: 'staking.rewards.tableHeader.rewardsAddress',
    defaultMessage: '!!!Rewards address',
    description: 'Table header "Rewards address" label on staking rewards page',
  },
  note: {
    id: 'staking.rewards.note',
    defaultMessage:
      '!!!<p>Rewards earned by delegating your stake are automatically collected into your reward account.</p><p>Rewards earned on the Incentivized Testnet are not added to your Rewards wallet balance. They will be paid to you in real ada on the Cardano mainnet after the end of the Incentivized Testnet.</p><p>If you are using funds from this wallet to operate a stake pool, the rewards displayed here may include your pledged stake, which will not be counted when reward balances are paid out on the Cardano mainnet.</p>',
    description: 'Rewards description text on staking rewards page',
  },
  syncingTooltipLabel: {
    id: 'staking.delegationCenter.syncingTooltipLabel',
    defaultMessage: '!!!Syncing {syncingProgress}%',
    description: 'unknown stake pool label on staking rewards page.',
  },
  detailsButtonLabel: {
    id: 'staking.rewards.detailsButton.label',
    defaultMessage: '!!!Details',
    description: 'Details Button label on staking rewards page.',
  },
  actionViewInExplorer: {
    id: 'staking.rewards.actionViewInExplorer',
    defaultMessage: '!!!View in explorer',
    description: 'View in explorer button label on staking rewards page.',
  },
});

const REWARD_FIELDS = {
  WALLET_ID: 'walletId',
  WALLET_NAME: 'walletName',
  IS_RESTORING: 'isRestoring',
  SYNCING_PROGRESS: 'syncingProgress',
  REWARD_AMOUNT: 'reward',
  REWARDS_ADDRESS: 'rewardsAddress',
  ACTIONS: 'actions',
};

const REWARD_ORDERS = {
  ASCENDING: 'asc',
  DESCENDING: 'desc',
};

type Props = {
  rewards: Array<Reward>,
  isLoading: boolean,
  onOpenWalletRewards: Function,
};

type State = {
  rewardsOrder: string,
  rewardsSortBy: string,
  contentScrollTop: number,
};

@observer
export default class StakingRewards extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    isLoading: false,
    isExporting: false,
  };

  constructor(props: Props) {
    super(props);
    this.state = {
      rewardsOrder: REWARD_ORDERS.ASCENDING,
      rewardsSortBy: REWARD_FIELDS.WALLET_NAME,
      contentScrollTop: 0,
    };
  }

  getSortedRewards = (): Array<Reward> => {
    const { rewards } = this.props;
    const { rewardsOrder, rewardsSortBy } = this.state;
    return rewards.slice().sort((rewardA: Reward, rewardB: Reward) => {
      const rewardCompareResult = bigNumberComparator(
        rewardA.reward,
        rewardB.reward,
        rewardsOrder === REWARD_ORDERS.ASCENDING
      );
      const walletNameCompareResult = stringComparator(
        rewardA.walletName,
        rewardB.walletName,
        rewardsOrder === REWARD_ORDERS.ASCENDING
      );
      const walletAddressCompareResult = stringComparator(
        rewardA.rewardsAddress,
        rewardB.rewardsAddress,
        rewardsOrder === REWARD_ORDERS.ASCENDING
      );
      if (rewardsSortBy === REWARD_FIELDS.REWARD_AMOUNT) {
        if (rewardCompareResult === 0 && walletAddressCompareResult === 0) {
          return walletNameCompareResult;
        }
        if (rewardCompareResult === 0 && walletNameCompareResult === 0) {
          return walletAddressCompareResult;
        }
        return rewardCompareResult;
      }
      if (rewardsSortBy === REWARD_FIELDS.WALLET_NAME) {
        if (walletNameCompareResult === 0 && walletAddressCompareResult) {
          return rewardCompareResult;
        }
        if (rewardCompareResult === 0 && walletNameCompareResult === 0) {
          return walletAddressCompareResult;
        }
        return walletNameCompareResult;
      }
      if (rewardsSortBy === REWARD_FIELDS.REWARDS_ADDRESS) {
        if (walletAddressCompareResult === 0 && rewardCompareResult === 0) {
          return walletNameCompareResult;
        }
        if (walletAddressCompareResult === 0 && walletNameCompareResult === 0) {
          return rewardCompareResult;
        }
        return walletAddressCompareResult;
      }
      return 0;
    });
  };

  handleContentScroll = (evt: SyntheticEvent<HTMLElement>) => {
    this.setState({ contentScrollTop: evt.currentTarget.scrollTop });
  };

  render() {
    const { rewards, isLoading, onOpenWalletRewards } = this.props;
    const { rewardsOrder, rewardsSortBy, contentScrollTop } = this.state;
    const { intl } = this.context;
    const noRewards = !isLoading && ((rewards && !rewards.length) || !rewards);
    const showRewards = rewards && rewards.length > 0 && !isLoading;
    const sortedRewards = showRewards ? this.getSortedRewards() : [];
    const availableTableHeaders = [
      {
        name: REWARD_FIELDS.WALLET_NAME,
        title: intl.formatMessage(messages.tableHeaderWallet),
      },
      {
        name: REWARD_FIELDS.REWARDS_ADDRESS,
        title: intl.formatMessage(messages.tableHeaderRewardsAddress),
      },
      {
        name: REWARD_FIELDS.REWARD_AMOUNT,
        title: `${intl.formatMessage(
          messages.tableHeaderReward
        )} (${intl.formatMessage(globalMessages.adaUnit)})`,
      },
      {
        name: REWARD_FIELDS.ACTIONS,
        title: '',
      },
    ];
    const headerWrapperClasses = classNames([
      styles.headerWrapper,
      contentScrollTop > 10 ? styles.headerWrapperWithShadow : null,
    ]);

    if (isLoading) {
      return <LoadingStakePools />;
    }

    return (
      <div className={styles.component}>
        <div className={headerWrapperClasses}>
          <div className={styles.title}>
            {intl.formatMessage(messages.title)}
          </div>
        </div>
        <div
          className={styles.contentWrapper}
          onScroll={this.handleContentScroll}
        >
          <BorderedBox>
            {noRewards && (
              <div className={styles.noRewardsLabel}>
                {intl.formatMessage(messages.noRewards)}
              </div>
            )}

            {sortedRewards.length > 0 && (
              <table>
                <thead>
                  <tr>
                    {map(availableTableHeaders, (tableHeader) => {
                      const isSorted = tableHeader.name === rewardsSortBy;
                      const hideSorting =
                        tableHeader.name === REWARD_FIELDS.ACTIONS;
                      const sortIconClasses = classNames([
                        styles.sortIcon,
                        isSorted ? styles.sorted : null,
                        isSorted && rewardsOrder === 'asc'
                          ? styles.ascending
                          : null,
                        hideSorting ? styles.hideSorting : null,
                      ]);

                      return (
                        <th
                          key={tableHeader.name}
                          onClick={() =>
                            !hideSorting
                              ? this.handleRewardsSort(tableHeader.name)
                              : null
                          }
                          className={styles[tableHeader.name]}
                        >
                          {tableHeader.title}
                          <SVGInline
                            svg={sortIcon}
                            className={sortIconClasses}
                          />
                        </th>
                      );
                    })}
                  </tr>
                </thead>
                <tbody>
                  {map(sortedRewards, (reward, key) => {
                    const rewardWallet = get(reward, REWARD_FIELDS.WALLET_NAME);
                    const isRestoring = get(reward, REWARD_FIELDS.IS_RESTORING);
                    const syncingProgress = get(
                      reward,
                      REWARD_FIELDS.SYNCING_PROGRESS
                    );
                    const rewardAmount = get(
                      reward,
                      REWARD_FIELDS.REWARD_AMOUNT
                    ).toFormat(DECIMAL_PLACES_IN_ADA);
                    const rewardsAddress = get(
                      reward,
                      REWARD_FIELDS.REWARDS_ADDRESS
                    );
                    const onOpenWalletRewardsBind = () =>
                      !isRestoring ? onOpenWalletRewards(reward) : null;

                    const trClassName = !isRestoring ? styles.hasLink : null;
                    const detailsButtonStyles = classNames([
                      styles.actionButton,
                      styles.detailsButton,
                    ]);

                    return (
                      <tr
                        key={key}
                        onClick={onOpenWalletRewardsBind}
                        className={trClassName}
                      >
                        <td className={styles.rewardWallet}>{rewardWallet}</td>
                        <td className={styles.rewardsAddress}>
                          <Ellipsis string={rewardsAddress} />
                        </td>
                        <td className={styles.rewardAmount}>
                          {isRestoring ? '-' : rewardAmount}
                        </td>
                        <td className={styles.actions}>
                          {!isRestoring && (
                            <div className={detailsButtonStyles}>
                              {intl.formatMessage(messages.detailsButtonLabel)}
                            </div>
                          )}
                          {isRestoring && (
                            <div className={styles.syncingProgress}>
                              <div className={styles.actionButton}>
                                <SVGInline
                                  svg={tinySpinnerIcon}
                                  className={styles.syncingProgressIcon}
                                />
                                {intl.formatMessage(
                                  messages.syncingTooltipLabel,
                                  {
                                    syncingProgress,
                                  }
                                )}
                              </div>
                            </div>
                          )}
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            )}
          </BorderedBox>
          <div className={styles.note}>
            <div className={styles.noteContent}>
              <FormattedHTMLMessage {...messages.note} />
            </div>
          </div>
        </div>
      </div>
    );
  }

  handleRewardsSort = (newSortBy: string) => {
    const { rewardsOrder, rewardsSortBy } = this.state;
    let newRewardsOrder;
    if (rewardsSortBy === newSortBy) {
      // on same sort change order
      newRewardsOrder = rewardsOrder === 'asc' ? 'desc' : 'asc';
    } else {
      // on new sort instance, order by initial value 'descending'
      newRewardsOrder = 'desc';
    }

    this.setState({
      rewardsSortBy: newSortBy,
      rewardsOrder: newRewardsOrder,
    });
  };
}
