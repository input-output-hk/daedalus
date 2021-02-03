// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { get, map } from 'lodash';
import classNames from 'classnames';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import moment from 'moment';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import { StakingPageScrollContext } from '../layouts/StakingWithNavigation';
import {
  bigNumberComparator,
  stringComparator,
} from '../../../utils/sortComparators';
import BorderedBox from '../../widgets/BorderedBox';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import sortIcon from '../../../assets/images/ascending.inline.svg';
import downloadIcon from '../../../assets/images/download-ic.inline.svg';
import type { RewardForIncentivizedTestnet } from '../../../api/staking/types';
import styles from './StakingRewardsForIncentivizedTestnet.scss';

const messages = defineMessages({
  title: {
    id: 'staking.rewards.title',
    defaultMessage: '!!!Earned delegation rewards',
    description:
      'Title "Earned delegation rewards" label on the staking rewards page.',
  },
  csvFilenamePrefix: {
    id: 'staking.rewards.csvFilenamePrefix',
    defaultMessage: '!!!Rewards',
    description:
      'Filename prefix for the "Export CSV" on the staking rewards page.',
  },
  exportButtonLabel: {
    id: 'staking.rewards.exportButtonLabel',
    defaultMessage: '!!!Export CSV',
    description:
      'Label for the "Export CSV" button on the staking rewards page.',
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
    defaultMessage: '!!!Reward',
    description: 'Table header "Reward" label on staking rewards page',
  },
  tableHeaderDate: {
    id: 'staking.rewards.tableHeader.date',
    defaultMessage: '!!!Date',
    description: 'Table header "Date" label in exported csv file',
  },
  learnMoreButtonLabel: {
    id: 'staking.rewards.learnMore.ButtonLabel',
    defaultMessage: '!!!Learn more',
    description: 'Label for "Learn more" button on staking rewards page',
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
});

const REWARD_FIELDS = {
  WALLET_NAME: 'wallet',
  IS_RESTORING: 'isRestoring',
  SYNCING_PROGRESS: 'syncingProgress',
  REWARD: 'reward',
};

const REWARD_ORDERS = {
  ASCENDING: 'asc',
  DESCENDING: 'desc',
};

type Props = {
  rewards: Array<RewardForIncentivizedTestnet>,
  isLoading: boolean,
  isExporting: boolean,
  onExportCsv: Function,
};

type State = {
  rewardsOrder: string,
  rewardsSortBy: string,
};

@observer
export default class StakingRewardsForIncentivizedTestnet extends Component<
  Props,
  State
> {
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
      rewardsOrder: REWARD_ORDERS.DESCENDING,
      rewardsSortBy: REWARD_FIELDS.WALLET_NAME,
    };
  }

  handleExportCsv = (
    availableTableHeaders: Array<any>,
    sortedRewards: Array<RewardForIncentivizedTestnet>
  ) => {
    const { onExportCsv } = this.props;
    const { intl } = this.context;
    const exportedHeader = [
      ...availableTableHeaders.map((header) => header.title),
      intl.formatMessage(messages.tableHeaderDate),
    ];
    const date = `${moment().utc().format('YYYY-MM-DDTHHmmss.0SSS')}Z`;
    const exportedBody = sortedRewards.map((reward) => {
      const rewardWallet = get(reward, REWARD_FIELDS.WALLET_NAME);
      const isRestoring = get(reward, REWARD_FIELDS.IS_RESTORING);
      const rewardAmount = get(reward, REWARD_FIELDS.REWARD).toFormat(
        DECIMAL_PLACES_IN_ADA
      );
      return [rewardWallet, isRestoring ? '-' : `${rewardAmount} ADA`, date];
    });
    const exportedContent = [exportedHeader, ...exportedBody];

    onExportCsv({
      fileContent: exportedContent,
      filenamePrefix: intl.formatMessage(messages.csvFilenamePrefix),
    });
  };

  getSortedRewards = (): Array<RewardForIncentivizedTestnet> => {
    const { rewards } = this.props;
    const { rewardsOrder, rewardsSortBy } = this.state;
    return rewards
      .slice()
      .sort(
        (
          rewardA: RewardForIncentivizedTestnet,
          rewardB: RewardForIncentivizedTestnet
        ) => {
          const rewardCompareResult = bigNumberComparator(
            rewardA.reward,
            rewardB.reward,
            rewardsOrder === REWARD_ORDERS.ASCENDING
          );
          const walletNameCompareResult = stringComparator(
            rewardA.wallet,
            rewardB.wallet,
            rewardsOrder === REWARD_ORDERS.ASCENDING
          );
          if (rewardsSortBy === REWARD_FIELDS.REWARD) {
            if (rewardCompareResult === 0) {
              return walletNameCompareResult;
            }
            return rewardCompareResult;
          }
          if (rewardsSortBy === REWARD_FIELDS.WALLET_NAME) {
            if (walletNameCompareResult === 0) {
              return rewardCompareResult;
            }
            return walletNameCompareResult;
          }
          return 0;
        }
      );
  };

  render() {
    const {
      rewards,
      isLoading,
      isExporting,
      // onLearnMoreClick,
    } = this.props;
    const { rewardsOrder, rewardsSortBy } = this.state;
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
        name: REWARD_FIELDS.REWARD,
        title: intl.formatMessage(messages.tableHeaderReward),
      },
    ];
    const exportCsvButtonLabel = isExporting ? (
      <div className={styles.exportingSpinnerWrapper}>
        <LoadingSpinner />
      </div>
    ) : (
      <>
        <div className={styles.actionLabel}>
          {intl.formatMessage(messages.exportButtonLabel)}
        </div>
        <SVGInline svg={downloadIcon} className={styles.downloadIcon} />
      </>
    );
    const exportCsvButtonClasses = (ctx) =>
      classNames([
        'primary',
        styles.actionButton,
        ctx.scrollTop > 10 ? styles.actionButtonFaded : null,
      ]);

    return (
      <StakingPageScrollContext.Consumer>
        {(context) => (
          <div className={styles.component}>
            <div className={styles.headerWrapper}>
              <div className={styles.title}>
                {intl.formatMessage(messages.title)}
              </div>
              {!noRewards && (
                <Button
                  className={exportCsvButtonClasses(context)}
                  label={exportCsvButtonLabel}
                  onClick={() =>
                    this.handleExportCsv(availableTableHeaders, sortedRewards)
                  }
                  skin={ButtonSkin}
                />
              )}
            </div>
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
                        const sortIconClasses = classNames([
                          styles.sortIcon,
                          isSorted ? styles.sorted : null,
                          isSorted && rewardsOrder === 'asc'
                            ? styles.ascending
                            : null,
                        ]);

                        return (
                          <th
                            key={tableHeader.name}
                            onClick={() =>
                              this.handleRewardsSort(tableHeader.name)
                            }
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
                      const rewardWallet = get(
                        reward,
                        REWARD_FIELDS.WALLET_NAME
                      );
                      const isRestoring = get(
                        reward,
                        REWARD_FIELDS.IS_RESTORING
                      );
                      const syncingProgress = get(
                        reward,
                        REWARD_FIELDS.SYNCING_PROGRESS
                      );
                      const rewardAmount = get(
                        reward,
                        REWARD_FIELDS.REWARD
                      ).toFormat(DECIMAL_PLACES_IN_ADA);

                      return (
                        <tr key={key}>
                          <td>{rewardWallet}</td>
                          <td>
                            {isRestoring ? '-' : `${rewardAmount} ADA`}
                            {isRestoring && (
                              <div className={styles.syncingProgress}>
                                <PopOver
                                  content={intl.formatMessage(
                                    messages.syncingTooltipLabel,
                                    {
                                      syncingProgress,
                                    }
                                  )}
                                >
                                  <LoadingSpinner medium />
                                </PopOver>
                              </div>
                            )}
                          </td>
                        </tr>
                      );
                    })}
                  </tbody>
                </table>
              )}

              {isLoading && (
                <div className={styles.loadingSpinnerWrapper}>
                  <LoadingSpinner />
                </div>
              )}
            </BorderedBox>

            <div className={styles.note}>
              <div className={styles.noteContent}>
                <FormattedHTMLMessage {...messages.note} />
              </div>
            </div>
          </div>
        )}
      </StakingPageScrollContext.Consumer>
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
