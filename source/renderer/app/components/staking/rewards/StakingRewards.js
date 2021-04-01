// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { get, map } from 'lodash';
import classNames from 'classnames';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import {
  bigNumberComparator,
  stringComparator,
  dateComparator,
} from '../../../utils/sortComparators';
import BorderedBox from '../../widgets/BorderedBox';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import sortIcon from '../../../assets/images/ascending.inline.svg';
import type { Reward } from '../../../api/staking/types';
import styles from './StakingRewards.scss';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';

const messages = defineMessages({
  title: {
    id: 'staking.rewards.title',
    defaultMessage: '!!!Earned delegation rewards',
    description:
      'Title "Earned delegation rewards" label on the staking rewards page.',
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
  tableHeaderDate: {
    id: 'staking.rewards.tableHeader.date',
    defaultMessage: '!!!Date',
    description: 'Table header "Date" label on staking rewards page',
  },
  tableHeaderPool: {
    id: 'staking.rewards.tableHeader.pool',
    defaultMessage: '!!!Stake pool',
    description: 'Table header "Stake pool" label on staking rewards page',
  },
  tableHeaderWallet: {
    id: 'staking.rewards.tableHeader.wallet',
    defaultMessage: '!!!Wallet',
    description: 'Table header "Wallet" label on staking rewards page',
  },
  tableHeaderReward: {
    id: 'staking.rewards.tableHeader.reward',
    defaultMessage: '!!!Total rewards earned',
    description: 'Table header "Reward" label on staking rewards page',
  },
  learnMoreButtonLabel: {
    id: 'staking.rewards.learnMore.ButtonLabel',
    defaultMessage: '!!!Learn more',
    description: 'Label for "Learn more" button on staking rewards page',
  },
  note: {
    id: 'staking.rewards.note',
    defaultMessage:
      '!!!Rewards earned by delegating your stake are automatically collected into your reward account and added to your wallet balance.',
    description: 'Rewards description text on staking rewards page',
  },
});

const REWARD_FIELDS = {
  WALLET_NAME: 'wallet',
  IS_RESTORING: 'isRestoring',
  POOL: 'pool',
  NAME: 'name',
  TICKER: 'ticker',
  REWARD: 'reward',
  DATE: 'date',
};

const REWARD_ORDERS = {
  ASCENDING: 'asc',
  DESCENDING: 'desc',
};

type Props = {
  rewards: Array<Reward>,
  isLoading: boolean,
  onLearnMoreClick: Function,
};

type State = {
  rewardsOrder: string,
  rewardsSortBy: string,
};

@observer
export default class StakingRewards extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    isLoading: false,
  };

  constructor(props: Props) {
    super(props);
    this.state = {
      rewardsOrder: REWARD_ORDERS.DESCENDING,
      rewardsSortBy: REWARD_FIELDS.DATE,
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
        rewardA.wallet,
        rewardB.wallet,
        rewardsOrder === REWARD_ORDERS.ASCENDING
      );
      const poolCompareResult = stringComparator(
        rewardA.pool.name,
        rewardB.pool.name,
        rewardsOrder === REWARD_ORDERS.ASCENDING
      );
      const dateCompareResult = dateComparator(
        rewardA.date,
        rewardB.date,
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
      if (rewardsSortBy === REWARD_FIELDS.DATE) {
        if (dateCompareResult === 0) {
          return walletNameCompareResult;
        }
        return dateCompareResult;
      }
      if (rewardsSortBy === REWARD_FIELDS.POOL) {
        if (poolCompareResult === 0) {
          return walletNameCompareResult;
        }
        return poolCompareResult;
      }

      return 0;
    });
  };

  render() {
    const { rewards, isLoading, onLearnMoreClick } = this.props;
    const { rewardsOrder, rewardsSortBy } = this.state;
    const { intl } = this.context;
    const noRewards = !isLoading && ((rewards && !rewards.length) || !rewards);
    const showRewards = rewards && rewards.length > 0 && !isLoading;
    const sortedRewards = showRewards ? this.getSortedRewards() : [];
    const availableTableHeaders = [
      {
        name: REWARD_FIELDS.DATE,
        title: intl.formatMessage(messages.tableHeaderDate),
      },
      {
        name: REWARD_FIELDS.POOL,
        title: intl.formatMessage(messages.tableHeaderPool),
      },
      {
        name: REWARD_FIELDS.WALLET_NAME,
        title: intl.formatMessage(messages.tableHeaderWallet),
      },
      {
        name: REWARD_FIELDS.REWARD,
        title: intl.formatMessage(messages.tableHeaderReward),
      },
    ];

    return (
      <div className={styles.component}>
        <div className={styles.headerWrapper}>
          <div className={styles.title}>
            {intl.formatMessage(messages.title)}
          </div>
          {!noRewards && (
            <div className={styles.actionLabel}>
              {intl.formatMessage(messages.exportButtonLabel)}
            </div>
          )}
        </div>

        <BorderedBox>
          {noRewards && (
            <div className={styles.noRewardsLabel}>
              {intl.formatMessage(messages.noRewards)}
            </div>
          )}

          {sortedRewards && (
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
                        onClick={() => this.handleRewardsSort(tableHeader.name)}
                      >
                        {tableHeader.title}
                        <SVGInline svg={sortIcon} className={sortIconClasses} />
                      </th>
                    );
                  })}
                </tr>
              </thead>
              <tbody>
                {map(sortedRewards, (reward, key) => {
                  const rewardDate = get(reward, REWARD_FIELDS.DATE, '');
                  const rewardPoolTicker = get(
                    reward,
                    [REWARD_FIELDS.POOL, REWARD_FIELDS.TICKER],
                    ''
                  );
                  const rewardPoolName = get(
                    reward,
                    [REWARD_FIELDS.POOL, REWARD_FIELDS.NAME],
                    ''
                  );
                  const rewardWallet = get(
                    reward,
                    REWARD_FIELDS.WALLET_NAME,
                    ''
                  );
                  const isRestoring = get(reward, REWARD_FIELDS.IS_RESTORING);
                  const rewardAmount = get(
                    reward,
                    REWARD_FIELDS.REWARD
                  ).toFormat(DECIMAL_PLACES_IN_ADA);

                  return (
                    <tr key={key}>
                      <td>{rewardDate}</td>
                      <td>
                        <p>
                          <span className={styles.stakePoolReference}>
                            [{rewardPoolTicker}]
                          </span>{' '}
                          {rewardPoolName}
                        </p>
                      </td>
                      <td>{rewardWallet}</td>
                      <td>{isRestoring ? '-' : `${rewardAmount} ADA`}</td>
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
          <div className={styles.asterisk}>*</div>
          <div className={styles.noteContent}>
            <FormattedHTMLMessage {...messages.note} />
            <Link
              className={styles.externalLink}
              onClick={onLearnMoreClick}
              label={intl.formatMessage(messages.learnMoreButtonLabel)}
              skin={LinkSkin}
            />
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
