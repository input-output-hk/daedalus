// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { get, map, orderBy } from 'lodash';
import classNames from 'classnames';
import { BigNumber } from 'bignumber.js';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
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
    defaultMessage: '!!!Reward',
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
      '!!!<p>Rewards earned by delegating your stake are automatically collected into your reward account.</p><p>Rewards earned on the Incentivized Testnet are not added to your Rewards wallet balance. They will be paid to you in real ada on the Cardano mainnet after the end of the Incentivized Testnet.</p><p>If you are using funds from this wallet to operate a stake pool, the rewards displayed here may include your pledged stake, which will not be counted when reward balances are paid out on the Cardano mainnet.</p>',
    description: 'Rewards description text on staking rewards page',
  },
});

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

  constructor() {
    super();
    this.state = {
      rewardsOrder: 'desc',
      rewardsSortBy: 'date',
    };
  }

  render() {
    const { rewardsOrder, rewardsSortBy } = this.state;
    const { rewards, isLoading, onLearnMoreClick } = this.props;

    const { intl } = this.context;
    const noRewards = !isLoading && ((rewards && !rewards.length) || !rewards);
    const showRewards = rewards && rewards.length > 0 && !isLoading;

    let sortedRewards;
    if (showRewards) {
      sortedRewards = orderBy(
        rewards,
        rewardsSortBy === 'pool' ? 'pool.name' : rewardsSortBy,
        rewardsOrder
      );
    }

    const availableTableHeaders = [
      {
        name: 'date',
        title: intl.formatMessage(messages.tableHeaderDate),
      },
      {
        name: 'pool',
        title: intl.formatMessage(messages.tableHeaderPool),
      },
      {
        name: 'wallet',
        title: intl.formatMessage(messages.tableHeaderWallet),
      },
      {
        name: 'reward',
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
                  {map(availableTableHeaders, tableHeader => {
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
                  const rewardDate = get(reward, 'date', '');
                  const rewardPoolTicker = get(reward, ['pool', 'ticker'], '');
                  const rewardPoolName = get(reward, ['pool', 'name'], '');
                  const rewardWallet = get(reward, 'wallet', '');
                  const rewardAmount = get(reward, 'reward', '');
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
                      <td>
                        {new BigNumber(rewardAmount).toFormat(
                          DECIMAL_PLACES_IN_ADA
                        )}{' '}
                        ADA
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
