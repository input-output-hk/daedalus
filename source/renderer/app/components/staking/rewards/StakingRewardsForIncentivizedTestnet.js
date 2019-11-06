// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { get, map, orderBy } from 'lodash';
import classNames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { StakingPageScrollContext } from '../layouts/StakingWithNavigation';
import BorderedBox from '../../widgets/BorderedBox';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import sortIcon from '../../../assets/images/ascending.inline.svg';
import externalLinkIcon from '../../../assets/images/link-ic.inline.svg';
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
  learnMoreButtonLabel: {
    id: 'staking.rewards.learnMore.ButtonLabel',
    defaultMessage: '!!!Learn more',
    description: 'Label for "Learn more" button on staking rewards page',
  },
  note: {
    id: 'staking.rewards.note',
    defaultMessage:
      '!!!Rewards from stake delegation are automatically collected into your reward account.',
    description: 'Rewards description text on staking rewards page',
  },
});

type Props = {
  rewards: Array<RewardForIncentivizedTestnet>,
  isLoading: boolean,
  isExporting: boolean,
  onLearnMoreClick: Function,
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

  constructor() {
    super();
    this.state = {
      rewardsOrder: 'desc',
      rewardsSortBy: 'wallet',
    };
  }

  handleExportCsv = (
    availableTableHeaders: Array<any>,
    sortedRewards: Array<RewardForIncentivizedTestnet>
  ) => {
    const { onExportCsv } = this.props;
    const exportedHeader = availableTableHeaders.map(header => header.title);
    const exportedBody = sortedRewards.map(reward => {
      const rewardWallet = get(reward, 'wallet', '');
      const rewardAmount = get(reward, 'reward', 0);

      return [rewardWallet, `${rewardAmount} ADA`];
    });
    const exportedContent = [exportedHeader, ...exportedBody];

    onExportCsv(exportedContent);
  };

  render() {
    const { rewards, isLoading, isExporting, onLearnMoreClick } = this.props;
    const { rewardsOrder, rewardsSortBy } = this.state;
    const { intl } = this.context;
    const noRewards = !isLoading && ((rewards && !rewards.length) || !rewards);
    const showRewards = rewards && rewards.length > 0 && !isLoading;
    const sortedRewards = showRewards
      ? orderBy(rewards, rewardsSortBy, rewardsOrder)
      : [];
    const availableTableHeaders = [
      {
        name: 'wallet',
        title: intl.formatMessage(messages.tableHeaderWallet),
      },
      {
        name: 'reward',
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

    return (
      <StakingPageScrollContext.Consumer>
        {context => (
          <div className={styles.component}>
            <div className={styles.headerWrapper}>
              <div className={styles.title}>
                {intl.formatMessage(messages.title)}
              </div>
              {!noRewards && (
                <Button
                  className={classNames([
                    'primary',
                    styles.actionButton,
                    context.scrollTop > 10 ? styles.actionButtonFaded : null,
                  ])}
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
                      const rewardWallet = get(reward, 'wallet', '');
                      const rewardAmount = get(reward, 'reward', 0);

                      return (
                        <tr key={key}>
                          <td>{rewardWallet}</td>
                          <td>{rewardAmount} ADA</td>
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
              <span>* {intl.formatMessage(messages.note)} </span>
              <button onClick={onLearnMoreClick}>
                {intl.formatMessage(messages.learnMoreButtonLabel)}
                <SVGInline svg={externalLinkIcon} />
              </button>
              <span>.</span>
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
