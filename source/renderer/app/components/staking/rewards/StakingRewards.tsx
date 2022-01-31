import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { get, map } from 'lodash';
import classNames from 'classnames';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import CopyToClipboard from 'react-copy-to-clipboard';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import {
  bigNumberComparator,
  stringComparator,
} from '../../../utils/sortComparators';
import BorderedBox from '../../widgets/BorderedBox';
import LoadingSpinner from '../../widgets/LoadingSpinner';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/ascendi... Remove this comment to see the full error message
import sortIcon from '../../../assets/images/ascending.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/downloa... Remove this comment to see the full error message
import downloadIcon from '../../../assets/images/download-ic.inline.svg';
import type { Reward } from '../../../api/staking/types';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakingRewards.scss' or its ... Remove this comment to see the full error message
import styles from './StakingRewards.scss';
import globalMessages from '../../../i18n/global-messages';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/clipboa... Remove this comment to see the full error message
import iconCopy from '../../../assets/images/clipboard-ic.inline.svg';
import ButtonLink from '../../widgets/ButtonLink';
import { RewardAmount } from './RewardAmount';

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
    defaultMessage: '!!!Total rewards earned (ADA)',
    description: 'Table header "Reward" label on staking rewards page',
  },
  tableHeaderRewardsAddress: {
    id: 'staking.rewards.tableHeader.rewardsAddress',
    defaultMessage: '!!!Rewards address',
    description: 'Table header "Rewards address" label on staking rewards page',
  },
  tableHeaderDate: {
    id: 'staking.rewards.tableHeader.date',
    defaultMessage: '!!!Date',
    description: 'Table header "Date" label in exported csv file',
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
  actionViewInExplorer: {
    id: 'staking.rewards.actionViewInExplorer',
    defaultMessage: '!!!View in explorer',
    description: 'View in explorer button label on staking rewards page.',
  },
});
const REWARD_FIELDS = {
  WALLET_NAME: 'wallet',
  IS_RESTORING: 'isRestoring',
  SYNCING_PROGRESS: 'syncingProgress',
  REWARD: 'reward',
  REWARDS_ADDRESS: 'rewardsAddress',
};
const REWARD_ORDERS = {
  ASCENDING: 'asc',
  DESCENDING: 'desc',
};
const IS_EXPLORER_LINK_BUTTON_ENABLED = false;
type Props = {
  rewards: Array<Reward>;
  isLoading: boolean;
  isExporting: boolean;
  onExportCsv: (...args: Array<any>) => any;
  onCopyAddress: (...args: Array<any>) => any;
  onOpenExternalLink: (...args: Array<any>) => any;
};
type State = {
  rewardsOrder: string;
  rewardsSortBy: string;
  contentScrollTop: number;
};

@observer
class StakingRewards extends Component<Props, State> {
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
      contentScrollTop: 0,
    };
  }

  handleExportCsv = (
    availableTableHeaders: Array<any>,
    sortedRewards: Array<Reward>
  ) => {
    const { onExportCsv } = this.props;
    const { intl } = this.context;
    const exportedHeader = [
      ...availableTableHeaders.map((header) => header.title),
      intl.formatMessage(messages.tableHeaderDate),
    ];
    const date = new Date().toISOString();
    const exportedBody = sortedRewards.map((reward) => {
      const rewardWallet = get(reward, REWARD_FIELDS.WALLET_NAME);
      const isRestoring = get(reward, REWARD_FIELDS.IS_RESTORING);
      const rewardAmount = get(reward, REWARD_FIELDS.REWARD).toFormat(
        DECIMAL_PLACES_IN_ADA
      );
      const rewardsAddress = get(reward, REWARD_FIELDS.REWARDS_ADDRESS);
      return [
        rewardWallet,
        rewardsAddress,
        isRestoring ? '-' : rewardAmount,
        date,
      ];
    });
    const exportedContent = [exportedHeader, ...exportedBody];
    onExportCsv({
      fileContent: exportedContent,
      filenamePrefix: intl.formatMessage(messages.csvFilenamePrefix),
    });
  };
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
      const walletAddressCompareResult = stringComparator(
        rewardA.rewardsAddress,
        rewardB.rewardsAddress,
        rewardsOrder === REWARD_ORDERS.ASCENDING
      );

      if (rewardsSortBy === REWARD_FIELDS.REWARD) {
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
  handleContentScroll = (evt: React.SyntheticEvent<HTMLElement>) => {
    this.setState({
      contentScrollTop: evt.currentTarget.scrollTop,
    });
  };

  render() {
    const {
      rewards,
      isLoading,
      isExporting,
      onCopyAddress,
      onOpenExternalLink,
    } = this.props;
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
        name: REWARD_FIELDS.REWARD,
        title: `${intl.formatMessage(
          messages.tableHeaderReward
        )} (${intl.formatMessage(globalMessages.adaUnit)})`,
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
    const exportCsvButtonClasses = classNames(['flat', styles.actionButton]);
    const explorerButtonClasses = classNames([
      'flat',
      styles.actionExplorerLink,
    ]);
    const headerWrapperClasses = classNames([
      styles.headerWrapper,
      contentScrollTop > 10 ? styles.headerWrapperWithShadow : null,
    ]);
    return (
      <div className={styles.component}>
        <div className={headerWrapperClasses}>
          <div className={styles.title}>
            {intl.formatMessage(messages.title)}
          </div>
          {!noRewards && (
            <Button
              className={exportCsvButtonClasses}
              label={exportCsvButtonLabel}
              onClick={() =>
                this.handleExportCsv(availableTableHeaders, sortedRewards)
              }
              skin={ButtonSkin}
            />
          )}
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
                    const rewardWallet = get(reward, REWARD_FIELDS.WALLET_NAME);
                    const isRestoring = get(reward, REWARD_FIELDS.IS_RESTORING);
                    const syncingProgress = get(
                      reward,
                      REWARD_FIELDS.SYNCING_PROGRESS
                    );
                    const rewardAmount = get(
                      reward,
                      REWARD_FIELDS.REWARD
                    ).toFormat(DECIMAL_PLACES_IN_ADA);
                    const rewardsAddress = get(
                      reward,
                      REWARD_FIELDS.REWARDS_ADDRESS
                    );
                    return (
                      <tr key={key}>
                        <td className={styles.rewardWallet}>{rewardWallet}</td>
                        <td className={styles.rewardsAddress}>
                          {rewardsAddress && (
                            <div>
                              <CopyToClipboard
                                text={rewardsAddress}
                                onCopy={() => onCopyAddress(rewardsAddress)}
                              >
                                <div className={styles.addressContainer}>
                                  <span className={styles.address}>
                                    {rewardsAddress}
                                  </span>
                                  <span className={styles.copyAddress}>
                                    <SVGInline
                                      svg={iconCopy}
                                      className={styles.copyIcon}
                                    />
                                  </span>
                                </div>
                              </CopyToClipboard>
                              {IS_EXPLORER_LINK_BUTTON_ENABLED && (
                                <ButtonLink
                                  // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
                                  className={explorerButtonClasses}
                                  onClick={() =>
                                    onOpenExternalLink(rewardsAddress)
                                  }
                                  skin={ButtonSkin}
                                  label={intl.formatMessage(
                                    messages.actionViewInExplorer
                                  )}
                                  linkProps={{
                                    className: styles.externalLink,
                                    hasIconBefore: false,
                                    hasIconAfter: true,
                                  }}
                                />
                              )}
                            </div>
                          )}
                        </td>
                        <td className={styles.rewardAmount}>
                          {isRestoring ? (
                            '-'
                          ) : (
                            <RewardAmount amount={rewardAmount} />
                          )}
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

export default StakingRewards;
