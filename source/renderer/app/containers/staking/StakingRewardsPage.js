// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { generateFileNameWithTimestamp } from '../../../../common/utils/files';
import StakingRewards from '../../components/staking/rewards/StakingRewards';
import StakingRewardsForIncentivizedTestnet from '../../components/staking/rewards/StakingRewardsForIncentivizedTestnet';
import type { InjectedProps } from '../../types/injectedPropsType';
import type { CsvRecord } from '../../../../common/types/rewards-csv-request.types';

const messages = defineMessages({
  learnMoreLinkUrl: {
    id: 'staking.rewards.learnMore.linkUrl',
    defaultMessage: '!!!https://staking.cardano.org/',
    description: '"Learn more" link URL in the staking rewards page',
  },
});

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakingRewardsPage extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = { actions: null, stores: null };

  handleLearnMoreClick = (event: SyntheticEvent<HTMLButtonElement>) => {
    event.persist();
    const { intl } = this.context;
    const learnMoreLinkUrl = intl.formatMessage(messages.learnMoreLinkUrl);
    this.props.stores.app.openExternalLink(learnMoreLinkUrl);
  };

  onExportCsv = (rewards: Array<CsvRecord>) => {
    const {
      actions: { wallets },
    } = this.props;
    const filePath = global.dialog.showSaveDialog({
      defaultPath: generateFileNameWithTimestamp({
        prefix: 'Rewards',
        extension: 'csv',
        isUTC: true,
      }),
      filters: [
        {
          extensions: ['csv'],
        },
      ],
    });

    // if cancel button is clicked or path is empty
    if (!filePath) return;

    wallets.generateRewardsCsv.trigger({ rewards, filePath });
  };

  render() {
    const {
      staking: { rewards, rewardsForIncentivizedTestnet },
      networkStatus,
      wallets,
    } = this.props.stores;

    if (networkStatus.isIncentivizedTestnet) {
      return (
        <StakingRewardsForIncentivizedTestnet
          rewards={rewardsForIncentivizedTestnet}
          isLoading={false}
          isExporting={wallets.generatingRewardsCsvInProgress}
          onLearnMoreClick={this.handleLearnMoreClick}
          onExportCsv={this.onExportCsv}
        />
      );
    }
    return (
      <StakingRewards
        rewards={rewards}
        isLoading={false}
        onLearnMoreClick={this.handleLearnMoreClick}
      />
    );
  }
}
