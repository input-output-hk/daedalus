// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import StakingRewards from '../../components/staking/rewards/StakingRewards';
import StakingRewardsForIncentivizedTestnet from '../../components/staking/rewards/StakingRewardsForIncentivizedTestnet';
import type { InjectedProps } from '../../types/injectedPropsType';

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

  render() {
    const {
      staking: { rewards, rewardsForIncentivizedTestnet },
      networkStatus,
    } = this.props.stores;

    if (networkStatus.isIncentivizedTestnet) {
      return (
        <StakingRewardsForIncentivizedTestnet
          rewards={rewardsForIncentivizedTestnet}
          isLoading={false}
          onLearnMoreClick={this.handleLearnMoreClick}
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
