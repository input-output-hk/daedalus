// @flow
import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import StakingRewards from '../../components/staking/rewards/StakingRewards';
import StakingRewardsDialog from '../../components/staking/rewards/StakingRewardsDialog';
import StakingRewardsForIncentivizedTestnet from '../../components/staking/rewards/StakingRewardsForIncentivizedTestnet';
import StakingRewardsDialogContainer from './dialogs/StakingRewardsDialogContainer';
import type { InjectedProps } from '../../types/injectedPropsType';
import type { RewardForIncentivizedTestnet } from '../../api/staking/types';
import { ellipsis } from '../../utils/strings';
import { getNetworkExplorerUrl } from '../../utils/network';

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

  handleOpenWalletRewards = (reward: RewardForIncentivizedTestnet) => {
    const { updateDataForActiveDialog, open } = this.props.actions.dialogs;
    open.trigger({ dialog: StakingRewardsDialog });
    updateDataForActiveDialog.trigger({
      data: { reward },
    });
  };

  handleOpenExternalLink = (rewardsAddress: string) => {
    const { app } = this.props.stores;
    const {
      environment: { network, rawNetwork },
    } = app;
    const cardanoExplorerLink = `${getNetworkExplorerUrl(
      network,
      rawNetwork
    )}/address/${rewardsAddress}`;
    this.props.stores.app.openExternalLink(cardanoExplorerLink);
  };

  handleCopyAddress = (copiedAddress: string) => {
    const address = ellipsis(copiedAddress, 15, 15);
    this.props.actions.wallets.copyAddress.trigger({ address });
  };

  render() {
    const {
      staking: { rewards, rewardsForIncentivizedTestnet },
      networkStatus,
      uiDialogs,
    } = this.props.stores;
    const { isIncentivizedTestnet, isShelleyTestnet } = global;
    const {
      isMainnet,
      isSelfnode,
      isStaging,
      isTestnet,
      isTest,
    } = networkStatus.environment;

    const stakingComponent =
      isMainnet ||
      isStaging ||
      isTestnet ||
      isIncentivizedTestnet ||
      isShelleyTestnet ||
      isSelfnode ||
      isTest ? (
        <StakingRewardsForIncentivizedTestnet
          rewards={rewardsForIncentivizedTestnet}
          isLoading={false}
          onLearnMoreClick={this.handleLearnMoreClick}
          onCopyAddress={this.handleCopyAddress}
          onOpenExternalLink={this.handleOpenExternalLink}
          onOpenWalletRewards={this.handleOpenWalletRewards}
        />
      ) : (
        <StakingRewards
          rewards={rewards}
          isLoading={false}
          onLearnMoreClick={this.handleLearnMoreClick}
        />
      );

    return (
      <Fragment>
        {stakingComponent}
        {uiDialogs.isOpen(StakingRewardsDialog) ? (
          <StakingRewardsDialogContainer />
        ) : null}
      </Fragment>
    );
  }
}
