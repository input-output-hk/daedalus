import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import StakingRewards from '../../components/staking/rewards/StakingRewards';
import type { InjectedProps } from '../../types/injectedPropsType';
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
class StakingRewardsPage extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  static defaultProps = {
    actions: null,
    stores: null,
  };
  handleLearnMoreClick = (event: React.SyntheticEvent<HTMLButtonElement>) => {
    event.persist();
    const { intl } = this.context;
    const learnMoreLinkUrl = intl.formatMessage(messages.learnMoreLinkUrl);
    this.props.stores.app.openExternalLink(learnMoreLinkUrl);
  };
  onOpenExternalLink = (rewardsAddress: string) => {
    const { app } = this.props.stores;
    const {
      environment: { network },
    } = app;
    const cardanoExplorerLink = `${getNetworkExplorerUrl(
      network
    )}/address/${rewardsAddress}`;
    this.props.stores.app.openExternalLink(cardanoExplorerLink);
  };
  handleCopyAddress = (copiedAddress: string) => {
    const address = ellipsis(copiedAddress, 15, 15);
    this.props.actions.wallets.copyAddress.trigger({
      address,
    });
  };

  render() {
    const {
      staking: { rewards },
      wallets,
    } = this.props.stores;
    const { requestCSVFile } = this.props.actions.staking;
    return (
      <StakingRewards
        rewards={rewards}
        isLoading={false}
        isExporting={wallets.generatingRewardsCsvInProgress}
        // @ts-ignore ts-migrate(2322) FIXME: Type '{ rewards: any; isLoading: false; isExportin... Remove this comment to see the full error message
        onLearnMoreClick={this.handleLearnMoreClick}
        onExportCsv={requestCSVFile.trigger}
        onCopyAddress={this.handleCopyAddress}
        onOpenExternalLink={this.onOpenExternalLink}
      />
    );
  }
}

export default StakingRewardsPage;
