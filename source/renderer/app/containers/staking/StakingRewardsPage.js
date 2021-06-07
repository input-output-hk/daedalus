// @flow
import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import StakingRewards from '../../components/staking/rewards/StakingRewards';
import StakingRewardsHistoryDialog from '../../components/staking/rewards/StakingRewardsHistoryDialog';
import StakingRewardsHistoryDialogContainer from './dialogs/StakingRewardsHistoryDialogContainer';
import type { InjectedProps } from '../../types/injectedPropsType';
import type { Reward } from '../../api/staking/types';

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

  handleOpenWalletRewards = (reward: Reward) => {
    const { updateDataForActiveDialog, open } = this.props.actions.dialogs;
    open.trigger({ dialog: StakingRewardsHistoryDialog });
    updateDataForActiveDialog.trigger({
      data: { reward },
    });
  };

  render() {
    const {
      staking: { rewards },
      uiDialogs,
    } = this.props.stores;

    return (
      <Fragment>
        <StakingRewards
          rewards={rewards}
          isLoading={false}
          onLearnMoreClick={this.handleLearnMoreClick}
          onOpenWalletRewards={this.handleOpenWalletRewards}
        />
        {uiDialogs.isOpen(StakingRewardsHistoryDialog) ? (
          <StakingRewardsHistoryDialogContainer />
        ) : null}
      </Fragment>
    );
  }
}
