// @flow
import React, { Component } from 'react';
import { toJS } from 'mobx';
import { observer, inject } from 'mobx-react';
import StakingRewardsHistoryDialog from '../../../components/staking/rewards/StakingRewardsHistoryDialog';
import { ellipsis } from '../../../utils/strings';
import type { InjectedProps } from '../../../types/injectedPropsType';
import { generateSupportRequestLink } from '../../../../../common/utils/reporting';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakingRewardsHistoryDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  componentDidMount() {
    this.fetchStakingRewards();
  }

  fetchStakingRewards() {
    const { stores, actions } = this.props;
    const { reward } = stores.uiDialogs.dataForActiveDialog;
    actions.staking.fetchRewardsHistory.trigger({
      address: reward.rewardsAddress,
    });
  }

  handleCopyAddress = (copiedAddress: string) => {
    const address = ellipsis(copiedAddress, 15, 15);
    this.props.actions.wallets.copyAddress.trigger({ address });
  };

  handleNoDataClick = async (url: string) => {
    const { profile, app } = this.props.stores;
    const { currentLocale } = profile;
    const { environment, openExternalLink } = app;
    const supportUrl = generateSupportRequestLink(
      url,
      environment,
      currentLocale
    );
    openExternalLink(supportUrl);
  };

  render() {
    const { stores, actions } = this.props;
    const { reward } = stores.uiDialogs.dataForActiveDialog;
    const { closeActiveDialog } = actions.dialogs;
    const { currentDateFormat, currentTheme } = stores.profile;
    const { openExternalLink } = stores.app;
    const {
      isFetchingRewardsHistory,
      rewardsHistory: rewardsHistoryObject,
    } = stores.staking;
    const { requestRewardsHistoryCSVFile } = actions.staking;
    const rewardsHistory = rewardsHistoryObject[reward.rewardsAddress] || [];
    if (!reward) return null;
    return (
      <StakingRewardsHistoryDialog
        currentDateFormat={currentDateFormat}
        currentTheme={currentTheme}
        isFetchingRewardsHistory={isFetchingRewardsHistory}
        onClose={closeActiveDialog.trigger}
        onExportCSV={requestRewardsHistoryCSVFile.trigger}
        onOpenExternalLink={openExternalLink}
        onNoDataClick={this.handleNoDataClick}
        reward={reward}
        rewardsHistory={toJS(rewardsHistory)}
        onCopyAddress={this.handleCopyAddress}
      />
    );
  }
}
