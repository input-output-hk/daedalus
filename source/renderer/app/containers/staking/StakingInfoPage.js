// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import StakingInfo from '../../components/staking/info/StakingInfo';
import StakingInfoCountdown from '../../components/staking/info/StakingInfoCountdown';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakingInfoPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { stores, actions } = this.props;
    const {
      decentralizationProgress,
      epochToFullyDecentralized,
      isFullyDecentralized,
    } = stores.networkStatus;
    const { stakingInfoWasOpen } = stores.staking;
    const { setStakingInfoWasOpen } = actions.staking;
    const { openExternalLink } = stores.app;
    if (!epochToFullyDecentralized) {
      return (
        <StakingInfo
          percentage={decentralizationProgress}
          onLearnMoreClick={openExternalLink}
        />
      );
    }
    return (
      <StakingInfoCountdown
        percentage={decentralizationProgress}
        onLearnMoreClick={openExternalLink}
        epoch={epochToFullyDecentralized}
        onSetStakingInfoWasOpen={setStakingInfoWasOpen.trigger}
        isFullyDecentralized={isFullyDecentralized}
        stakingInfoWasOpen={stakingInfoWasOpen}
      />
    );
  }
}
