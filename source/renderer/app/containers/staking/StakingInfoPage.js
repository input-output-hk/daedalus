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
    const { stores } = this.props;
    const {
      decentralizationProgress,
      epochToFullyDecentralized,
    } = stores.networkStatus;
    const { openExternalLink } = stores.app;
    return epochToFullyDecentralized ? (
      <StakingInfoCountdown
        percentage={decentralizationProgress}
        onLearnMoreClick={openExternalLink}
        epoch={epochToFullyDecentralized.epochNumber}
      />
    ) : (
      <StakingInfo
        percentage={decentralizationProgress}
        onLearnMoreClick={openExternalLink}
      />
    );
  }
}
