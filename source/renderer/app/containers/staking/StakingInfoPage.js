// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import StakingInfoCountdown from '../../components/staking/info/StakingInfoCountdown';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakingInfoPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { stores, actions } = this.props;
    const { isAlonzoActivated, alonzoEraEpoch } = stores.networkStatus;
    const { stakingInfoWasOpen } = stores.staking;
    const { setStakingInfoWasOpen } = actions.staking;
    const { openExternalLink } = stores.app;
    return (
      <StakingInfoCountdown
        percentage={decentralizationProgress}
        onLearnMoreClick={openExternalLink}
        epoch={alonzoEraEpoch}
        onSetStakingInfoWasOpen={setStakingInfoWasOpen.trigger}
        isAlonzoActivated={isAlonzoActivated}
        stakingInfoWasOpen={stakingInfoWasOpen}
      />
    );
  }
}
