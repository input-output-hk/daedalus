import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import StakingInfoCountdown from '../../components/staking/info/StakingInfoCountdown';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
class StakingInfoPage extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  render() {
    const { stores, actions } = this.props;
    const { isAlonzoActivated, alonzoActivationTime } = stores.networkStatus;
    const { stakingInfoWasOpen } = stores.staking;
    const { setStakingInfoWasOpen } = actions.staking;
    const { openExternalLink } = stores.app;
    return (
      <StakingInfoCountdown
        startDateTime={alonzoActivationTime}
        isAlonzoActivated={isAlonzoActivated}
        stakingInfoWasOpen={stakingInfoWasOpen}
        onSetStakingInfoWasOpen={setStakingInfoWasOpen.trigger}
        onLearnMoreClick={openExternalLink}
      />
    );
  }
}

export default StakingInfoPage;
