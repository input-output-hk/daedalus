// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import StakingEpochs from '../../components/staking/epochs/StakingEpochs';
import type { InjectedProps } from '../../types/injectedPropsType';

import PREVIOUS_EPOCHS from '../../config/stakingPreviousEpoch.dummy.json';
import CURRENT_EPOCHS from '../../config/stakingCurrentEpoch.dummy.json';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakingEpochsPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  componentDidMount() {
    const {
      stores: { networkStatus },
      actions: {
        staking: { goToStakingDelegationCenterPage },
      },
    } = this.props;

    if (networkStatus.isIncentivizedTestnet) {
      goToStakingDelegationCenterPage.trigger();
    }
  }

  render() {
    return (
      <StakingEpochs
        currentEpochName={CURRENT_EPOCHS.name}
        currentEpochData={CURRENT_EPOCHS.data}
        currentEpochEndDateTime={CURRENT_EPOCHS.endsAt}
        currentEpochProgress={CURRENT_EPOCHS.progress}
        previousEpochName={PREVIOUS_EPOCHS.name}
        previousEpochData={PREVIOUS_EPOCHS.data}
        isLoading={false}
      />
    );
  }
}
