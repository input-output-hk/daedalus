import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import StakingEpochs from '../../components/staking/epochs/StakingEpochs';
import type { InjectedProps } from '../../types/injectedPropsType';
import PREVIOUS_EPOCHS from '../../config/stakingPreviousEpoch.dummy.json';
import CURRENT_EPOCHS from '../../config/stakingCurrentEpoch.dummy.json';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
class StakingEpochsPage extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  render() {
    return (
      <StakingEpochs
        currentEpochName={CURRENT_EPOCHS.name}
        // @ts-ignore ts-migrate(2739) FIXME: Type '{ pool: { id: string; ranking: number; ticke... Remove this comment to see the full error message
        currentEpochData={CURRENT_EPOCHS.data}
        currentEpochEndDateTime={CURRENT_EPOCHS.endsAt}
        currentEpochProgress={CURRENT_EPOCHS.progress}
        previousEpochName={PREVIOUS_EPOCHS.name}
        // @ts-ignore ts-migrate(2739) FIXME: Type '{ pool: { id: string; ranking: number; ticke... Remove this comment to see the full error message
        previousEpochData={PREVIOUS_EPOCHS.data}
        isLoading={false}
      />
    );
  }
}

export default StakingEpochsPage;
