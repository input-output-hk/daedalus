// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import StakingEpochs from '../../components/staking/epochs/StakingEpochs';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakingEpochsPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    return (
      <StakingEpochs
        currentEpochName=""
        currentEpochData={[]}
        currentEpochEndDateTime="2019-12-31T00:00:00.161Z"
        currentEpochProgress={0}
        previousEpochName=""
        previousEpochData={[]}
        isLoading={false}
      />
    );
  }
}
