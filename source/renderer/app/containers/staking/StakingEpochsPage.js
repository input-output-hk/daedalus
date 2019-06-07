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
        previousEpochName=""
        previousEpochData={[]}
        isLoading={false}
      />
    );
  }
}
