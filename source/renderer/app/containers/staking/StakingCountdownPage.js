// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import StakingCountdown from '../../components/staking/countdown/StakingCountdown';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakingCountdownPage extends Component<Props> {
  static defaultProps = { actions: null, stores: {} };

  render() {
    const { stores, actions } = this.props;
    const { profile, staking } = stores;
    const {
      staking: { goToStakingPage },
    } = actions;
    const redirectToStakingPage = goToStakingPage.trigger;

    return (
      <StakingCountdown
        redirectToStakingPage={redirectToStakingPage}
        currentLocale={profile.currentLocale}
        startDateTime={staking.startDateTime}
      />
    );
  }
}
