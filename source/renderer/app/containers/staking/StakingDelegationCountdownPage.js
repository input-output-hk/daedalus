// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import StakingDelegationCountdown from '../../components/staking/delegation-countdown/StakingDelegationCountdown';
import Layout from '../MainLayout';

@inject('stores')
@observer
export default class StakingDelegationCountdownPage extends Component<InjectedProps> {
  render() {
    const { stores } = this.props;
    const { profile, staking } = stores;

    return (
      <Layout>
        <div style={{ height: '100%' }}>
          <StakingDelegationCountdown
            currentLocale={profile.currentLocale}
            startDateTime={staking.startDateTime}
          />
        </div>
      </Layout>
    );
  }
}
