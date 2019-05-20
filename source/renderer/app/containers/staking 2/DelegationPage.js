// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import Delegation from '../../components/staking/Delegation';
import Layout from '../MainLayout';

@inject('stores')
@observer
export default class DelegationPage extends Component<InjectedProps> {
  render() {
    const { stores } = this.props;
    const { currentLocale } = stores.profile;
    const startDateTime = new Date().toISOString(); // TODO: Replace the value from API response

    return (
      <Layout>
        <div style={{ height: '100%' }}>
          <Delegation
            currentLocale={currentLocale}
            startDateTime={startDateTime}
          />
        </div>
      </Layout>
    );
  }
}
