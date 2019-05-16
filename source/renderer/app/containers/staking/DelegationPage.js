// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../types/injectedPropsType';
import Delegation from '../../components/staking/Delegation';
import Layout from '../MainLayout';

@inject('stores')
@observer
export default class DelegationPage extends Component<InjectedProps> {
  render() {
    const { stores } = this.props;
    const { currentLocale } = stores.profile;

    return (
      <Layout>
        <div style={{ height: '100%' }}>
          <Delegation currentLocale={currentLocale} />
        </div>
      </Layout>
    );
  }
}
