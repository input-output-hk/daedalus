// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import DelegationProgress from '../../components/staking/DelegationProgress';
import Layout from '../MainLayout';

@observer
export default class DelegationProgressPage extends Component<any> {
  render() {
    const percentage = 10; // TODO: Replace value with the one fetched from API
    return (
      <Layout>
        <div style={{ height: '100%' }}>
          <DelegationProgress percentage={percentage} />
        </div>
      </Layout>
    );
  }
}
