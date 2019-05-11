// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Staking from '../../components/staking/Staking';
import Layout from '../MainLayout';

@observer
export default class CardanoDecentralisationNotificationPage extends Component<any> {
  render() {
    return (
      <Layout>
        <div style={{ height: '100%' }}>
          <Staking />
        </div>
      </Layout>
    );
  }
}
