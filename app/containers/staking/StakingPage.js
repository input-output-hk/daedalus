// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Staking from '../../components/staking/Staking';
import resolver from '../../lib/resolver';

const Layout = resolver('containers/MainLayout');

@observer
export default class StakingPage extends Component {

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
