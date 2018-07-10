// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Staking from '../../components/staking/Staking';
import { resolve } from '../../utils/imports';

const Layout = resolve(require.context('../', true, /MainLayout.js/));

@observer
export default class StakingPage extends Component<any> {

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
