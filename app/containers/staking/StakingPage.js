// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject, PropTypes as MobxPropTypes } from 'mobx-react';
import Staking from '../../components/staking/Staking';
import Layout from '../MainLayout';

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
