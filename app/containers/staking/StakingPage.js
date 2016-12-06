// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import Staking from '../../components/staking/Staking';
import Layout from '../Layout';

@observer(['state', 'controller'])
export default class ProfileSettingsPage extends Component {

  static propTypes = {
    state: PropTypes.shape({
      settings: PropTypes.shape({
        profile: MobxPropTypes.observableObject.isRequired
      }).isRequired,
      login: PropTypes.shape({
        isLoading: PropTypes.bool.isRequired
      }).isRequired
    }).isRequired,
  };

  render() {
    const { isLoading } = this.props.state.login;
    if (isLoading) return <div>Loading</div>;
    return (
      <Layout>
        <div style={{ height: '100%' }}>
          <Staking />
        </div>
      </Layout>
    );
  }

}
