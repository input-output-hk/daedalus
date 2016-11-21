import React, { Component, PropTypes } from 'react';
import { Match, Redirect } from 'react-router';
import { observer } from 'mobx-react';
import Layout from '../Layout';
import ProfileSettingsPage from './ProfileSettingsPage';

@observer(['state', 'controller'])
export default class Settings extends Component {

  static propTypes = {
    state: PropTypes.shape({
      account: PropTypes.shape({
        isLoading: PropTypes.bool.isRequired
      }).isRequired
    }).isRequired
  };

  render() {
    const settingsPath = '/settings';
    const { isLoading } = this.props.state.account;
    if (isLoading) return <div>Loading</div>;
    return (
      <Layout>
        <div style={{ height: '100%' }}>
          <Match pattern={settingsPath} exactly render={() => <Redirect to={`${settingsPath}/profile`} />} />
          <Match pattern={`${settingsPath}/profile`} component={ProfileSettingsPage} />
        </div>
      </Layout>
    );
  }
}
