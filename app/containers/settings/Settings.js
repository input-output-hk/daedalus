import React, { Component } from 'react';
import { Match, Redirect } from 'react-router';
import { observer, inject } from 'mobx-react';
import Layout from '../MainLayout';
import ProfileSettingsPage from './ProfileSettingsPage';

@inject('state', 'controller') @observer
export default class Settings extends Component {
  render() {
    const settingsPath = '/settings';
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
