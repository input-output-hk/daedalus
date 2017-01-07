import React, { Component, PropTypes } from 'react';
import { Match, Redirect } from 'react-router';
import { observer, inject } from 'mobx-react';
import Layout from '../MainLayout';
import ProfileSettingsPage from './categories/ProfileSettingsPage';
import TermsOfUseSettingsPage from './categories/TermsOfUseSettingsPage';
import SettingsLayout from '../../components/settings/SettingsLayout';
import SettingsMenu from '../../components/settings/menu/SettingsMenu';

@inject('state', 'controller') @observer
export default class Settings extends Component {

  static propTypes = {
    state: PropTypes.shape({
      router: PropTypes.shape({
        location: PropTypes.shape({
          pathname: PropTypes.string.isRequired
        }).isRequired
      }).isRequired
    }).isRequired,
    controller: PropTypes.shape({
      navigateTo: PropTypes.func.isRequired
    }).isRequired
  };

  isActivePage(page: string) {
    const { router } = this.props.state;
    if (router.location) {
      return router.location.pathname === `/settings/${page}`;
    }
    return false;
  }

  render() {
    const settingsPath = '/settings';
    const { controller } = this.props;
    const menu = (
      <SettingsMenu
        onItemClick={(page) => controller.navigateTo(`${settingsPath}/${page}`)}
        isActiveItem={this.isActivePage.bind(this)}
      />
    );
    return (
      <Layout>
        <SettingsLayout menu={menu}>
          <Match pattern={settingsPath} exactly render={() => <Redirect to={`${settingsPath}/profile`} />} />
          <Match pattern={`${settingsPath}/profile`} component={ProfileSettingsPage} />
          <Match pattern={`${settingsPath}/termsOfUse`} component={TermsOfUseSettingsPage} />
        </SettingsLayout>
      </Layout>
    );
  }
}
