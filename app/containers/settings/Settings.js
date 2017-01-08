import React, { Component, PropTypes } from 'react';
import { Match, Redirect } from 'react-router';
import { observer, inject } from 'mobx-react';
import Layout from '../MainLayout';
import ProfileSettingsPage from './categories/ProfileSettingsPage';
import TermsOfUseSettingsPage from './categories/TermsOfUseSettingsPage';
import SettingsLayout from '../../components/settings/SettingsLayout';
import SettingsMenu from '../../components/settings/menu/SettingsMenu';

@inject('stores', 'actions') @observer
export default class Settings extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      routing: PropTypes.shape({
        location: PropTypes.shape({
          pathname: PropTypes.string.isRequired
        }).isRequired
      }).isRequired
    }).isRequired,
    actions: PropTypes.shape({
      goToRoute: PropTypes.func.isRequired
    }).isRequired
  };

  isActivePage = (page: string) => {
    const { location } = this.props.stores.routing;
    if (location) {
      return location.pathname === `/settings/${page}`;
    }
    return false;
  };

  render() {
    const settingsPath = '/settings';
    const { actions } = this.props;
    const menu = (
      <SettingsMenu
        onItemClick={(page) => actions.goToRoute({ route: `${settingsPath}/${page}` })}
        isActiveItem={this.isActivePage}
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
