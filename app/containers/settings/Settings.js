import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import Layout from '../MainLayout';
import SettingsLayout from '../../components/settings/SettingsLayout';
import SettingsMenu from '../../components/settings/menu/SettingsMenu';

@inject('stores', 'actions') @observer
export default class Settings extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      router: PropTypes.shape({
        location: PropTypes.shape({
          pathname: PropTypes.string.isRequired,
        }).isRequired,
      }).isRequired,
    }).isRequired,
    actions: PropTypes.shape({
      goToRoute: PropTypes.func.isRequired,
    }).isRequired,
  };

  isActivePage = (page: string) => {
    const { location } = this.props.stores.router;
    if (location) {
      return location.pathname === `/settings/${page}`;
    }
    return false;
  };

  render() {
    const settingsPath = '/settings';
    const { actions, children } = this.props;
    const menu = (
      <SettingsMenu
        onItemClick={(page) => actions.goToRoute({ route: `${settingsPath}/${page}` })}
        isActiveItem={this.isActivePage}
      />
    );
    return (
      <Layout>
        <SettingsLayout menu={menu}>
          {children}
        </SettingsLayout>
      </Layout>
    );
  }
}
