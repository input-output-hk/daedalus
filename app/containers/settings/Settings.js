import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import Layout from '../MainLayout';
import { oneOrManyChildElements } from '../../propTypes';
import SettingsLayout from '../../components/settings/SettingsLayout';
import SettingsMenu from '../../components/settings/menu/SettingsMenu';
import { buildRoute } from '../../lib/routing-helpers';

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
      router: PropTypes.shape({
        goToRoute: PropTypes.func.isRequired,
      }).isRequired,
    }).isRequired,
    children: oneOrManyChildElements.isRequired,
  };

  isActivePage = (route: string) => {
    const { location } = this.props.stores.router;
    if (location) {
      return location.pathname === buildRoute(route);
    }
    return false;
  };

  render() {
    const { actions, children } = this.props;
    const menu = (
      <SettingsMenu
        onItemClick={(route) => actions.router.goToRoute({ route })}
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
