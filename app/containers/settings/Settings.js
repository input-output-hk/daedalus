import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import SettingsLayout from '../../components/settings/SettingsLayout';
import SettingsMenu from '../../components/settings/menu/SettingsMenu';
import resolver from '../../utils/imports';
import { buildRoute } from '../../utils/routing';
import type { InjectedContainerProps } from '../../types/injectedPropsType';

const Layout = resolver('containers/MainLayout');

@inject('stores', 'actions') @observer
export default class Settings extends Component<InjectedContainerProps> {

  static defaultProps = { actions: null, stores: null };

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
        onItemClick={(route) => actions.router.goToRoute.trigger({ route })}
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
