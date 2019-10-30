// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { intlShape } from 'react-intl';
import SettingsLayout from '../../components/settings/SettingsLayout';
import SettingsMenu from '../../components/settings/menu/SettingsMenu';
import Layout from '../MainLayout';
import { buildRoute } from '../../utils/routing';
import type { InjectedContainerProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class Settings extends Component<InjectedContainerProps> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = { actions: null, stores: null };

  isActivePage = (route: string) => {
    const { location } = this.props.stores.router;
    if (location) {
      return location.pathname === buildRoute(route);
    }
    return false;
  };

  render() {
    const { actions, children, stores } = this.props;
    const {
      isIncentivizedTestnet,
      environment: { isTest },
    } = stores.networkStatus;
    const menu = (
      <SettingsMenu
        onItemClick={route => actions.router.goToRoute.trigger({ route })}
        isActiveItem={this.isActivePage}
        showDisplaySettings={!isIncentivizedTestnet || isTest}
      />
    );
    return (
      <Layout>
        <SettingsLayout menu={menu}>{children}</SettingsLayout>
      </Layout>
    );
  }
}
