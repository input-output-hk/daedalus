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
class Settings extends Component<InjectedContainerProps> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  static defaultProps = {
    actions: null,
    stores: null,
  };
  isActivePage = (route: string) => {
    const { location } = this.props.stores.router;

    if (location) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      return location.pathname === buildRoute(route);
    }

    return false;
  };

  render() {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'isFlight' does not exist on type 'typeof... Remove this comment to see the full error message
    const { isFlight } = global;
    const { actions, stores, children } = this.props;
    const { networkStatus, app, router } = stores;
    const { isSynced } = networkStatus;
    const { currentRoute } = app;
    const { location } = router;
    const menu = (
      <SettingsMenu
        isSyncing={!isSynced}
        isFlight={isFlight}
        currentRoute={currentRoute}
        onItemClick={(route) =>
          actions.router.goToRoute.trigger({
            route,
          })
        }
        isActiveItem={this.isActivePage}
      />
    );
    return (
      <Layout>
        <SettingsLayout menu={menu} activePage={location.pathname}>
          {children}
        </SettingsLayout>
      </Layout>
    );
  }
}

export default Settings;
