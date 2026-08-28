import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import { withRouter } from 'react-router-dom';
import type { RouteComponentProps } from 'react-router-dom';
import GovernanceWithNavigation from '../../components/governance/layouts/GovernanceWithNavigation';
import type { NavButtonProps } from '../../components/navigation/Navigation';
import type { InjectedContainerProps } from '../../types/injectedPropsType';
import MainLayout from '../MainLayout';
import { ROUTES } from '../../routes-config';

const messages = defineMessages({
  tabDashboard: {
    id: 'governance.tabs.dashboard',
    defaultMessage: '!!!Governance Center',
    description: 'Label for the governance wallets tab.',
  },
  tabDirectory: {
    id: 'governance.tabs.directory',
    defaultMessage: '!!!Directory',
    description: 'Label for the DRep directory tab.',
  },
  tabFavorites: {
    id: 'governance.drepDirectory.tabs.favorites',
    defaultMessage: '!!!Favorites',
    description: 'Label for the DRep favorites tab.',
  },
});

type Props = InjectedContainerProps & {
  intl: intlShape.isRequired;
} & RouteComponentProps;

@inject('stores', 'actions')
@observer
class Governance extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  handleNavItemClick = (itemId: string) => {
    if (this.props.history.location.pathname !== itemId) {
      this.props.history.push(itemId);
    }
  };

  render() {
    const { app } = this.props.stores;
    const { intl } = this.props;

    const navItems: Array<NavButtonProps> = [
      {
        id: ROUTES.GOVERNANCE.DASHBOARD,
        label: intl.formatMessage(messages.tabDashboard),
      },
      {
        id: ROUTES.GOVERNANCE.DREPS,
        label: intl.formatMessage(messages.tabDirectory),
      },
      {
        id: ROUTES.GOVERNANCE.FAVORITES,
        label: intl.formatMessage(messages.tabFavorites),
      },
    ];

    const activeItem = navItems.find(
      (item) =>
        app.currentRoute === item.id || app.currentRoute.startsWith(item.id)
    );

    return (
      <MainLayout>
        <GovernanceWithNavigation
          items={navItems}
          activeItem={activeItem?.id}
          isActiveNavItem={(id: string) => id === activeItem?.id}
          onNavItemClick={this.handleNavItemClick}
        >
          {this.props.children}
        </GovernanceWithNavigation>
      </MainLayout>
    );
  }
}

export default withRouter(injectIntl(Governance));
