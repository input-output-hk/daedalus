import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import { withRouter, type RouteComponentProps } from 'react-router-dom';
import Navigation from '../../components/navigation/Navigation';
import type { NavButtonProps } from '../../components/navigation/Navigation';
import type { InjectedContainerProps } from '../../types/injectedPropsType';
import MainLayout from '../MainLayout';
import { ROUTES } from '../../routes-config';

const messages = defineMessages({
  tabDashboard: {
    id: 'governance.tabs.dashboard',
    defaultMessage: '!!!Governance Dashboard',
    description: 'Label for the governance dashboard tab.',
  },
  tabDirectory: {
    id: 'governance.tabs.directory',
    defaultMessage: '!!!DRep Directory',
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
    const { app, wallets } = this.props.stores;
    const { intl } = this.props;

    const anyWalletDelegating = (wallets?.all ?? []).some(
      (w) => w.currentVote != null
    );

    const navItems: Array<NavButtonProps> = [
      ...(anyWalletDelegating
        ? [
            {
              id: ROUTES.GOVERNANCE.DASHBOARD,
              label: intl.formatMessage(messages.tabDashboard),
            },
          ]
        : []),
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
        <Navigation
          items={navItems}
          activeItem={activeItem?.id}
          onNavItemClick={this.handleNavItemClick}
        />
        {this.props.children}
      </MainLayout>
    );
  }
}

export default withRouter(injectIntl(Governance));
