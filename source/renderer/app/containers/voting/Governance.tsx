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
    // Guard against pushing the path we are already on — react-router's hash
    // history emits "Hash history cannot PUSH the same path" otherwise.
    if (this.props.history.location.pathname !== itemId) {
      this.props.history.push(itemId);
    }
  };

  render() {
    const { app } = this.props.stores;
    const { intl } = this.props;
    const navItems: Array<NavButtonProps> = [
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
