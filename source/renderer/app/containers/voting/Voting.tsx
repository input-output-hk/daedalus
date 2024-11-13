import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import Navigation from '../../components/navigation/Navigation';
import type { NavButtonProps } from '../../components/navigation/Navigation';
import type { InjectedContainerProps } from '../../types/injectedPropsType';
import MainLayout from '../MainLayout';
import { ROUTES } from '../../routes-config';

type Props = InjectedContainerProps;

@inject('stores', 'actions')
@observer
export class Voting extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  render() {
    const { app } = this.props.stores;
    const navItems: Array<NavButtonProps> = [
      {
        id: ROUTES.VOTING.REGISTRATION,
        label: 'Registration',
      },
      {
        id: ROUTES.VOTING.GOVERNANCE,
        label: 'Governance',
      },
    ];
    const activeItem = navItems.find((item) => app.currentRoute === item.id);
    return (
      <MainLayout>
        <div style={{ height: '50px' }}>
          <Navigation
            items={navItems}
            activeItem={activeItem.label}
            isActiveNavItem={(navItemId: string) => navItemId === activeItem.id}
            onNavItemClick={(navItemId: string) => {
              this.props.actions.router.goToRoute.trigger({
                route: navItemId,
              });
            }}
          />
        </div>
        {this.props.children}
      </MainLayout>
    );
  }
}
