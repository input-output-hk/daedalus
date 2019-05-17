// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import MainLayout from '../MainLayout';
import StakingWithNavigation from '../../components/staking/layouts/StakingWithNavigation';
import StakingDelegationCountdownPage from './StakingDelegationCountdownPage';

// import { buildRoute } from '../../utils/routing';
import { ROUTES } from '../../routes-config';
import type { InjectedContainerProps } from '../../types/injectedPropsType';

type Props = InjectedContainerProps;

@inject('stores', 'actions')
@observer
export default class Wallet extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  handleNavItemClick = (page: string) => {
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.STAKING.PAGE,
      params: { page },
    });
  };

  render() {
    const { stores } = this.props;
    const { app, staking } = stores;

    if (staking.showCountdown === true) {
      return <StakingDelegationCountdownPage />;
    }

    return (
      <MainLayout>
        <StakingWithNavigation
          isActiveScreen={this.isActiveScreen}
          onNavItemClick={this.handleNavItemClick}
          activeItem={app.currentPage}
        >
          {this.props.children}
        </StakingWithNavigation>
      </MainLayout>
    );
  }
}
