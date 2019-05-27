// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import MainLayout from '../MainLayout';
import StakingWithNavigation from '../../components/staking/layouts/StakingWithNavigation';

import { ROUTES } from '../../routes-config';
import type { InjectedContainerProps } from '../../types/injectedPropsType';

type Props = InjectedContainerProps;

@inject('stores', 'actions')
@observer
export default class Staking extends Component<Props> {
  componentDidMount() {
    this.handleDelegationRoute();
  }

  handleDelegationRoute = () => {
    const {
      actions,
      stores: { staking },
    } = this.props;

    if (staking.showCountdown() && !staking.isStakingDelegationCountdown) {
      return actions.router.goToRoute.trigger({
        route: ROUTES.STAKING.DELEGATION_COUNTDOWN,
      });
    }

    if (!staking.showCountdown() && staking.isStakingDelegationCountdown) {
      return actions.router.goToRoute.trigger({
        route: ROUTES.STAKING.INFO,
      });
    }

    return true;
  };

  handleNavItemClick = (page: string) => {
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.STAKING.PAGE,
      params: { page },
    });
  };

  render() {
    const {
      stores: { app, staking },
      children,
    } = this.props;

    return (
      <MainLayout>
        {staking.showCountdown() ? (
          children
        ) : (
          <StakingWithNavigation
            onNavItemClick={this.handleNavItemClick}
            activeItem={app.currentPage}
          >
            {children}
          </StakingWithNavigation>
        )}
      </MainLayout>
    );
  }
}
