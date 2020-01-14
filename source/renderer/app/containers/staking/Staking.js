// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import MainLayout from '../MainLayout';
import StakingWithNavigation from '../../components/staking/layouts/StakingWithNavigation';
import { ROUTES } from '../../routes-config';
import { buildRoute } from '../../utils/routing';
import type { InjectedContainerProps } from '../../types/injectedPropsType';
import type { NavDropdownProps } from '../../components/navigation/Navigation';

type Props = InjectedContainerProps;

@inject('stores', 'actions')
@observer
export default class Staking extends Component<Props> {
  // TODO: Uncomment when the we need the countdown logic
  // componentDidMount() {
  //   this.handleDelegationRoute();
  // }

  // handleDelegationRoute = () => {
  //   const {
  //     actions,
  //     stores: { staking },
  //   } = this.props;

  //   if (staking.showCountdown() && !staking.isStakingDelegationCountdown) {
  //     return actions.router.goToRoute.trigger({
  //       route: ROUTES.STAKING.DELEGATION_COUNTDOWN,
  //     });
  //   }

  //   if (!staking.showCountdown() && staking.isStakingDelegationCountdown) {
  //     return actions.router.goToRoute.trigger({
  //       route: ROUTES.STAKING.INFO,
  //     });
  //   }

  //   return true;
  // };

  isActiveNavItem = (page: string, item: NavDropdownProps) => {
    const { app } = this.props.stores;
    const { options } = item;
    if (options && options.length) {
      options.forEach(option => {
        if (
          app.currentRoute &&
          app.currentRoute.includes(option.value.toString())
        ) {
          page = option.value.toString();
        }
      });
    }
    const screenRoute = buildRoute(ROUTES.STAKING.PAGE, {
      page,
    });
    return app.currentRoute === screenRoute;
  };

  handleNavItemClick = (page: string) => {
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.STAKING.PAGE,
      params: { page },
    });
  };

  render() {
    const {
      stores: { app, staking, networkStatus },
      children,
    } = this.props;

    return (
      <MainLayout>
        {staking.isStakingDelegationCountdown ? (
          children
        ) : (
          <StakingWithNavigation
            isActiveNavItem={this.isActiveNavItem}
            onNavItemClick={this.handleNavItemClick}
            activeItem={app.currentPage}
            isIncentivizedTestnet={networkStatus.isIncentivizedTestnet}
          >
            {children}
          </StakingWithNavigation>
        )}
      </MainLayout>
    );
  }
}
