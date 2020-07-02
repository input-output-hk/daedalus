// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import MainLayout from '../MainLayout';
import StakingWithNavigation from '../../components/staking/layouts/StakingWithNavigation';
import ExperimentalDataOverlay from '../../components/notifications/ExperimentalDataOverlay';
import { ROUTES } from '../../routes-config';
import { buildRoute } from '../../utils/routing';
import type { InjectedContainerProps } from '../../types/injectedPropsType';
import type { NavDropdownProps } from '../../components/navigation/Navigation';

type Props = InjectedContainerProps;

@inject('stores', 'actions')
@observer
export default class Staking extends Component<Props> {
  static defaultProps = { actions: null, stores: null };
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
  //       route: ROUTES.STAKING.COUNTDOWN,
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

  handleCloseExperimentalDataOverlay = () => {
    const { stores } = this.props;
    const { markStakingExperimentAsRead } = stores.staking;
    markStakingExperimentAsRead();
  };

  render() {
    const {
      stores: { app, staking },
      children,
    } = this.props;
    const { isShelleyTestnet } = global;
    const { isStakingExperimentRead, isStakingDelegationCountdown } = staking;

    return (
      <MainLayout>
        {!isShelleyTestnet && !isStakingExperimentRead && (
          <ExperimentalDataOverlay
            onClose={this.handleCloseExperimentalDataOverlay}
          />
        )}
        {isStakingDelegationCountdown ? (
          children
        ) : (
          <StakingWithNavigation
            isActiveNavItem={this.isActiveNavItem}
            onNavItemClick={this.handleNavItemClick}
            activeItem={app.currentPage}
            isIncentivizedTestnet={global.isIncentivizedTestnet}
            isShelleyTestnet={global.isShelleyTestnet}
          >
            {children}
          </StakingWithNavigation>
        )}
      </MainLayout>
    );
  }
}
