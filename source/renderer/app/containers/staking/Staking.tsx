import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import MainLayout from '../MainLayout';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import StakingUnavailable from '../../components/staking/StakingUnavailable';
import StakingWithNavigation from '../../components/staking/layouts/StakingWithNavigation';
import DelegationSetupWizardDialog from '../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import UndelegateWalletConfirmationDialog from '../../components/wallet/settings/UndelegateWalletConfirmationDialog';
import { ROUTES } from '../../routes-config';
import { buildRoute } from '../../utils/routing';
import type { InjectedContainerProps } from '../../types/injectedPropsType';
import type { NavDropdownProps } from '../../components/navigation/Navigation';
import { IS_STAKING_INFO_PAGE_AVAILABLE } from '../../config/stakingConfig';

type Props = InjectedContainerProps;

@inject('stores', 'actions')
@observer
class Staking extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

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
        route: ROUTES.STAKING.COUNTDOWN,
      });
    }

    if (!staking.showCountdown() && staking.isStakingDelegationCountdown) {
      return actions.router.goToRoute.trigger({
        route: ROUTES.STAKING.INFO,
      });
    }

    return true;
  };
  isActiveNavItem = (page: string, item: NavDropdownProps) => {
    const { app } = this.props.stores;
    const { options } = item;

    // @ts-ignore ts-migrate(2339) FIXME: Property 'length' does not exist on type 'never'.
    if (options && options.length) {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'forEach' does not exist on type 'never'.
      options.forEach((option) => {
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
      params: {
        page,
      },
    });
  };

  render() {
    const {
      stores: { app, staking, networkStatus, uiDialogs },
      children,
    } = this.props;
    const {
      isSynced,
      syncPercentage,
      isAlonzoPending,
      isAlonzoActivated,
    } = networkStatus;
    const { isStakingDelegationCountdown } = staking;
    const shouldShowInfoTab = isAlonzoPending || isAlonzoActivated;
    const isDelegationWizardOpen = uiDialogs.isOpen(
      DelegationSetupWizardDialog
    );
    const isUndelegationWizardOpen = uiDialogs.isOpen(
      UndelegateWalletConfirmationDialog
    );

    if (!isSynced && !(isDelegationWizardOpen || isUndelegationWizardOpen)) {
      return (
        <MainLayout>
          <VerticalFlexContainer>
            <StakingUnavailable syncPercentage={syncPercentage} />
          </VerticalFlexContainer>
        </MainLayout>
      );
    }

    return (
      <MainLayout>
        {isStakingDelegationCountdown ? (
          children
        ) : (
          <StakingWithNavigation
            isActiveNavItem={this.isActiveNavItem}
            onNavItemClick={this.handleNavItemClick}
            activeItem={app.currentPage}
            showInfoTab={IS_STAKING_INFO_PAGE_AVAILABLE && shouldShowInfoTab}
          >
            {children}
          </StakingWithNavigation>
        )}
      </MainLayout>
    );
  }
}

export default Staking;
