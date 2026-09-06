import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import MainLayout from '../MainLayout';
import WalletWithNavigation from '../../components/wallet/layouts/WalletWithNavigation';
import LoadingSpinner from '../../components/widgets/LoadingSpinner';
import RestoreNotification from '../../components/notifications/RestoreNotification';
import ChangeSpendingPasswordDialog from '../../components/wallet/settings/ChangeSpendingPasswordDialog';
import { buildRoute } from '../../utils/routing';
import { ROUTES } from '../../routes-config';
import type { InjectedContainerProps } from '../../types/injectedPropsType';
import type { NavDropdownProps } from '../../components/navigation/Navigation';

type Props = InjectedContainerProps;

@inject('stores', 'actions')
@observer
class Wallet extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  isActiveScreen = (page: string, item: NavDropdownProps) => {
    const { app, wallets } = this.props.stores;
    if (!wallets.active) return false;
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

    const screenRoute = buildRoute(ROUTES.WALLETS.PAGE, {
      id: wallets.active.id,
      page,
    });
    return app.currentRoute === screenRoute;
  };
  handleWalletNavItemClick = (page: string) => {
    const { wallets } = this.props.stores;
    if (!wallets.active) return;
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.WALLETS.PAGE,
      params: {
        id: wallets.active.id,
        page,
      },
    });
  };

  render() {
    const { actions, stores } = this.props;
    const { app, wallets, walletSettings, uiDialogs } = stores;
    const { isOpen: isDialogOpen } = uiDialogs;
    const { restartNode } = actions.networkStatus;
    const { active: activeWallet } = wallets;

    if (!activeWallet) {
      return (
        <MainLayout>
          <LoadingSpinner />
        </MainLayout>
      );
    }

    const {
      hasNotification,
    } = walletSettings.getWalletsRecoveryPhraseVerificationData(
      activeWallet.id
    );
    const {
      isRestoring,
      isLegacy,
      isNotResponding,
      isHardwareWallet,
      hasPassword,
    } = activeWallet;
    return (
      <MainLayout>
        {isRestoring ? (
          <RestoreNotification
            restoreProgress={activeWallet.restorationProgress}
          />
        ) : null}

        <WalletWithNavigation
          activeItem={app.currentPage}
          hasNotification={hasNotification}
          hasPassword={hasPassword}
          isActiveScreen={this.isActiveScreen}
          isLegacy={isLegacy}
          isNotResponding={isNotResponding}
          isHardwareWallet={isHardwareWallet}
          isSetWalletPasswordDialogOpen={isDialogOpen(
            ChangeSpendingPasswordDialog
          )}
          onOpenExternalLink={(url: string) => stores.app.openExternalLink(url)}
          onRestartNode={() => restartNode.trigger()}
          onSetWalletPassword={() => {
            actions.dialogs.open.trigger({
              dialog: ChangeSpendingPasswordDialog,
            });
          }}
          onWalletNavItemClick={this.handleWalletNavItemClick}
        >
          {this.props.children}
        </WalletWithNavigation>
      </MainLayout>
    );
  }
}

export default Wallet;
