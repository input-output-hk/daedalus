import { useEffect } from 'react';
import { matchPath } from 'react-router-dom';
import { WalletSettingsStateEnum } from '../../../../common/ipc/api';
import { ROUTES } from '../../routes-config';
import type { UseMenuUpdaterArgs } from './types';
import { AnalyticsAcceptanceStatus } from '../../analytics';

const walletRoutes = Object.values(ROUTES.WALLETS);

const useMenuUpdater = ({
  stores: { app, profile, router, uiDialogs },
  rebuildApplicationMenu,
}: UseMenuUpdaterArgs) => {
  useEffect(() => {
    let walletSettingsState = WalletSettingsStateEnum.hidden;
    const walletSettingsOptionVisible = walletRoutes.some(
      (path) =>
        matchPath(router.location?.pathname, {
          path,
        })?.isExact
    );

    if (walletSettingsOptionVisible) {
      const itIsTheWalletSettingsPage = matchPath(router.location?.pathname, {
        path: ROUTES.WALLETS.SETTINGS,
      })?.isExact;
      const anyDialogOpen = uiDialogs.activeDialog || app.activeDialog;
      walletSettingsState =
        anyDialogOpen || itIsTheWalletSettingsPage
          ? WalletSettingsStateEnum.disabled
          : WalletSettingsStateEnum.enabled;
    }

    rebuildApplicationMenu.send({
      isNavigationEnabled:
        profile.areTermsOfUseAccepted &&
        profile.analyticsAcceptanceStatus !== AnalyticsAcceptanceStatus.PENDING,
      walletSettingsState,
    });
  }, [
    app.activeDialog,
    profile.analyticsAcceptanceStatus,
    profile.areTermsOfUseAccepted,
    profile.currentLocale,
    router.location?.pathname,
    uiDialogs.activeDialog,
  ]);
};

export default useMenuUpdater;
