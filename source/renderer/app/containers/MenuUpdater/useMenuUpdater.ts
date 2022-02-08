import { memoize } from 'lodash/fp';
import { useEffect, useMemo } from 'react';
import { matchPath } from 'react-router-dom';
import { WalletSettingsStateEnum } from '../../../../common/ipc/api';
import { ROUTES } from '../../routes-config';
import type { UseMenuUpdaterArgs } from './types';

const walletRoutes = Object.values(ROUTES.WALLETS);

const shouldWalletSettingsOptionBeVisible = memoize((pathname: string) =>
  walletRoutes.some(
    (path) =>
      matchPath(pathname, {
        path,
      })?.isExact
  )
);

const useMenuUpdater = ({
  stores: { app, profile, router, staking, uiDialogs },
  rebuildApplicationMenu,
}: UseMenuUpdaterArgs) => {
  useEffect(() => {
    let walletSettingsState = WalletSettingsStateEnum.hidden;

    if (shouldWalletSettingsOptionBeVisible(router.location?.pathname)) {
      const itIsTheWalletSettingsPage = matchPath(router.location?.pathname, {
        path: ROUTES.WALLETS.SETTINGS,
      })?.isExact;
      const anyDialogOpen =
        uiDialogs.activeDialog || app.activeDialog || staking.redeemStep;
      walletSettingsState =
        anyDialogOpen || itIsTheWalletSettingsPage
          ? WalletSettingsStateEnum.disabled
          : WalletSettingsStateEnum.enabled;
    }

    rebuildApplicationMenu.send({
      isNavigationEnabled: profile.areTermsOfUseAccepted,
      walletSettingsState,
    });
  }, [
    app.activeDialog,
    profile.areTermsOfUseAccepted,
    profile.currentLocale,
    router.location?.pathname,
    staking.redeemStep,
    uiDialogs.activeDialog,
  ]);
};

export default useMenuUpdater;
