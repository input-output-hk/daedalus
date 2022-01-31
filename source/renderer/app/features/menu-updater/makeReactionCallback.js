// @flow
import { matchPath } from 'react-router-dom';
import { WalletSettingsStateEnum } from '../../../../common/ipc/api';
import { ROUTES } from '../../routes-config';
import type { MakeReactionCallbackArgs } from './types';

const walletRoutes = Object.values(ROUTES.WALLETS);

const makeReactionCallback = ({
  stores: { app, profile, router, staking, uiDialogs },
  rebuildApplicationMenu,
}: MakeReactionCallbackArgs) => () => {
  let walletSettingsState = WalletSettingsStateEnum.hidden;

  const walletSettingsOptionVisible = walletRoutes.some(
    (path) => matchPath(router.location?.pathname, { path })?.isExact
  );

  if (walletSettingsOptionVisible) {
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

  if (profile.currentLocale) {
    // Just mentioning profile.currentLocale to make the Reaction watch its changes
  }

  rebuildApplicationMenu.send({
    isNavigationEnabled: profile.areTermsOfUseAccepted,
    walletSettingsState,
  });
};

export default makeReactionCallback;
