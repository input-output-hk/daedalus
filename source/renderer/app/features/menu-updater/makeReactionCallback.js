// @flow
import { matchPath } from 'react-router-dom';
import { WalletSettingsStateEnum } from '../../../../common/ipc/api';
import { ROUTES } from '../../routes-config';
import type { MakeReactionCallbackArgs } from './types';

const walletRoutesExceptSettings = Object.values(ROUTES.WALLETS).filter(
  (route) => route !== ROUTES.WALLETS.SETTINGS
);

const makeReactionCallback = ({
  stores: { profile, router, uiDialogs },
  rebuildApplicationMenu,
}: MakeReactionCallbackArgs) => () => {
  let walletSettingsState = WalletSettingsStateEnum.hidden;

  const itIsTheWalletSettingsPage = matchPath(router.location?.pathname, {
    path: ROUTES.WALLETS.SETTINGS,
  })?.isExact;
  const walletSettingsOptionVisible = itIsTheWalletSettingsPage
    ? false
    : walletRoutesExceptSettings.some(
        (path) => matchPath(router.location?.pathname, { path })?.isExact
      );

  if (walletSettingsOptionVisible) {
    walletSettingsState = uiDialogs.activeDialog
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
