'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = require('react');
const react_router_dom_1 = require('react-router-dom');
const api_1 = require('../../../../common/ipc/api');
const routes_config_1 = require('../../routes-config');
const analytics_1 = require('../../analytics');
const walletRoutes = Object.values(routes_config_1.ROUTES.WALLETS);
const useMenuUpdater = ({
  stores: { app, profile, router, staking, uiDialogs },
  rebuildApplicationMenu,
}) => {
  (0, react_1.useEffect)(() => {
    let walletSettingsState = api_1.WalletSettingsStateEnum.hidden;
    const walletSettingsOptionVisible = walletRoutes.some(
      (path) =>
        (0, react_router_dom_1.matchPath)(router.location?.pathname, {
          path,
        })?.isExact
    );
    if (walletSettingsOptionVisible) {
      const itIsTheWalletSettingsPage = (0, react_router_dom_1.matchPath)(
        router.location?.pathname,
        {
          path: routes_config_1.ROUTES.WALLETS.SETTINGS,
        }
      )?.isExact;
      const anyDialogOpen =
        uiDialogs.activeDialog || app.activeDialog || staking.redeemStep;
      walletSettingsState =
        anyDialogOpen || itIsTheWalletSettingsPage
          ? api_1.WalletSettingsStateEnum.disabled
          : api_1.WalletSettingsStateEnum.enabled;
    }
    rebuildApplicationMenu.send({
      isNavigationEnabled:
        profile.areTermsOfUseAccepted &&
        profile.analyticsAcceptanceStatus !==
          analytics_1.AnalyticsAcceptanceStatus.PENDING,
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
exports.default = useMenuUpdater;
//# sourceMappingURL=useMenuUpdater.js.map
