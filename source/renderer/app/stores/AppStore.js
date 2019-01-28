// @flow
import { observable, computed, action } from 'mobx';
import Store from './lib/Store';
import LocalizableError from '../i18n/LocalizableError';
import { buildRoute } from '../utils/routing';
import {
  OPEN_ABOUT_DIALOG_CHANNEL,
  GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL,
  GO_TO_NETWORK_STATUS_SCREEN_CHANNEL,
  GO_TO_BLOCK_CONSOLIDATION_STATUS_CHANNEL
} from '../../../common/ipc/api';
import { GET_GPU_STATUS } from '../../../common/ipc-api';
import { ROUTES } from '../routes-config';
import type { GpuStatus } from '../types/gpuStatus';
import { openExternalUrlChannel } from '../ipc/open-external-url';

// TODO: refactor all parts that rely on this to ipc channels!
const { ipcRenderer } = global;

export default class AppStore extends Store {

  @observable error: ?LocalizableError = null;
  @observable isAboutDialogOpen = false;
  @observable gpuStatus: ?GpuStatus = null;
  @observable numberOfEpochsConsolidated: number = 0;
  @observable previousRoute: string = ROUTES.ROOT;

  setup() {
    this.actions.router.goToRoute.listen(this._updateRouteLocation);
    this.actions.app.openAboutDialog.listen(this._openAboutDialog);
    this.actions.app.closeAboutDialog.listen(this._closeAboutDialog);
    this.actions.app.getGpuStatus.listen(this._getGpuStatus);
    this.actions.app.toggleBlockConsolidationStatusScreen.listen(
      this._toggleBlockConsolidationStatusScreen
    );

    // TODO: refactor to ipc channels
    ipcRenderer.on(OPEN_ABOUT_DIALOG_CHANNEL, this._openAboutDialog);
    ipcRenderer.on(GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL, this._goToAdaRedemptionScreen);
    ipcRenderer.on(GO_TO_NETWORK_STATUS_SCREEN_CHANNEL, this._goToNetworkStatusScreen);
    ipcRenderer.on(
      GO_TO_BLOCK_CONSOLIDATION_STATUS_CHANNEL,
      this._toggleBlockConsolidationStatusScreen
    );
    ipcRenderer.on(GET_GPU_STATUS.SUCCESS, this._onGetGpuStatusSuccess);
  }

  teardown() {
    // TODO: refactor to ipc channels
    ipcRenderer.removeListener(OPEN_ABOUT_DIALOG_CHANNEL, this._openAboutDialog);
    ipcRenderer.removeListener(GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL, this._goToAdaRedemptionScreen);
    ipcRenderer.removeListener(GO_TO_NETWORK_STATUS_SCREEN_CHANNEL, this._goToNetworkStatusScreen);
    ipcRenderer.removeListener(
      GO_TO_BLOCK_CONSOLIDATION_STATUS_CHANNEL,
      this._toggleBlockConsolidationStatusScreen
    );
  }

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  openExternalLink(url: string, event?: MouseEvent): void {
    if (event) event.preventDefault();
    openExternalUrlChannel.send(url);
  }

  _getGpuStatus = () => {
    // TODO: refactor to ipc channel
    ipcRenderer.send(GET_GPU_STATUS.REQUEST);
  };

  _onGetGpuStatusSuccess = action((event, status) => {
    this.gpuStatus = status;
  });

  _updateRouteLocation = (options: { route: string, params: ?Object }) => {
    const routePath = buildRoute(options.route, options.params);
    const currentRoute = this.stores.router.location.pathname;
    if (currentRoute !== routePath) this.stores.router.push(routePath);
    this._updatePreviousRoute(currentRoute);
  };

  @action _updatePreviousRoute = (currentRoute?: string) => {
    this.previousRoute = currentRoute;
  }

  @action _openAboutDialog = () => {
    this.isAboutDialogOpen = true;
  };

  @action _closeAboutDialog = () => {
    this.isAboutDialogOpen = false;
  };

  @computed get isSetupPage(): boolean {
    return (
      this.currentRoute === ROUTES.PROFILE.LANGUAGE_SELECTION ||
      this.currentRoute === ROUTES.PROFILE.TERMS_OF_USE
    );
  }

  @action _goToAdaRedemptionScreen = () => {
    const { isConnected, isSynced } = this.stores.networkStatus;
    const { hasLoadedWallets } = this.stores.wallets;
    if (isConnected && isSynced && hasLoadedWallets && !this.isSetupPage) {
      this.actions.router.goToRoute.trigger({ route: ROUTES.ADA_REDEMPTION });
    }
  };

  @computed get isNetworkStatusPage(): boolean {
    return this.currentRoute === ROUTES.NETWORK_STATUS;
  }

  @computed get isBlockConsolidationStatusPage(): boolean {
    return this.currentRoute === ROUTES.BLOCK_CONSOLIDATION_STATUS;
  }

  @action _goToNetworkStatusScreen = () => {
    const route = this.isNetworkStatusPage ? ROUTES.ROOT : ROUTES.NETWORK_STATUS;
    this.actions.router.goToRoute.trigger({ route });
  };

  @action _toggleBlockConsolidationStatusScreen = () => {
    const route = this.isBlockConsolidationStatusPage
      ? this.previousRoute
      : ROUTES.BLOCK_CONSOLIDATION_STATUS;
    this._updateRouteLocation({ route });
  };

}
