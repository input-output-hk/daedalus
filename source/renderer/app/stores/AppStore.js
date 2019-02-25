// @flow
import { observable, computed, action } from 'mobx';
import Store from './lib/Store';
import LocalizableError from '../i18n/LocalizableError';
import { buildRoute } from '../utils/routing';
import {
  TOGGLE_ABOUT_DIALOG_CHANNEL,
  TOGGLE_NETWORK_STATUS_DIALOG_CHANNEL,
  TOGGLE_BLOCK_CONSOLIDATION_STATUS_SCREEN_CHANNEL,
  GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL
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
  @observable isNetworkStatusDialogOpen = false;
  @observable gpuStatus: ?GpuStatus = null;
  @observable numberOfEpochsConsolidated: number = 0;
  @observable previousRoute: string = ROUTES.ROOT;

  setup() {
    this.actions.router.goToRoute.listen(this._updateRouteLocation);
    this.actions.app.openAboutDialog.listen(this._openAboutDialog);
    this.actions.app.closeAboutDialog.listen(this._closeAboutDialog);
    this.actions.app.openNetworkStatusDialog.listen(this._openNetworkStatusDialog);
    this.actions.app.closeNetworkStatusDialog.listen(this._closeNetworkStatusDialog);
    this.actions.app.getGpuStatus.listen(this._getGpuStatus);
    this.actions.app.toggleBlockConsolidationStatusScreen.listen(
      this._toggleBlockConsolidationStatusScreen
    );

    /* eslint-disable max-len */
    // TODO: refactor to ipc channels
    ipcRenderer.on(TOGGLE_ABOUT_DIALOG_CHANNEL, this._toggleAboutDialog);
    ipcRenderer.on(TOGGLE_NETWORK_STATUS_DIALOG_CHANNEL, this._toggleNetworkStatusDialog);
    ipcRenderer.on(TOGGLE_BLOCK_CONSOLIDATION_STATUS_SCREEN_CHANNEL, this._toggleBlockConsolidationStatusScreen);
    ipcRenderer.on(GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL, this._goToAdaRedemptionScreen);
    ipcRenderer.on(GET_GPU_STATUS.SUCCESS, this._onGetGpuStatusSuccess);
    /* eslint-disable max-len */
  }

  teardown() {
    /* eslint-disable max-len */
    // TODO: refactor to ipc channels
    ipcRenderer.removeListener(TOGGLE_ABOUT_DIALOG_CHANNEL, this._toggleAboutDialog);
    ipcRenderer.removeListener(TOGGLE_NETWORK_STATUS_DIALOG_CHANNEL, this._toggleNetworkStatusDialog);
    ipcRenderer.removeListener(TOGGLE_BLOCK_CONSOLIDATION_STATUS_SCREEN_CHANNEL, this._toggleBlockConsolidationStatusScreen);
    ipcRenderer.removeListener(GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL, this._goToAdaRedemptionScreen);
    ipcRenderer.removeListener(GET_GPU_STATUS.SUCCESS, this._onGetGpuStatusSuccess);
    /* eslint-disable max-len */
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

  _updateRouteLocation = (options: { route: string, params?: ?Object }) => {
    const routePath = buildRoute(options.route, options.params);
    const currentRoute = this.stores.router.location.pathname;
    if (currentRoute !== routePath) this.stores.router.push(routePath);
    this._updatePreviousRoute(currentRoute);
  };

  @action _updatePreviousRoute = (currentRoute?: string) => {
    this.previousRoute = currentRoute || ROUTES.ROOT;
  }

  @action _openAboutDialog = () => {
    this.isAboutDialogOpen = true;
  };

  @action _closeAboutDialog = () => {
    this.isAboutDialogOpen = false;
  };

  @action _toggleAboutDialog = () => {
    this.isAboutDialogOpen = !this.isAboutDialogOpen;
  };

  @action _openNetworkStatusDialog = () => {
    this.isNetworkStatusDialogOpen = true;
  };

  @action _closeNetworkStatusDialog = () => {
    this.isNetworkStatusDialogOpen = false;
  };

  @action _toggleNetworkStatusDialog = () => {
    this.isNetworkStatusDialogOpen = !this.isNetworkStatusDialogOpen;
  };

  @action _goToAdaRedemptionScreen = () => {
    const { isConnected, isSynced } = this.stores.networkStatus;
    const { hasLoadedWallets } = this.stores.wallets;
    if (isConnected && isSynced && hasLoadedWallets && !this.isSetupPage) {
      this.actions.router.goToRoute.trigger({ route: ROUTES.ADA_REDEMPTION });
    }
  };

  @action _toggleBlockConsolidationStatusScreen = () => {
    const route = this.isBlockConsolidationStatusPage
      ? this.previousRoute
      : ROUTES.BLOCK_CONSOLIDATION_STATUS;
    this._updateRouteLocation({ route });
  };

  @computed get isBlockConsolidationStatusPage(): boolean {
    return this.currentRoute === ROUTES.BLOCK_CONSOLIDATION_STATUS;
  }

  @computed get isSetupPage(): boolean {
    return (
      this.currentRoute === ROUTES.PROFILE.LANGUAGE_SELECTION ||
      this.currentRoute === ROUTES.PROFILE.TERMS_OF_USE
    );
  }

}
