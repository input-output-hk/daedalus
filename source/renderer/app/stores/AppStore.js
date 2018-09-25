// @flow
import { observable, computed, action } from 'mobx';
import { ipcRenderer, shell } from 'electron';
import Store from './lib/Store';
import LocalizableError from '../i18n/LocalizableError';
import { buildRoute } from '../utils/routing';
import { OPEN_ABOUT_DIALOG_CHANNEL } from '../../../common/ipc-api/open-about-dialog';
import { GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL } from '../../../common/ipc-api/go-to-ada-redemption-screen';
import { GO_TO_NETWORK_STATUS_SCREEN_CHANNEL } from '../../../common/ipc-api/go-to-network-status-screen';
import { GET_GPU_STATUS } from '../../../common/ipc-api';
import { INIT_ENVIRONMENT } from '../../../common/ipc-api/init-environment';
import { ROUTES } from '../routes-config';
import type GpuStatus from '../types/gpuStatus';

export default class AppStore extends Store {

  @observable error: ?LocalizableError = null;
  @observable isAboutDialogOpen = false;
  @observable gpuStatus: ?GpuStatus = {};
  @observable enviroment: ?Object = null;

  setup() {
    this.actions.router.goToRoute.listen(this._updateRouteLocation);
    this.actions.app.openAboutDialog.listen(this._openAboutDialog);
    this.actions.app.closeAboutDialog.listen(this._closeAboutDialog);
    this.actions.app.getGpuStatus.listen(this._getGpuStatus);
    this.actions.app.initEnvironment.listen(this._initEnvironment);
    ipcRenderer.on(OPEN_ABOUT_DIALOG_CHANNEL, this._openAboutDialog);
    ipcRenderer.on(GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL, this._goToAdaRedemptionScreen);
    ipcRenderer.on(GO_TO_NETWORK_STATUS_SCREEN_CHANNEL, this._goToNetworkStatusScreen);
    ipcRenderer.on(GET_GPU_STATUS.SUCCESS, this._onGetGpuStatusSuccess);
    ipcRenderer.on(INIT_ENVIRONMENT, this._initEnvironment);
  }

  teardown() {
    ipcRenderer.removeListener(OPEN_ABOUT_DIALOG_CHANNEL, this._openAboutDialog);
    ipcRenderer.removeListener(GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL, this._goToAdaRedemptionScreen);
    ipcRenderer.removeListener(GO_TO_NETWORK_STATUS_SCREEN_CHANNEL, this._goToNetworkStatusScreen);
  }

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  openExternalLink(link: string): void {
    shell.openExternal(link);
  }

  _getGpuStatus = () => {
    ipcRenderer.send(GET_GPU_STATUS.REQUEST);
  };

  _onGetGpuStatusSuccess = action((event, status) => {
    this.gpuStatus = status;
  });

  _updateRouteLocation = (options: { route: string, params: ?Object }) => {
    const routePath = buildRoute(options.route, options.params);
    const currentRoute = this.stores.router.location.pathname;
    if (currentRoute !== routePath) this.stores.router.push(routePath);
  };

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
    const { hasLoadedWallets } = this.stores[this.environment.API].wallets;
    if (isConnected && isSynced && hasLoadedWallets && !this.isSetupPage) {
      this.actions.router.goToRoute.trigger({ route: ROUTES.ADA_REDEMPTION });
    }
  };

  @action _initEnvironment = (environment: Object) => {
    this.environment = environment;
  };

  @computed get isNetworkStatusPage(): boolean {
    return this.currentRoute === ROUTES.NETWORK_STATUS;
  }

  @action _goToNetworkStatusScreen = () => {
    const route = this.isNetworkStatusPage ? ROUTES.ROOT : ROUTES.NETWORK_STATUS;
    this.actions.router.goToRoute.trigger({ route });
  };
}
