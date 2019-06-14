// @flow
import { observable, computed, action, runInAction } from 'mobx';

import Store from './lib/Store';
import LocalizableError from '../i18n/LocalizableError';
import { buildRoute } from '../utils/routing';
import { ROUTES } from '../routes-config';
import { DIALOGS, SCREENS, NOTIFICATIONS } from '../../../common/ipc/constants';
import { openExternalUrlChannel } from '../ipc/open-external-url';
import {
  toggleUiPartChannel,
  showUiPartChannel,
} from '../ipc/control-ui-parts';
import { getGPUStatusChannel } from '../ipc/get-gpu-status.ipc';
import { generateFileNameWithTimestamp } from '../../../common/utils/files';

import type { GpuStatus } from '../types/gpuStatus';

export default class AppStore extends Store {
  @observable error: ?LocalizableError = null;
  @observable isAboutDialogOpen = false;
  @observable isDaedalusDiagnosticsDialogOpen = false;
  @observable isDownloadNotificationVisible = false;
  @observable gpuStatus: ?GpuStatus = null;
  @observable numberOfEpochsConsolidated: number = 0;
  @observable previousRoute: string = ROUTES.ROOT;

  setup() {
    this.actions.router.goToRoute.listen(this._updateRouteLocation);
    this.actions.app.openAboutDialog.listen(this._openAboutDialog);
    this.actions.app.closeAboutDialog.listen(this._closeAboutDialog);
    this.actions.app.openDaedalusDiagnosticsDialog.listen(
      this._openDaedalusDiagnosticsDialog
    );
    this.actions.app.closeDaedalusDiagnosticsDialog.listen(
      this._closeDaedalusDiagnosticsDialog
    );
    this.actions.app.getGpuStatus.listen(this._getGpuStatus);
    this.actions.app.toggleBlockConsolidationStatusScreen.listen(
      this._toggleBlockConsolidationStatusScreen
    );

    this.actions.app.downloadLogs.listen(this._downloadLogs);

    this.actions.app.setNotificationVisibility.listen(
      this._setDownloadNotification
    );

    toggleUiPartChannel.onReceive(this.toggleUiPart);
    showUiPartChannel.onReceive(this.showUiPart);
  }

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  @computed get currentPage(): string {
    return this.currentRoute.split('/').pop();
  }

  openExternalLink(url: string, event?: MouseEvent): void {
    if (event) event.preventDefault();
    openExternalUrlChannel.send(url);
  }

  /**
   * Toggles the dialog specified by the constant string identifier.
   */
  toggleUiPart = (uiPart: string) => {
    switch (uiPart) {
      case DIALOGS.ABOUT:
        this._toggleAboutDialog();
        break;
      case DIALOGS.DAEDALUS_DIAGNOSTICS:
        this._toggleDaedalusDiagnosticsDialog();
        break;
      case SCREENS.BLOCK_CONSOLIDATION:
        this._toggleBlockConsolidationStatusScreen();
        break;
      default:
    }
    return Promise.resolve();
  };

  /**
   * Shows the screen specified by the constant string identifier.
   */
  showUiPart = (uiPart: string) => {
    switch (uiPart) {
      case SCREENS.ADA_REDEMPTION:
        this._showAdaRedemptionScreen();
        break;
      case NOTIFICATIONS.DOWNLOAD_LOGS:
        this._downloadLogs();
        break;
      default:
    }
    return Promise.resolve();
  };

  @computed get isBlockConsolidationStatusDialog(): boolean {
    return this.currentRoute === ROUTES.BLOCK_CONSOLIDATION_STATUS;
  }

  @computed get isSetupPage(): boolean {
    return (
      this.currentRoute === ROUTES.PROFILE.LANGUAGE_SELECTION ||
      this.currentRoute === ROUTES.PROFILE.TERMS_OF_USE
    );
  }

  // ===================== PRIVATE ======================= //

  _getGpuStatus = async () => {
    const gpuStatus = await getGPUStatusChannel.request();
    runInAction('get gpu status', () => {
      this.gpuStatus = gpuStatus;
    });
  };

  _updateRouteLocation = (options: { route: string, params?: ?Object }) => {
    const routePath = buildRoute(options.route, options.params);
    const currentRoute = this.stores.router.location.pathname;
    if (currentRoute !== routePath) this.stores.router.push(routePath);
    this._updatePreviousRoute(currentRoute);
  };

  @action _updatePreviousRoute = (currentRoute?: string) => {
    this.previousRoute = currentRoute || ROUTES.ROOT;
  };

  @action _openAboutDialog = () => {
    this.isAboutDialogOpen = true;
  };

  @action _closeAboutDialog = () => {
    this.isAboutDialogOpen = false;
  };

  @action _toggleAboutDialog = () => {
    this.isAboutDialogOpen = !this.isAboutDialogOpen;
  };

  @action _openDaedalusDiagnosticsDialog = () => {
    this.isDaedalusDiagnosticsDialogOpen = true;
  };

  @action _closeDaedalusDiagnosticsDialog = () => {
    this.isDaedalusDiagnosticsDialogOpen = false;
  };

  @action _toggleDaedalusDiagnosticsDialog = () => {
    this.isDaedalusDiagnosticsDialogOpen = !this
      .isDaedalusDiagnosticsDialogOpen;
  };

  @action _showAdaRedemptionScreen = () => {
    const { isConnected, isSynced } = this.stores.networkStatus;
    const { hasLoadedWallets } = this.stores.wallets;
    if (isConnected && isSynced && hasLoadedWallets && !this.isSetupPage) {
      this.actions.router.goToRoute.trigger({ route: ROUTES.ADA_REDEMPTION });
    }
  };

  @action _toggleBlockConsolidationStatusScreen = () => {
    const route = this.isBlockConsolidationStatusDialog
      ? this.previousRoute
      : ROUTES.BLOCK_CONSOLIDATION_STATUS;
    this._updateRouteLocation({ route });
  };

  @action _downloadLogs = () => {
    if (this.isDownloadNotificationVisible) {
      return;
    }
    const fileName = generateFileNameWithTimestamp();
    global.dialog.showSaveDialog(
      {
        defaultPath: fileName,
      },
      destination => {
        if (destination) {
          this.actions.profile.downloadLogs.trigger({
            fileName,
            destination,
            fresh: true,
          });
        } else {
          this.actions.app.setNotificationVisibility.trigger(
            !this.isDownloadNotificationVisible
          );
        }
      }
    );
    this.isDownloadNotificationVisible = true;
  };

  @action _setDownloadNotification = (
    isDownloadNotificationVisible: boolean
  ) => {
    this.isDownloadNotificationVisible = isDownloadNotificationVisible;
  };
}
