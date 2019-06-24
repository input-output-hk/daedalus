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
import type { ApplicationDialog } from '../types/applicationDialogTypes';

export default class AppStore extends Store {
  @observable error: ?LocalizableError = null;
  @observable isDownloadNotificationVisible = false;
  @observable gpuStatus: ?GpuStatus = null;
  @observable numberOfEpochsConsolidated: number = 0;
  @observable previousRoute: string = ROUTES.ROOT;
  @observable activeDialog: ApplicationDialog = null;
  @observable dialogIsFirstTimeClosed: boolean = false;

  setup() {
    this.actions.router.goToRoute.listen(this._updateRouteLocation);
    this.actions.app.getGpuStatus.listen(this._getGpuStatus);

    // About dialog actions
    this.actions.app.closeAboutDialog.listen(() => {
      this._closeActiveDialog();
    });
    this.actions.app.openAboutDialog.listen(() => {
      this._updateActiveDialog(DIALOGS.ABOUT);
    });

    // Block Consolidation dialog actions
    this.actions.app.closeBlockConsolidationStatusDialog.listen(() => {
      this._closeActiveDialog();
    });
    this.actions.app.openBlockConsolidationStatusDialog.listen(() => {
      this._updateActiveDialog(DIALOGS.BLOCK_CONSOLIDATION);
    });

    // Daedalus Diagnostics dialog actions
    this.actions.app.closeDaedalusDiagnosticsDialog.listen(() => {
      this._closeActiveDialog();
    });
    this.actions.app.openDaedalusDiagnosticsDialog.listen(() => {
      this._updateActiveDialog(DIALOGS.DAEDALUS_DIAGNOSTICS);
    });

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

  isActiveDialog = (dialog: ApplicationDialog): boolean => {
    return this.activeDialog === dialog;
  };

  /**
   * Toggles the dialog specified by the constant string identifier.
   */
  toggleUiPart = (uiPart: string) => {
    switch (uiPart) {
      default:
    }
    return Promise.resolve();
  };

  /**
   * Shows the screen specified by the constant string identifier.
   */
  showUiPart = (uiPart: string) => {
    switch (uiPart) {
      case DIALOGS.ABOUT:
        this._updateActiveDialog(DIALOGS.ABOUT);
        break;
      case DIALOGS.BLOCK_CONSOLIDATION:
        this._updateActiveDialog(DIALOGS.BLOCK_CONSOLIDATION);
        break;
      case DIALOGS.DAEDALUS_DIAGNOSTICS:
        this._updateActiveDialog(DIALOGS.DAEDALUS_DIAGNOSTICS);
        break;
      case NOTIFICATIONS.DOWNLOAD_LOGS:
        this._downloadLogs();
        break;
      case SCREENS.ADA_REDEMPTION:
        this._openAdaRedemptionScreen();
        this._closeActiveDialog();
        break;
      default:
    }
    return Promise.resolve();
  };

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

  @action _updateActiveDialog = (currentDialog: ApplicationDialog) => {
    if (this.activeDialog !== currentDialog) this.activeDialog = currentDialog;
  };

  @action _closeActiveDialog = (dialogIsFirstTimeClosed?: boolean) => {
    if (this.activeDialog !== null) this.activeDialog = null;
    if (dialogIsFirstTimeClosed) this.dialogIsFirstTimeClosed = dialogIsFirstTimeClosed;
  };

  @action _openAdaRedemptionScreen = () => {
    const { isConnected, isSynced } = this.stores.networkStatus;
    const { hasLoadedWallets } = this.stores.wallets;
    if (isConnected && isSynced && hasLoadedWallets && !this.isSetupPage) {
      this._updateRouteLocation({ route: ROUTES.ADA_REDEMPTION });
    }
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
