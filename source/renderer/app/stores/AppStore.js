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
import type { ActiveScreenType } from '../types/activeScreenType';

export default class AppStore extends Store {
  @observable error: ?LocalizableError = null;
  @observable isDownloadNotificationVisible = false;
  @observable gpuStatus: ?GpuStatus = null;
  @observable numberOfEpochsConsolidated: number = 0;
  @observable previousRoute: string = ROUTES.ROOT;
  @observable activeScreen: ActiveScreenType = {
    current: null,
  };

  setup() {
    this.actions.router.goToRoute.listen(this._updateRouteLocation);
    this.actions.app.openAboutDialog.listen(() => {
      this.handleScreensToggle(DIALOGS.ABOUT);
    });
    this.actions.app.closeAboutDialog.listen(() => {
      this.handleScreensToggle(DIALOGS.ABOUT);
    });
    this.actions.app.openDaedalusDiagnosticsDialog.listen(() => {
      this.handleScreensToggle(DIALOGS.DAEDALUS_DIAGNOSTICS);
    });
    this.actions.app.closeDaedalusDiagnosticsDialog.listen(() => {
      this.handleScreensToggle(DIALOGS.DAEDALUS_DIAGNOSTICS);
    });
    this.actions.app.getGpuStatus.listen(this._getGpuStatus);
    this.actions.app.toggleBlockConsolidationStatusScreen.listen(() => {
      this.handleScreensToggle(SCREENS.BLOCK_CONSOLIDATION);
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

  /**
   * Handles screen switching/toggling specified by the screenType string identifier.
   */
  handleScreensToggle = (screenType: string) => {
    let currentScreen = null;
    switch (screenType) {
      case DIALOGS.ABOUT:
        currentScreen = this.activeScreen.current === 'about' ? null : 'about';
        break;
      case DIALOGS.DAEDALUS_DIAGNOSTICS:
        currentScreen =
          this.activeScreen.current === 'daedalusDiagnostics'
            ? null
            : 'daedalusDiagnostics';
        break;
      case SCREENS.BLOCK_CONSOLIDATION:
        currentScreen =
          this.activeScreen.current === 'blockConsolidation'
            ? null
            : 'blockConsolidation';
        break;
      case SCREENS.ADA_REDEMPTION:
        currentScreen =
          this.activeScreen.current === 'adaRedemption'
            ? null
            : 'adaRedemption';
        this._toggleAdaRedemptionScreen();
        break;
      default:
    }
    this._updateActiveScreen(currentScreen);
    return Promise.resolve();
  };

  /**
   * Toggles the dialog specified by the constant string identifier.
   */
  toggleUiPart = (uiPart: string) => {
    switch (uiPart) {
      case DIALOGS.ABOUT:
        this.handleScreensToggle(DIALOGS.ABOUT);
        break;
      case DIALOGS.DAEDALUS_DIAGNOSTICS:
        this.handleScreensToggle(DIALOGS.DAEDALUS_DIAGNOSTICS);
        break;
      case SCREENS.BLOCK_CONSOLIDATION:
        this.handleScreensToggle(SCREENS.BLOCK_CONSOLIDATION);
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
        this.handleScreensToggle(SCREENS.ADA_REDEMPTION);
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

  @action _updateActiveScreen = (currentScreen: string | null) => {
    this.activeScreen.current = currentScreen;
  };

  @action _toggleAdaRedemptionScreen = () => {
    const { isConnected, isSynced } = this.stores.networkStatus;
    const { hasLoadedWallets } = this.stores.wallets;
    if (isConnected && isSynced && hasLoadedWallets && !this.isSetupPage) {
      const route =
        this.activeScreen.current === 'adaRedemption'
          ? this.previousRoute
          : ROUTES.ADA_REDEMPTION;
      this._updateRouteLocation({ route });
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
