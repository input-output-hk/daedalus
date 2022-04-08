import { observable, computed, action, runInAction } from 'mobx';
import path from 'path';
import Store from './lib/Store';
import LocalizableError from '../i18n/LocalizableError';
import { buildRoute } from '../utils/routing';
import { ROUTES } from '../routes-config';
import { DIALOGS, PAGES, NOTIFICATIONS } from '../../../common/ipc/constants';
import { openExternalUrlChannel } from '../ipc/open-external-url';
import { showSaveDialogChannel } from '../ipc/show-file-dialog-channels';
import {
  toggleUiPartChannel,
  showUiPartChannel,
} from '../ipc/control-ui-parts';
import { getGPUStatusChannel } from '../ipc/get-gpu-status.ipc';
import { generateFileNameWithTimestamp } from '../../../common/utils/files';
import type { GpuStatus } from '../types/gpuStatus';
import type { ApplicationDialog } from '../types/applicationDialogTypes';

export default class AppStore extends Store {
  @observable
  error: LocalizableError | null | undefined = null;
  @observable
  isDownloadNotificationVisible = false;
  @observable
  gpuStatus: GpuStatus | null | undefined = null;
  @observable
  activeDialog: ApplicationDialog = null;
  @observable
  newsFeedIsOpen = false;

  setup() {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AppStor... Remove this comment to see the full error message
    this.actions.router.goToRoute.listen(this._updateRouteLocation);
    // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AppStor... Remove this comment to see the full error message
    this.actions.app.getGpuStatus.listen(this._getGpuStatus);

    this._getGpuStatus();

    // About dialog actions
    // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AppStor... Remove this comment to see the full error message
    this.actions.app.closeAboutDialog.listen(() => {
      this._closeActiveDialog();
    });
    // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AppStor... Remove this comment to see the full error message
    this.actions.app.openAboutDialog.listen(() => {
      this._updateActiveDialog(DIALOGS.ABOUT);
    });
    // Daedalus Diagnostics dialog actions
    // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AppStor... Remove this comment to see the full error message
    this.actions.app.closeDaedalusDiagnosticsDialog.listen(() => {
      this._closeActiveDialog();
    });
    // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AppStor... Remove this comment to see the full error message
    this.actions.app.openDaedalusDiagnosticsDialog.listen(() => {
      this._updateActiveDialog(DIALOGS.DAEDALUS_DIAGNOSTICS);
    });
    this.actions.app.closeToggleRTSFlagsModeDialog.listen(() => {
      this._closeActiveDialog();
    });
    this.actions.app.openToggleRTSFlagsModeDialog.listen(() => {
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
      this._updateActiveDialog(DIALOGS.TOGGLE_RTS_FLAGS_MODE);
    });
    this.actions.app.downloadLogs.listen(this._downloadLogs);
    // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AppStor... Remove this comment to see the full error message
    this.actions.app.setIsDownloadingLogs.listen(this._setIsDownloadingLogs);
    // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AppStor... Remove this comment to see the full error message
    this.actions.app.toggleNewsFeed.listen(this._toggleNewsFeed);
    // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AppStor... Remove this comment to see the full error message
    this.actions.app.closeNewsFeed.listen(this._closeNewsFeed);
    toggleUiPartChannel.onReceive(this.toggleUiPart);
    showUiPartChannel.onReceive(this.showUiPart);
  }

  @computed
  get currentRoute(): string {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'AppStore... Remove this comment to see the full error message
    const { location } = this.stores.router;
    return location ? location.pathname : '';
  }

  @computed
  get currentPage(): string {
    return this.currentRoute.split('/').pop();
  }

  openExternalLink(url: string, event?: MouseEvent): void {
    if (event) event.preventDefault();
    openExternalUrlChannel.send(url);
  }

  isActiveDialog = (dialog: ApplicationDialog): boolean => {
    return this.activeDialog === dialog;
  };
  @action
  _toggleNewsFeed = () => {
    this.newsFeedIsOpen = !this.newsFeedIsOpen;
  };
  @action
  _closeNewsFeed = () => {
    this.newsFeedIsOpen = false;
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
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'AppStore... Remove this comment to see the full error message
    const { wallets } = this.stores;

    switch (uiPart) {
      case DIALOGS.ABOUT:
        this._updateActiveDialog(DIALOGS.ABOUT);

        break;

      case DIALOGS.DAEDALUS_DIAGNOSTICS:
        this._updateActiveDialog(DIALOGS.DAEDALUS_DIAGNOSTICS);

        break;

      case DIALOGS.TOGGLE_RTS_FLAGS_MODE:
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        this._updateActiveDialog(DIALOGS.TOGGLE_RTS_FLAGS_MODE);

        break;

      case DIALOGS.ITN_REWARDS_REDEMPTION:
        // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AppStor... Remove this comment to see the full error message
        this.actions.staking.onRedeemStart.trigger();
        break;

      case NOTIFICATIONS.DOWNLOAD_LOGS:
        this._downloadLogs();

        break;

      case PAGES.SETTINGS:
        // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AppStor... Remove this comment to see the full error message
        this.actions.router.goToRoute.trigger({
          route: ROUTES.SETTINGS.ROOT,
        });
        // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AppStor... Remove this comment to see the full error message
        this.actions.dialogs.closeActiveDialog.trigger();
        break;

      case PAGES.WALLET_SETTINGS:
        if (wallets.active && wallets.active.id) {
          // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AppStor... Remove this comment to see the full error message
          this.actions.router.goToRoute.trigger({
            route: ROUTES.WALLETS.PAGE,
            params: {
              id: wallets.active.id,
              page: 'settings',
            },
          });
          // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AppStor... Remove this comment to see the full error message
          this.actions.dialogs.closeActiveDialog.trigger();
        }

        break;

      default:
    }

    return Promise.resolve();
  };

  @computed
  get isSetupPage(): boolean {
    return (
      this.currentRoute === ROUTES.PROFILE.INITIAL_SETTINGS ||
      this.currentRoute === ROUTES.PROFILE.TERMS_OF_USE ||
      this.currentRoute === ROUTES.PROFILE.ANALYTICS
    );
  }

  // ===================== PRIVATE ======================= //
  _getGpuStatus = async () => {
    const gpuStatus = await getGPUStatusChannel.request();
    runInAction('get gpu status', () => {
      this.gpuStatus = gpuStatus;
    });
  };
  @action
  _updateRouteLocation = (options: {
    route: string;
    params?: Record<string, any> | null | undefined;
  }) => {
    const newRoutePath = buildRoute(options.route, options.params);

    if (this.currentRoute !== newRoutePath) {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'AppStore... Remove this comment to see the full error message
      this.stores.router.push(newRoutePath);
    }
  };
  @action
  _updateActiveDialog = (currentDialog: ApplicationDialog) => {
    if (this.newsFeedIsOpen) {
      this.newsFeedIsOpen = false;
    }
    if (this.activeDialog !== currentDialog) this.activeDialog = currentDialog;
  };
  @action
  _closeActiveDialog = () => {
    if (this.activeDialog !== null) this.activeDialog = null;
  };
  @action
  _downloadLogs = async () => {
    if (this.isDownloadNotificationVisible) {
      return;
    }

    const fileName = generateFileNameWithTimestamp();
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'AppStore... Remove this comment to see the full error message
    const { desktopDirectoryPath } = this.stores.profile;
    const defaultPath = path.join(desktopDirectoryPath, fileName);
    const params = {
      defaultPath,
    };
    const { filePath } = await showSaveDialogChannel.send(params);

    if (filePath) {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AppStor... Remove this comment to see the full error message
      this.actions.app.setIsDownloadingLogs.trigger(true);
      // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AppStor... Remove this comment to see the full error message
      this.actions.profile.downloadLogs.trigger({
        fileName,
        destination: filePath,
        fresh: true,
      });
    } else {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AppStor... Remove this comment to see the full error message
      this.actions.app.setIsDownloadingLogs.trigger(false);
    }
  };
  @action
  _setIsDownloadingLogs = (isDownloadNotificationVisible: boolean) => {
    this.isDownloadNotificationVisible = isDownloadNotificationVisible;
  };
}
