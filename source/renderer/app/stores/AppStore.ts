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
    this.actions.router.goToRoute.listen(this._updateRouteLocation);
    this.actions.app.getGpuStatus.listen(this._getGpuStatus);

    this._getGpuStatus();

    // About dialog actions
    this.actions.app.closeAboutDialog.listen(() => {
      this._closeActiveDialog();
    });
    this.actions.app.openAboutDialog.listen(() => {
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
      this._updateActiveDialog(DIALOGS.ABOUT);
    });
    // Daedalus Diagnostics dialog actions
    this.actions.app.closeDaedalusDiagnosticsDialog.listen(() => {
      this._closeActiveDialog();
    });
    this.actions.app.openDaedalusDiagnosticsDialog.listen(() => {
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
      this._updateActiveDialog(DIALOGS.DAEDALUS_DIAGNOSTICS);
    });
    this.actions.app.downloadLogs.listen(this._downloadLogs);
    this.actions.app.setIsDownloadingLogs.listen(this._setIsDownloadingLogs);
    this.actions.app.toggleNewsFeed.listen(this._toggleNewsFeed);
    this.actions.app.closeNewsFeed.listen(this._closeNewsFeed);
    toggleUiPartChannel.onReceive(this.toggleUiPart);
    showUiPartChannel.onReceive(this.showUiPart);
  }

  @computed
  get currentRoute(): string {
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
    const { wallets } = this.stores;

    switch (uiPart) {
      case DIALOGS.ABOUT:
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        this._updateActiveDialog(DIALOGS.ABOUT);

        break;

      case DIALOGS.DAEDALUS_DIAGNOSTICS:
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        this._updateActiveDialog(DIALOGS.DAEDALUS_DIAGNOSTICS);

        break;

      case DIALOGS.ITN_REWARDS_REDEMPTION:
        // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
        this.actions.staking.onRedeemStart.trigger();
        break;

      case NOTIFICATIONS.DOWNLOAD_LOGS:
        this._downloadLogs();

        break;

      case PAGES.SETTINGS:
        this.actions.router.goToRoute.trigger({
          route: ROUTES.SETTINGS.ROOT,
        });
        // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
        this.actions.dialogs.closeActiveDialog.trigger();
        break;

      case PAGES.WALLET_SETTINGS:
        if (wallets.active && wallets.active.id) {
          this.actions.router.goToRoute.trigger({
            route: ROUTES.WALLETS.PAGE,
            params: {
              id: wallets.active.id,
              page: 'settings',
            },
          });
          // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
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
  @action
  _updateRouteLocation = (options: {
    route: string;
    params?: Record<string, any> | null | undefined;
  }) => {
    const newRoutePath = buildRoute(options.route, options.params);

    if (this.currentRoute !== newRoutePath) {
      this.stores.router.push(newRoutePath);
    }
  };
  @action
  _updateActiveDialog = (currentDialog: ApplicationDialog) => {
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
    const { desktopDirectoryPath } = this.stores.profile;
    const defaultPath = path.join(desktopDirectoryPath, fileName);
    const params = {
      defaultPath,
    };
    const { filePath } = await showSaveDialogChannel.send(params);

    if (filePath) {
      this.actions.app.setIsDownloadingLogs.trigger(true);
      this.actions.profile.downloadLogs.trigger({
        fileName,
        destination: filePath,
        fresh: true,
      });
    } else {
      this.actions.app.setIsDownloadingLogs.trigger(false);
    }
  };
  @action
  _setIsDownloadingLogs = (isDownloadNotificationVisible: boolean) => {
    this.isDownloadNotificationVisible = isDownloadNotificationVisible;
  };
}
