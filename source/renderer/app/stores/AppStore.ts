import {
  observable,
  computed,
  action,
  runInAction,
  makeObservable,
} from 'mobx';
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
import { AnalyticsTracker, EventCategories } from '../analytics';
import { Api } from '../api';
import { ActionsMap } from '../actions';

export default class AppStore extends Store {
  error: LocalizableError | null | undefined = null;
  isDownloadNotificationVisible = false;
  gpuStatus: GpuStatus | null | undefined = null;
  activeDialog: ApplicationDialog = null;
  newsFeedIsOpen = false;

  constructor(api: Api, actions: ActionsMap, analytics: AnalyticsTracker) {
    super(api, actions, analytics);

    makeObservable(this, {
      error: observable,
      isDownloadNotificationVisible: observable,
      gpuStatus: observable,
      activeDialog: observable,
      newsFeedIsOpen: observable,
      currentRoute: computed,
      currentPage: computed,
      _toggleNewsFeed: action,
      _closeNewsFeed: action,
      isSetupPage: computed,
      _updateRouteLocation: action,
      _updateActiveDialog: action,
      _closeActiveDialog: action,
      _downloadLogs: action,
      _setIsDownloadingLogs: action,
    });
  }

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
    this.actions.app.closeToggleRTSFlagsModeDialog.listen(() => {
      this._closeActiveDialog();
    });
    this.actions.app.openToggleRTSFlagsModeDialog.listen(() => {
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
      this._updateActiveDialog(DIALOGS.TOGGLE_RTS_FLAGS_MODE);
    });
    this.actions.app.downloadLogs.listen(this._downloadLogs);
    this.actions.app.setIsDownloadingLogs.listen(this._setIsDownloadingLogs);
    this.actions.app.toggleNewsFeed.listen(this._toggleNewsFeed);
    this.actions.app.closeNewsFeed.listen(this._closeNewsFeed);
    toggleUiPartChannel.onReceive(this.toggleUiPart);
    showUiPartChannel.onReceive(this.showUiPart);
  }

  get currentRoute(): string {
    const { location } = this.stores.router;
    return location ? location.pathname : '';
  }

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
  _toggleNewsFeed = () => {
    this.newsFeedIsOpen = !this.newsFeedIsOpen;

    if (this.newsFeedIsOpen) {
      this.analytics.sendEvent(EventCategories.LAYOUT, 'Opened newsfeed');
    }
  };
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
        this.analytics.sendEvent(
          EventCategories.SYSTEM_MENU,
          'Showed about dialog'
        );
        break;

      case DIALOGS.DAEDALUS_DIAGNOSTICS:
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        this._updateActiveDialog(DIALOGS.DAEDALUS_DIAGNOSTICS);
        this.analytics.sendEvent(
          EventCategories.SYSTEM_MENU,
          'Showed diagnostics dialog'
        );

        break;

      case DIALOGS.TOGGLE_RTS_FLAGS_MODE:
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        this._updateActiveDialog(DIALOGS.TOGGLE_RTS_FLAGS_MODE);
        this.analytics.sendEvent(
          EventCategories.SYSTEM_MENU,
          'Showed toggle RTS flags dialog'
        );
        break;

      case DIALOGS.ITN_REWARDS_REDEMPTION:
        // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
        this.actions.staking.onRedeemStart.trigger();
        this.analytics.sendEvent(
          EventCategories.SYSTEM_MENU,
          'Showed ITN rewards redemption dialog'
        );
        break;

      case NOTIFICATIONS.DOWNLOAD_LOGS:
        this._downloadLogs();
        this.analytics.sendEvent(
          EventCategories.SYSTEM_MENU,
          'Downloaded logs'
        );
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
    runInAction(() => {
      this.gpuStatus = gpuStatus;
    });
  };
  _updateRouteLocation = (options: {
    route: string;
    params?: Record<string, any> | null | undefined;
  }) => {
    const newRoutePath = buildRoute(options.route, options.params);

    if (this.currentRoute !== newRoutePath) {
      this.stores.router.push(newRoutePath);
    }
  };
  _updateActiveDialog = (currentDialog: ApplicationDialog) => {
    if (this.newsFeedIsOpen) {
      this.newsFeedIsOpen = false;
    }
    if (this.activeDialog !== currentDialog) this.activeDialog = currentDialog;
  };
  _closeActiveDialog = () => {
    if (this.activeDialog !== null) this.activeDialog = null;
  };
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
  _setIsDownloadingLogs = (isDownloadNotificationVisible: boolean) => {
    this.isDownloadNotificationVisible = isDownloadNotificationVisible;
  };
}
