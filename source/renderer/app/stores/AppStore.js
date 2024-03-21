'use strict';
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const mobx_1 = require('mobx');
const path_1 = __importDefault(require('path'));
const Store_1 = __importDefault(require('./lib/Store'));
const LocalizableError_1 = __importDefault(require('../i18n/LocalizableError'));
const routing_1 = require('../utils/routing');
const routes_config_1 = require('../routes-config');
const constants_1 = require('../../../common/ipc/constants');
const open_external_url_1 = require('../ipc/open-external-url');
const show_file_dialog_channels_1 = require('../ipc/show-file-dialog-channels');
const control_ui_parts_1 = require('../ipc/control-ui-parts');
const get_gpu_status_ipc_1 = require('../ipc/get-gpu-status.ipc');
const files_1 = require('../../../common/utils/files');
const analytics_1 = require('../analytics');
class AppStore extends Store_1.default {
  error = null;
  isDownloadNotificationVisible = false;
  gpuStatus = null;
  activeDialog = null;
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
      this._updateActiveDialog(constants_1.DIALOGS.ABOUT);
    });
    // Daedalus Diagnostics dialog actions
    this.actions.app.closeDaedalusDiagnosticsDialog.listen(() => {
      this._closeActiveDialog();
    });
    this.actions.app.openDaedalusDiagnosticsDialog.listen(() => {
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
      this._updateActiveDialog(constants_1.DIALOGS.DAEDALUS_DIAGNOSTICS);
    });
    this.actions.app.closeToggleRTSFlagsModeDialog.listen(() => {
      this._closeActiveDialog();
    });
    this.actions.app.openToggleRTSFlagsModeDialog.listen(() => {
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
      this._updateActiveDialog(constants_1.DIALOGS.TOGGLE_RTS_FLAGS_MODE);
    });
    this.actions.app.downloadLogs.listen(this._downloadLogs);
    this.actions.app.setIsDownloadingLogs.listen(this._setIsDownloadingLogs);
    this.actions.app.toggleNewsFeed.listen(this._toggleNewsFeed);
    this.actions.app.closeNewsFeed.listen(this._closeNewsFeed);
    control_ui_parts_1.toggleUiPartChannel.onReceive(this.toggleUiPart);
    control_ui_parts_1.showUiPartChannel.onReceive(this.showUiPart);
  }
  get currentRoute() {
    const { location } = this.stores.router;
    return location ? location.pathname : '';
  }
  get currentPage() {
    return this.currentRoute.split('/').pop();
  }
  openExternalLink(url, event) {
    if (event) event.preventDefault();
    open_external_url_1.openExternalUrlChannel.send(url);
  }
  isActiveDialog = (dialog) => {
    return this.activeDialog === dialog;
  };
  _toggleNewsFeed = () => {
    this.newsFeedIsOpen = !this.newsFeedIsOpen;
    if (this.newsFeedIsOpen) {
      this.analytics.sendEvent(
        analytics_1.EventCategories.LAYOUT,
        'Opened newsfeed'
      );
    }
  };
  _closeNewsFeed = () => {
    this.newsFeedIsOpen = false;
  };
  /**
   * Toggles the dialog specified by the constant string identifier.
   */
  toggleUiPart = (uiPart) => {
    switch (uiPart) {
      default:
    }
    return Promise.resolve();
  };
  /**
   * Shows the screen specified by the constant string identifier.
   */
  showUiPart = (uiPart) => {
    const { wallets } = this.stores;
    switch (uiPart) {
      case constants_1.DIALOGS.ABOUT:
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        this._updateActiveDialog(constants_1.DIALOGS.ABOUT);
        this.analytics.sendEvent(
          analytics_1.EventCategories.SYSTEM_MENU,
          'Showed about dialog'
        );
        break;
      case constants_1.DIALOGS.DAEDALUS_DIAGNOSTICS:
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        this._updateActiveDialog(constants_1.DIALOGS.DAEDALUS_DIAGNOSTICS);
        this.analytics.sendEvent(
          analytics_1.EventCategories.SYSTEM_MENU,
          'Showed diagnostics dialog'
        );
        break;
      case constants_1.DIALOGS.TOGGLE_RTS_FLAGS_MODE:
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        this._updateActiveDialog(constants_1.DIALOGS.TOGGLE_RTS_FLAGS_MODE);
        this.analytics.sendEvent(
          analytics_1.EventCategories.SYSTEM_MENU,
          'Showed toggle RTS flags dialog'
        );
        break;
      case constants_1.DIALOGS.ITN_REWARDS_REDEMPTION:
        // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
        this.actions.staking.onRedeemStart.trigger();
        this.analytics.sendEvent(
          analytics_1.EventCategories.SYSTEM_MENU,
          'Showed ITN rewards redemption dialog'
        );
        break;
      case constants_1.NOTIFICATIONS.DOWNLOAD_LOGS:
        this._downloadLogs();
        this.analytics.sendEvent(
          analytics_1.EventCategories.SYSTEM_MENU,
          'Downloaded logs'
        );
        break;
      case constants_1.PAGES.SETTINGS:
        this.actions.router.goToRoute.trigger({
          route: routes_config_1.ROUTES.SETTINGS.ROOT,
        });
        // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
        this.actions.dialogs.closeActiveDialog.trigger();
        break;
      case constants_1.PAGES.WALLET_SETTINGS:
        if (wallets.active && wallets.active.id) {
          this.actions.router.goToRoute.trigger({
            route: routes_config_1.ROUTES.WALLETS.PAGE,
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
  get isSetupPage() {
    return (
      this.currentRoute === routes_config_1.ROUTES.PROFILE.INITIAL_SETTINGS ||
      this.currentRoute === routes_config_1.ROUTES.PROFILE.TERMS_OF_USE ||
      this.currentRoute === routes_config_1.ROUTES.PROFILE.ANALYTICS
    );
  }
  // ===================== PRIVATE ======================= //
  _getGpuStatus = async () => {
    const gpuStatus = await get_gpu_status_ipc_1.getGPUStatusChannel.request();
    (0, mobx_1.runInAction)('get gpu status', () => {
      this.gpuStatus = gpuStatus;
    });
  };
  _updateRouteLocation = (options) => {
    const newRoutePath = (0, routing_1.buildRoute)(
      options.route,
      options.params
    );
    if (this.currentRoute !== newRoutePath) {
      this.stores.router.push(newRoutePath);
    }
  };
  _updateActiveDialog = (currentDialog) => {
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
    const fileName = (0, files_1.generateFileNameWithTimestamp)();
    const { desktopDirectoryPath } = this.stores.profile;
    const defaultPath = path_1.default.join(desktopDirectoryPath, fileName);
    const params = {
      defaultPath,
    };
    const {
      filePath,
    } = await show_file_dialog_channels_1.showSaveDialogChannel.send(params);
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
  _setIsDownloadingLogs = (isDownloadNotificationVisible) => {
    this.isDownloadNotificationVisible = isDownloadNotificationVisible;
  };
}
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizableError_1.default)],
  AppStore.prototype,
  'error',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  AppStore.prototype,
  'isDownloadNotificationVisible',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  AppStore.prototype,
  'gpuStatus',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  AppStore.prototype,
  'activeDialog',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  AppStore.prototype,
  'newsFeedIsOpen',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  AppStore.prototype,
  'currentRoute',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  AppStore.prototype,
  'currentPage',
  null
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AppStore.prototype,
  '_toggleNewsFeed',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AppStore.prototype,
  '_closeNewsFeed',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  AppStore.prototype,
  'isSetupPage',
  null
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AppStore.prototype,
  '_updateRouteLocation',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AppStore.prototype,
  '_updateActiveDialog',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AppStore.prototype,
  '_closeActiveDialog',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AppStore.prototype,
  '_downloadLogs',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AppStore.prototype,
  '_setIsDownloadingLogs',
  void 0
);
exports.default = AppStore;
//# sourceMappingURL=AppStore.js.map
