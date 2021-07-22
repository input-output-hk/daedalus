// @flow
import { observable, computed, action, runInAction } from 'mobx';
import path from 'path';
import { map } from 'lodash';
import Store from './lib/Store';
import LocalizableError from '../i18n/LocalizableError';
import { buildRoute } from '../utils/routing';
import { ROUTES } from '../routes-config';
import { DIALOGS, PAGES, NOTIFICATIONS } from '../../../common/ipc/constants';
import { openExternalUrlChannel } from '../ipc/open-external-url';
import { showSaveDialogChannel } from '../ipc/show-file-dialog-channels';
import { introspectAddressChannel } from '../ipc/introspect-address';
import {
  toggleUiPartChannel,
  showUiPartChannel,
} from '../ipc/control-ui-parts';
import { getGPUStatusChannel } from '../ipc/get-gpu-status.ipc';
import { getCustomProtocolChannel } from '../ipc/getCustomProtocolChannel';
import { generateFileNameWithTimestamp } from '../../../common/utils/files';
import type { GpuStatus } from '../types/gpuStatus';
import type { ApplicationDialog } from '../types/applicationDialogTypes';

export default class AppStore extends Store {
  @observable error: ?LocalizableError = null;
  @observable isDownloadNotificationVisible = false;
  @observable gpuStatus: ?GpuStatus = null;
  @observable activeDialog: ApplicationDialog = null;
  @observable newsFeedIsOpen: boolean = false;
  // TODO: define all possible query params Daedalus can receive from external source
  @observable customProtocolParameters: ?Object = null;

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

    // Daedalus Diagnostics dialog actions
    this.actions.app.closeDaedalusDiagnosticsDialog.listen(() => {
      this._closeActiveDialog();
    });
    this.actions.app.openDaedalusDiagnosticsDialog.listen(() => {
      this._updateActiveDialog(DIALOGS.DAEDALUS_DIAGNOSTICS);
    });

    this.actions.app.downloadLogs.listen(this._downloadLogs);
    this.actions.app.setIsDownloadingLogs.listen(this._setIsDownloadingLogs);

    this.actions.app.toggleNewsFeed.listen(this._toggleNewsFeed);
    this.actions.app.closeNewsFeed.listen(this._closeNewsFeed);

    toggleUiPartChannel.onReceive(this.toggleUiPart);
    showUiPartChannel.onReceive(this.showUiPart);
    getCustomProtocolChannel.onReceive(this._handleCustomProtocol);

    // ============== MOBX REACTIONS ==============
    this.registerReactions([this._processCustomProtocolParams]);
  }

    // Handle url query data and switch to proper route
  _processCustomProtocolParams = () => {
    // Note: For now we are processing only BASIC tx
    const { appUpdate, wallets,networkStatus } = this.stores;
    const { displayAppUpdateOverlay } = appUpdate;
    const { hasLoadedWallets } = wallets;
    const {
      isConnected,
      isNotEnoughDiskSpace,
      isSystemTimeCorrect,
    } = networkStatus;

    if (
      isConnected &&
      hasLoadedWallets &&
      !isNotEnoughDiskSpace &&
      isSystemTimeCorrect &&
      !displayAppUpdateOverlay &&
      this.customProtocolParameters
    ) {
      if (wallets.hasAnyWallets) {
        const activeWallet = wallets.active || wallets.all[0];
        this.actions.router.goToRoute.trigger({
          route: ROUTES.WALLETS.SEND,
          params: { id: activeWallet.id },
        });
      }
    }
  };

  @computed get currentRoute(): string {
    const { location } = this.stores.router;
    return location ? location.pathname : '';
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

  @action _toggleNewsFeed = () => {
    this.newsFeedIsOpen = !this.newsFeedIsOpen;
  };

  @action _closeNewsFeed = () => {
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
        this._updateActiveDialog(DIALOGS.ABOUT);
        break;
      case DIALOGS.DAEDALUS_DIAGNOSTICS:
        this._updateActiveDialog(DIALOGS.DAEDALUS_DIAGNOSTICS);
        break;
      case DIALOGS.ITN_REWARDS_REDEMPTION:
        this.actions.staking.onRedeemStart.trigger();
        break;
      case NOTIFICATIONS.DOWNLOAD_LOGS:
        this._downloadLogs();
        break;
      case PAGES.SETTINGS:
        this.actions.router.goToRoute.trigger({ route: ROUTES.SETTINGS.ROOT });
        this.actions.dialogs.closeActiveDialog.trigger();
        break;
      case PAGES.WALLET_SETTINGS:
        if (wallets.active && wallets.active.id) {
          this.actions.router.goToRoute.trigger({
            route: ROUTES.WALLETS.PAGE,
            params: { id: wallets.active.id, page: 'settings' },
          });
          this.actions.dialogs.closeActiveDialog.trigger();
        }
        break;
      default:
    }
    return Promise.resolve();
  };

  @computed get isSetupPage(): boolean {
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

  // Parse custom URL and store query params
  @action _handleCustomProtocol = async (url: string) => {
    /* Cardano custom protocol specification (Grammar)
     * https://github.com/cardano-foundation/CIPs/blob/master/CIP-0013/CIP-0013.md

      1. cardanourn = "web+cardano:" (paymentref | stakepoolref)

      paymentref = cardanoaddress [ "?" amountparam ]
      cardanoaddress = *(base58 | bech32)
      amountparam = "amount=" *digit [ "." *digit ]
      Example:
      web+cardano:Ae2tdPwUPEZ76BjmWDTS7poTekAvNqBjgfthF92pSLSDVpRVnLP7meaFhVd
      web+cardano:Ae2tdPwUPEZLs4HtbuNey7tK4hTKrwNwYtGqp7bDfCy2WdR3P6735W5Yfpe?amount=5.000000


      2. stakepoolref = "//stake?" stakepool

      stakepool = poolhexid | poolticker
      poolhexid = 56HEXDIG
      poolticker = 3*5UNICODE
      Example:
      web+cardano://stake?c94e6fe1123bf111b77b57994bcd836af8ba2b3aa72cfcefbec2d3d4
      web+cardano://stake?COSD
    **/

    const queryParams = url.split('cardano:')[1].replace('//', '')
    const actionAndDataParams = queryParams.split('?');
    const actionParam = actionAndDataParams[0];
    const dataParams = actionAndDataParams[1];
    let isAddress;
    try {
      await introspectAddressChannel.send({ input: actionParam });
      isAddress = true;
    } catch (e) {
      isAddress = false;
    }
    if (isAddress && actionParam && dataParams) {
      // For now MVP we are handling just basic tx
      // e.g. web+cardano:Ae2tdPwUPEZLs4HtbuNey7tK4hTKrwNwYtGqp7bDfCy2WdR3P6735W5Yfpe?amount=5.000000
      const parsedQueryParams = dataParams.split('&');
      let parsedParamsData = {};
      map(parsedQueryParams, (queryParam) => {
        const queryParamPair = queryParam.split('=');
        const key = queryParamPair[0];
        const value = queryParamPair[1];
        parsedParamsData[key] = value;
      });
      if (isAddress) {
        parsedParamsData = {
          ...parsedParamsData,
          address: actionParam,
        }
      }
      if (this.stores.wallets.hasAnyWallets) {
        runInAction('Store custom protocol data', () => {
          this.customProtocolParameters = {
            action: isAddress ? 'address' : actionParam, // e.g address / statke /...
            data: parsedParamsData,
          };
        });
      }
    }
  };

  @action resetCustomProptocolParams = () => {
    this.customProtocolParameters = null;
  }

  @action _updateRouteLocation = (options: {
    route: string,
    params?: ?Object,
  }) => {
    const newRoutePath = buildRoute(options.route, options.params);
    if (this.currentRoute !== newRoutePath) {
      this.stores.router.push(newRoutePath);
    }
  };

  @action _updateActiveDialog = (currentDialog: ApplicationDialog) => {
    if (this.activeDialog !== currentDialog) this.activeDialog = currentDialog;
  };

  @action _closeActiveDialog = () => {
    if (this.activeDialog !== null) this.activeDialog = null;
  };

  @action _downloadLogs = async () => {
    if (this.isDownloadNotificationVisible) {
      return;
    }
    const fileName = generateFileNameWithTimestamp();
    const { desktopDirectoryPath } = this.stores.profile;
    const defaultPath = path.join(desktopDirectoryPath, fileName);
    const params = { defaultPath };
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

  @action _setIsDownloadingLogs = (isDownloadNotificationVisible: boolean) => {
    this.isDownloadNotificationVisible = isDownloadNotificationVisible;
  };
}
