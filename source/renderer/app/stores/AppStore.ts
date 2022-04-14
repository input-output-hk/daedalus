import { observable, computed, action, runInAction } from 'mobx';
import path from 'path';
import BigNumber from 'bignumber.js';
import { computedFn } from 'mobx-utils';
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
import { isReceiverAddressType } from '../utils/hardwareWalletUtils';
import {
  StakingParameters,
  TransactionParameters,
} from '../../../common/types/magic-links.types';

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
  // TODO: define all possible query params Daedalus can receive from external source
  @observable
  customProtocolParameters:
    | TransactionParameters
    | StakingParameters
    | undefined;

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
    getCustomProtocolChannel.onReceive(this._handleCustomProtocol);

    // ============== MOBX REACTIONS ==============
    this.registerReactions([this._processCustomProtocolParams]);
  }

  // Handle url query data and switch to proper route (react on specific case)
  _processCustomProtocolParams = () => {
    // Note: For now we are processing only BASIC tx
    const { appUpdate, wallets, networkStatus } = this.stores;
    const { displayAppUpdateOverlay } = appUpdate;
    const { hasLoadedWallets } = wallets;
    const {
      isConnected,
      isNotEnoughDiskSpace,
      isSystemTimeCorrect,
    } = networkStatus;

    // eslint-disable-next-line
    console.debug('>>> _processCustomProtocolParams: ', {
      isConnected,
      hasLoadedWallets,
      isNotEnoughDiskSpace,
      isSystemTimeCorrect,
      displayAppUpdateOverlay,
      customProtocolParameters: this.customProtocolParameters,
    });

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

      case DIALOGS.TOGGLE_RTS_FLAGS_MODE:
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        this._updateActiveDialog(DIALOGS.TOGGLE_RTS_FLAGS_MODE);

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

  /**
   *  Cardano custom protocol specification (Grammar)
   *  https://github.com/cardano-foundation/CIPs/blob/master/CIP-0013/CIP-0013.md
   *
   *  1. cardanourn = "web+cardano:" (paymentref | stakepoolref)
   *
   *  paymentref = cardanoaddress [ "?" amountparam ]
   *  cardanoaddress = *(base58 | bech32)
   *  amountparam = "amount=" *digit [ "." *digit ]
   *  Example:
   *  web+cardano:addr_test1qrakn4h9wkq96rx2eyfhn7mydpf285pg80dma203sr3qmulqqapc66zkutqnm0zk55gvs2drll6nune7eh24ax7ld3yqy53sf0?amount=5.000000
   *
   *
   *  2. stakepoolref = "//stake?" stakepool
   *
   *  stakepool = poolhexid | poolticker
   *  poolhexid = 56HEXDIG
   *  poolticker = 3*5UNICODE
   *  Example:
   *  web+cardano://stake?c94e6fe1123bf111b77b57994bcd836af8ba2b3aa72cfcefbec2d3d4
   *  web+cardano://stake?COSD
   */
  @action _handleCustomProtocol = async (url: string): Promise<void> => {
    try {
      const address = new URL(url);
      const actionParam = address.pathname?.replace('//', ''); // can be `stake` or an address
      const params = new URLSearchParams(address.search);
      const amountParam: BigNumber = new BigNumber(params.get('amount'));

      const isAddress = computedFn(
        async (input: string): Promise<boolean> => {
          const response = await introspectAddressChannel.send({
            input,
          });
          return !(
            response === 'Invalid' ||
            !isReceiverAddressType(response.introspection.address_type)
          );
        }
      );
      if ((await isAddress(actionParam)) && amountParam.isPositive()) {
        // For now (MVP) we are handling just basic tx
        // e.g. web+cardano://addr_test1qrakn4h9wkq96rx2eyfhn7mydpf285pg80dma203sr3qmulqqapc66zkutqnm0zk55gvs2drll6nune7eh24ax7ld3yqy53sf0?amount=5.000000

        if (this.stores.wallets.hasAnyWallets) {
          runInAction('Store custom protocol data', () => {
            this.customProtocolParameters = {
              action: 'transaction',
              address: actionParam,
              amount: amountParam,
            };
          });
        }
      }
    } catch (e) {
      console.error(e);
    }

    return Promise.resolve();
  };

  @action resetCustomProptocolParams = () => {
    this.customProtocolParameters = undefined;
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
