import * as ipcApi from '../../common/ipc/api';

export type PrivilegedIpcDirection =
  | 'renderer-to-main'
  | 'main-to-renderer'
  | 'bidirectional';

export type PrivilegedIpcManifestEntry = {
  contract: keyof typeof ipcApi;
  channel: string;
  transport: 'channel' | 'conversation';
  direction: PrivilegedIpcDirection;
  constructorOwner: string;
  rendererOwner: string;
  registrationOwner: string | null;
  receive: 'request' | 'broadcast' | 'conversation' | 'none';
  callerOwners: string[];
  callerCount: number;
  capability: string;
  settlement:
    | 'handler-response'
    | 'awaited'
    | 'fire-and-forget-owned'
    | 'mixed';
  authority: 'exact-active-trusted-main-frame';
};

type EntryDefaults = Pick<
  PrivilegedIpcManifestEntry,
  'constructorOwner' | 'rendererOwner' | 'registrationOwner' | 'capability'
> &
  Partial<
    Pick<
      PrivilegedIpcManifestEntry,
      | 'transport'
      | 'direction'
      | 'callerOwners'
      | 'callerCount'
      | 'settlement'
      | 'receive'
    >
  >;

const entries = (
  contracts: Array<keyof typeof ipcApi>,
  defaults: EntryDefaults
): PrivilegedIpcManifestEntry[] =>
  contracts.map((contract) => ({
    contract,
    channel: ipcApi[contract] as string,
    transport: defaults.transport || 'channel',
    direction: defaults.direction || 'renderer-to-main',
    constructorOwner: defaults.constructorOwner,
    rendererOwner: defaults.rendererOwner,
    registrationOwner: defaults.registrationOwner,
    receive:
      defaults.receive ||
      (defaults.transport === 'conversation' ? 'conversation' : 'request'),
    callerOwners: defaults.callerOwners || [],
    callerCount: defaults.callerCount || 0,
    capability: defaults.capability,
    settlement: defaults.settlement || 'handler-response',
    authority: 'exact-active-trusted-main-frame',
  }));

export const privilegedIpcManifest: PrivilegedIpcManifestEntry[] = [
  ...entries(['GET_LOGS_CHANNEL'], {
    constructorOwner: 'source/main/ipc/get-logs.ts',
    rendererOwner: 'source/renderer/app/ipc/logs.ipc.ts',
    registrationOwner: 'source/main/ipc/get-logs.ts',
    capability: 'logs',
  }),
  ...entries(['COMPRESS_LOGS_CHANNEL'], {
    constructorOwner: 'source/main/ipc/compress-logs.ts',
    rendererOwner: 'source/renderer/app/ipc/logs.ipc.ts',
    registrationOwner: 'source/main/ipc/compress-logs.ts',
    capability: 'logs',
  }),
  ...entries(['DOWNLOAD_LOGS_CHANNEL'], {
    constructorOwner: 'source/main/ipc/download-logs.ts',
    rendererOwner: 'source/renderer/app/ipc/logs.ipc.ts',
    registrationOwner: 'source/main/ipc/download-logs.ts',
    capability: 'logs-filesystem',
  }),
  ...entries(['GET_GPU_STATUS_CHANNEL'], {
    constructorOwner: 'source/main/ipc/get-gpu-status.ts',
    rendererOwner: 'source/renderer/app/ipc/get-gpu-status.ipc.ts',
    registrationOwner: 'source/main/ipc/get-gpu-status.ts',
    capability: 'system',
  }),
  ...entries(['CLOSE_WINDOW_CHANNEL', 'RESIZE_WINDOW_CHANNEL'], {
    constructorOwner: 'source/main/ipc/windowControlChannels.ts',
    rendererOwner: 'source/renderer/app/ipc/windowControlChannels.ts',
    registrationOwner: 'source/main/ipc/windowControlChannels.ts',
    capability: 'window',
    receive: 'broadcast',
  }),
  ...entries(
    [
      'DAPP_BROWSER_OPEN_CHANNEL',
      'DAPP_BROWSER_CLOSE_CHANNEL',
      'DAPP_BROWSER_STATUS_CHANNEL',
    ],
    {
      constructorOwner: 'source/main/ipc/dappBrowser.ts',
      rendererOwner: 'source/renderer/app/ipc/dappBrowser.ts',
      registrationOwner: 'source/main/ipc/dappBrowser.ts',
      capability: 'dapp-browser',
    }
  ),
  ...entries(['DAPP_CONNECTIONS_CHANNEL'], {
    constructorOwner: 'source/main/ipc/dappConnections.ts',
    rendererOwner: 'source/renderer/app/ipc/dappConnections.ts',
    registrationOwner: 'source/main/ipc/dappConnections.ts',
    capability: 'dapp-connections',
  }),
  ...entries(['DAPP_COLLATERAL_CHANNEL'], {
    constructorOwner: 'source/main/ipc/collateral.ts',
    rendererOwner: 'source/renderer/app/ipc/collateral.ts',
    registrationOwner: 'source/main/ipc/collateral.ts',
    capability: 'dapp-collateral',
  }),
  ...entries(['DAPP_BROWSER_STATE_CHANNEL'], {
    constructorOwner: 'source/main/ipc/dappBrowser.ts',
    rendererOwner: 'source/renderer/app/ipc/dappBrowser.ts',
    registrationOwner: null,
    capability: 'dapp-browser',
    direction: 'main-to-renderer',
    receive: 'none',
    settlement: 'fire-and-forget-owned',
    callerOwners: ['source/main/ipc/dappBrowser.ts'],
    callerCount: 1,
  }),
  ...entries(['DAPP_CONSENT_RENDER_CHANNEL'], {
    constructorOwner: 'source/main/ipc/dappConsent.ts',
    rendererOwner: 'source/renderer/app/ipc/dappConsent.ts',
    registrationOwner: null,
    capability: 'dapp-consent',
    direction: 'main-to-renderer',
    receive: 'none',
    settlement: 'awaited',
    callerOwners: ['source/main/ipc/dappConsent.ts'],
    callerCount: 2,
  }),
  ...entries(['DAPP_CIP30_WALLET_CHANNEL'], {
    constructorOwner: 'source/main/ipc/cip30Wallet.ts',
    rendererOwner: 'source/renderer/app/ipc/cip30Wallet.ts',
    registrationOwner: null,
    capability: 'dapp-wallet-executor',
    direction: 'main-to-renderer',
    receive: 'none',
    settlement: 'awaited',
    callerOwners: ['source/main/ipc/cip30Wallet.ts'],
    callerCount: 1,
  }),
  ...entries(['SHOW_UI_PART_CHANNEL'], {
    constructorOwner: 'source/main/ipc/control-ui-parts.ts',
    rendererOwner: 'source/renderer/app/ipc/control-ui-parts.ts',
    registrationOwner: null,
    callerOwners: [
      'source/main/utils/buildAppMenus.ts',
      'source/main/menus/osx.ts',
      'source/main/menus/win-linux.ts',
    ],
    capability: 'ui-notification',
    direction: 'main-to-renderer',
    settlement: 'fire-and-forget-owned',
    receive: 'none',
    callerCount: 3,
  }),
  ...entries(['TOGGLE_UI_PART_CHANNEL'], {
    constructorOwner: 'source/main/ipc/control-ui-parts.ts',
    rendererOwner: 'source/renderer/app/ipc/control-ui-parts.ts',
    registrationOwner: null,
    capability: 'ui-notification',
    direction: 'main-to-renderer',
    settlement: 'fire-and-forget-owned',
    receive: 'none',
  }),
  ...entries(['GET_DISK_SPACE_STATUS_CHANNEL'], {
    constructorOwner: 'source/main/ipc/get-disk-space-status.ts',
    rendererOwner: 'source/renderer/app/ipc/getDiskSpaceChannel.ts',
    registrationOwner: 'source/main/utils/handleDiskSpace.ts',
    callerOwners: ['source/main/utils/handleDiskSpace.ts'],
    capability: 'system-storage',
    direction: 'bidirectional',
    settlement: 'awaited',
    receive: 'broadcast',
    callerCount: 2,
  }),
  ...entries(['GET_STATE_DIRECTORY_PATH_CHANNEL'], {
    constructorOwner: 'source/main/ipc/getStateDirectoryPathChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/getStateDirectoryPathChannel.ts',
    registrationOwner: 'source/main/index.ts',
    capability: 'filesystem',
  }),
  ...entries(['GET_DESKTOP_DIRECTORY_PATH_CHANNEL'], {
    constructorOwner: 'source/main/ipc/getDesktopDirectoryPathChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/getDesktopDirectoryPathChannel.ts',
    registrationOwner: 'source/main/index.ts',
    capability: 'filesystem',
  }),
  ...entries(['GET_SYSTEM_LOCALE_CHANNEL'], {
    constructorOwner: 'source/main/ipc/getSystemLocaleChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/getSystemLocaleChannel.ts',
    registrationOwner: 'source/main/index.ts',
    capability: 'system',
  }),
  ...entries(['SET_STATE_SNAPSHOT_LOG_CHANNEL'], {
    constructorOwner: 'source/main/ipc/set-log-state-snapshot.ts',
    rendererOwner: 'source/renderer/app/ipc/setStateSnapshotLogChannel.ts',
    registrationOwner: 'source/main/index.ts',
    capability: 'logs',
    receive: 'broadcast',
  }),
  ...entries(['LOAD_ASSET_CHANNEL'], {
    constructorOwner: 'source/main/ipc/load-asset.ts',
    rendererOwner: 'source/renderer/app/ipc/loadAsset.ts',
    registrationOwner: 'source/main/ipc/load-asset.ts',
    capability: 'filesystem',
  }),
  ...entries(['OPEN_EXTERNAL_URL_CHANNEL'], {
    constructorOwner: 'source/main/ipc/open-external-url.ts',
    rendererOwner: 'source/renderer/app/ipc/open-external-url.ts',
    registrationOwner: 'source/main/ipc/open-external-url.ts',
    capability: 'shell',
    receive: 'broadcast',
  }),
  ...entries(['OPEN_LOCAL_DIRECTORY_CHANNEL'], {
    constructorOwner: 'source/main/ipc/open-local-directory.ts',
    rendererOwner: 'source/renderer/app/ipc/open-local-directory.ts',
    registrationOwner: 'source/main/ipc/open-local-directory.ts',
    capability: 'shell-filesystem',
    receive: 'broadcast',
  }),
  ...entries(['SUBMIT_BUG_REPORT_REQUEST_CHANNEL'], {
    constructorOwner: 'source/main/ipc/bugReportRequestChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/bugReportRequestChannel.ts',
    registrationOwner: 'source/main/ipc/bugReportRequestChannel.ts',
    capability: 'network',
    receive: 'broadcast',
  }),
  ...entries(['REBUILD_APP_MENU_CHANNEL'], {
    constructorOwner: 'source/main/ipc/rebuild-application-menu.ts',
    rendererOwner: 'source/renderer/app/ipc/rebuild-application-menu.ts',
    registrationOwner: 'source/main/index.ts',
    capability: 'menu',
    receive: 'broadcast',
  }),
  ...entries(['GENERATE_FILE_META_CHANNEL'], {
    constructorOwner: 'source/main/ipc/generateFileMetaChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/generateFileMetaChannel.ts',
    registrationOwner: 'source/main/ipc/generateFileMetaChannel.ts',
    capability: 'filesystem-export',
    receive: 'broadcast',
  }),
  ...entries(['GENERATE_PAPER_WALLET_CHANNEL'], {
    constructorOwner: 'source/main/ipc/generatePaperWalletChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/generatePaperWalletChannel.ts',
    registrationOwner: 'source/main/ipc/generatePaperWalletChannel.ts',
    capability: 'filesystem-export',
    receive: 'broadcast',
  }),
  ...entries(['GENERATE_ADDRESS_PDF_CHANNEL'], {
    constructorOwner: 'source/main/ipc/generateAddressPDFChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/generateAddressPDFChannel.ts',
    registrationOwner: 'source/main/ipc/generateAddressPDFChannel.ts',
    capability: 'filesystem-export',
    receive: 'broadcast',
  }),
  ...entries(['GENERATE_VOTING_PDF_CHANNEL'], {
    constructorOwner: 'source/main/ipc/generateVotingPDFChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/generateVotingPDFChannel.ts',
    registrationOwner: 'source/main/ipc/generateVotingPDFChannel.ts',
    capability: 'filesystem-export',
    receive: 'broadcast',
  }),
  ...entries(['GENERATE_CSV_CHANNEL'], {
    constructorOwner: 'source/main/ipc/generateCsvChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/generateCsvChannel.ts',
    registrationOwner: 'source/main/ipc/generateCsvChannel.ts',
    capability: 'filesystem-export',
    receive: 'broadcast',
  }),
  ...entries(['GENERATE_QRCODE_CHANNEL'], {
    constructorOwner: 'source/main/ipc/saveQRCodeImageChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/saveQRCodeImageChannel.ts',
    registrationOwner: 'source/main/ipc/saveQRCodeImageChannel.ts',
    capability: 'filesystem-export',
    receive: 'broadcast',
  }),
  ...entries(['GENERATE_WALLET_MIGRATION_REPORT_CHANNEL'], {
    constructorOwner: 'source/main/ipc/generateWalletMigrationReportChannel.ts',
    rendererOwner:
      'source/renderer/app/ipc/generateWalletMigrationReportChannel.ts',
    registrationOwner: 'source/main/index.ts',
    capability: 'filesystem-export',
    receive: 'broadcast',
  }),
  ...entries(['GET_WASM_BINARY_CHANNEL'], {
    constructorOwner: 'source/main/ipc/getRecoveryWalletIdChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/getRecoveryWalletIdChannel.ts',
    registrationOwner: 'source/main/ipc/getRecoveryWalletIdChannel.ts',
    capability: 'filesystem',
  }),
  ...entries(['SHOW_OPEN_DIALOG_CHANNEL', 'SHOW_SAVE_DIALOG_CHANNEL'], {
    constructorOwner: 'source/main/ipc/show-file-dialog-channels.ts',
    rendererOwner: 'source/renderer/app/ipc/show-file-dialog-channels.ts',
    registrationOwner: 'source/main/ipc/show-file-dialog-channels.ts',
    capability: 'filesystem-dialog',
    receive: 'broadcast',
  }),
  ...entries(['ELECTRON_STORE_CHANNEL'], {
    constructorOwner: 'source/main/ipc/electronStoreConversation.ts',
    rendererOwner: 'source/renderer/app/ipc/electronStoreConversation.ts',
    registrationOwner: 'source/main/ipc/electronStoreConversation.ts',
    capability: 'store',
    transport: 'conversation',
    receive: 'conversation',
  }),
  ...entries(['REQUEST_DOWNLOAD'], {
    constructorOwner: 'source/main/ipc/downloadManagerChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/downloadManagerChannel.ts',
    registrationOwner: 'source/main/ipc/downloadManagerChannel.ts',
    callerOwners: ['source/main/utils/downloadManager.ts'],
    capability: 'download-filesystem',
    direction: 'bidirectional',
    settlement: 'fire-and-forget-owned',
    callerCount: 6,
  }),
  ...entries(
    [
      'RESUME_DOWNLOAD',
      'DELETE_DOWNLOADED_FILE',
      'GET_DOWNLOAD_LOCAL_DATA',
      'GET_DOWNLOADS_LOCAL_DATA',
      'CLEAR_DOWNLOAD_LOCAL_DATA',
      'CHECK_FILE_EXISTS',
    ],
    {
      constructorOwner: 'source/main/ipc/downloadManagerChannel.ts',
      rendererOwner: 'source/renderer/app/ipc/downloadManagerChannel.ts',
      registrationOwner: 'source/main/ipc/downloadManagerChannel.ts',
      capability: 'download-filesystem',
    }
  ),
  ...entries(['GET_CACHED_BACKEND_STATUS_CHANNEL'], {
    constructorOwner: 'source/main/ipc/backendStatusChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/backendStatusChannel.ts',
    registrationOwner: 'source/main/ipc/index.ts',
    capability: 'cardano-state',
  }),
  ...entries(['MITHRIL_COMMAND_CHANNEL'], {
    constructorOwner: 'source/main/ipc/mithrilCommandChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/mithrilCommandChannel.ts',
    registrationOwner: 'source/main/ipc/index.ts',
    capability: 'mithril-lifecycle',
    receive: 'broadcast',
  }),
  ...entries(
    ['MITHRIL_PROGRESS_CHANNEL', 'MITHRIL_STATUS_CHANNEL', 'WALLET_PORT_CHANNEL'],
    {
      constructorOwner: 'source/main/ipc/mithrilPushChannel.ts',
      rendererOwner: 'source/renderer/app/ipc/mithrilPushChannel.ts',
      registrationOwner: null,
      callerOwners: ['source/main/BackendLifecycle.ts'],
      capability: 'cardano-state',
      direction: 'main-to-renderer',
      settlement: 'fire-and-forget-owned',
      receive: 'none',
      callerCount: 1,
    }
  ),
  ...entries(
    [
      'NODE_STARTUP_STATUS_CHANNEL',
      'NODE_BLOCK_SYNC_PROGRESS_CHANNEL',
      'WATCHDOG_STOPPED_CHANNEL',
    ],
    {
      constructorOwner: 'source/main/ipc/nodePushChannel.ts',
      rendererOwner: 'source/renderer/app/ipc/nodePushChannel.ts',
      registrationOwner: null,
      callerOwners: ['source/main/BackendLifecycle.ts'],
      capability: 'cardano-state',
      direction: 'main-to-renderer',
      settlement: 'fire-and-forget-owned',
      receive: 'none',
      callerCount: 1,
    }
  ),
  ...entries(
    ['VALIDATE_CHAIN_STORAGE_CHANNEL', 'CONFIRM_CHAIN_STORAGE_CHANNEL'],
    {
      constructorOwner: 'source/main/ipc/chainStorageChannel.ts',
      rendererOwner: 'source/renderer/app/ipc/chainStorageChannel.ts',
      registrationOwner: 'source/main/ipc/chainStorageChannel.ts',
      capability: 'filesystem-storage',
    }
  ),
  ...entries(['GOVERNANCE_DREP_ANCHOR_CHANNEL'], {
    constructorOwner: 'source/main/ipc/governanceAnchorChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/governanceChannel.ts',
    registrationOwner: 'source/main/ipc/governanceAnchorChannel.ts',
    capability: 'network',
  }),
  ...entries(['MANAGE_APP_UPDATE'], {
    constructorOwner: 'source/main/ipc/manageAppUpdateChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/manageAppUpdateChannel.ts',
    registrationOwner: 'source/main/ipc/manageAppUpdateChannel.ts',
    callerOwners: ['source/main/ipc/manageAppUpdateChannel.ts'],
    capability: 'update-filesystem',
    direction: 'bidirectional',
    settlement: 'fire-and-forget-owned',
    callerCount: 1,
  }),
  ...entries(['INTROSPECT_ADDRESS_CHANNEL'], {
    constructorOwner: 'source/main/ipc/introspect-address.ts',
    rendererOwner: 'source/renderer/app/ipc/introspect-address.ts',
    registrationOwner: 'source/main/ipc/introspect-address.ts',
    capability: 'cardano-data',
    receive: 'broadcast',
  }),
  ...entries(
    [
      'GET_HARDWARE_WALLET_TRANSPORT_CHANNEL',
      'GET_EXTENDED_PUBLIC_KEY_CHANNEL',
      'GET_CARDANO_ADA_APP_CHANNEL',
      'SIGN_TRANSACTION_LEDGER_CHANNEL',
      'SIGN_TRANSACTION_TREZOR_CHANNEL',
      'SIGN_EXACT_HARDWARE_TRANSACTION_CHANNEL',
      'SIGN_EXACT_HARDWARE_MESSAGE_CHANNEL',
      'GET_INIT_TREZOR_CONNECT_CHANNEL',
      'GET_INIT_LEDGER_CONNECT_CHANNEL',
      'DERIVE_XPUB_CHANNEL',
      'RESET_ACTION_TREZOR_CHANNEL',
      'DERIVE_ADDRESS_CHANNEL',
      'SHOW_ADDRESS_CHANNEL',
      'WAIT_FOR_LEDGER_DEVICES',
    ],
    {
      constructorOwner: 'source/main/ipc/createHardwareWalletIPCChannels.ts',
      rendererOwner: 'source/renderer/app/ipc/getHardwareWalletChannel.ts',
      registrationOwner: 'source/main/hardware/HardwareWalletService.ts',
      capability: 'hardware-wallet',
    }
  ),
  ...entries(['GET_HARDWARE_WALLET_CONNECTION_CHANNEL'], {
    constructorOwner: 'source/main/ipc/createHardwareWalletIPCChannels.ts',
    rendererOwner: 'source/renderer/app/ipc/getHardwareWalletChannel.ts',
    registrationOwner: null,
    callerOwners: ['source/main/hardware/HardwareWalletService.ts'],
    capability: 'hardware-wallet',
    direction: 'main-to-renderer',
    settlement: 'fire-and-forget-owned',
    receive: 'none',
    callerCount: 3,
  }),
  ...entries(['TOGGLE_RTS_FLAGS_MODE_CHANNEL'], {
    constructorOwner: 'source/main/ipc/toggleRTSFlagsModeChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/toggleRTSFlagsModeChannel.ts',
    registrationOwner: 'source/main/index.ts',
    capability: 'system',
    receive: 'broadcast',
  }),
];
