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
  ...entries(
    [
      'CARDANO_AWAIT_UPDATE_CHANNEL',
      'CARDANO_RESTART_CHANNEL',
      'CARDANO_FAULT_INJECTION_CHANNEL',
      'SET_CACHED_CARDANO_STATUS_CHANNEL',
    ],
    {
      constructorOwner: 'source/main/ipc/cardano.ipc.ts',
      rendererOwner: 'source/renderer/app/ipc/cardano.ipc.ts',
      registrationOwner: 'source/main/cardano/setup.ts',
      capability: 'cardano-lifecycle',
      receive: 'broadcast',
    }
  ),
  ...entries(['GET_CACHED_CARDANO_STATUS_CHANNEL', 'EXPORT_WALLETS_CHANNEL'], {
    constructorOwner: 'source/main/ipc/cardano.ipc.ts',
    rendererOwner: 'source/renderer/app/ipc/cardano.ipc.ts',
    registrationOwner: 'source/main/cardano/setup.ts',
    capability: 'cardano-lifecycle',
  }),
  ...entries(['CARDANO_STATE_CHANNEL', 'CARDANO_TLS_CONFIG_CHANNEL'], {
    constructorOwner: 'source/main/ipc/cardano.ipc.ts',
    rendererOwner: 'source/renderer/app/ipc/cardano.ipc.ts',
    registrationOwner: 'source/main/cardano/setup.ts',
    callerOwners: ['source/main/cardano/setup.ts'],
    capability: 'cardano-state',
    direction: 'bidirectional',
    settlement: 'fire-and-forget-owned',
    callerCount: 1,
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
  ...entries(
    [
      'MITHRIL_BOOTSTRAP_DECISION_CHANNEL',
      'MITHRIL_BOOTSTRAP_START_CHANNEL',
      'MITHRIL_BOOTSTRAP_CANCEL_CHANNEL',
      'MITHRIL_BOOTSTRAP_SNAPSHOTS_CHANNEL',
    ],
    {
      constructorOwner: 'source/main/ipc/mithrilBootstrapChannel.ts',
      rendererOwner: 'source/renderer/app/ipc/mithrilBootstrapChannel.ts',
      registrationOwner: 'source/main/ipc/mithrilBootstrapChannel.ts',
      capability: 'mithril-storage',
    }
  ),
  ...entries(['MITHRIL_BOOTSTRAP_STATUS_CHANNEL'], {
    constructorOwner: 'source/main/ipc/mithrilBootstrapChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/mithrilBootstrapChannel.ts',
    registrationOwner: 'source/main/ipc/mithrilBootstrapChannel.ts',
    callerOwners: ['source/main/ipc/mithrilBootstrapChannel.ts'],
    capability: 'mithril-storage',
    direction: 'bidirectional',
    settlement: 'awaited',
    callerCount: 1,
  }),
  ...entries(
    [
      'MITHRIL_PARTIAL_SYNC_START_CHANNEL',
      'MITHRIL_PARTIAL_SYNC_CANCEL_CHANNEL',
      'MITHRIL_PARTIAL_SYNC_RESTART_NORMAL_CHANNEL',
      'MITHRIL_PARTIAL_SYNC_WIPE_AND_FULL_SYNC_CHANNEL',
      'MITHRIL_PARTIAL_SYNC_AVAILABILITY_CHANNEL',
      'MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL',
    ],
    {
      constructorOwner: 'source/main/ipc/mithrilPartialSyncChannel.ts',
      rendererOwner: 'source/renderer/app/ipc/mithrilPartialSyncChannel.ts',
      registrationOwner: 'source/main/ipc/mithrilPartialSyncChannel.ts',
      capability: 'mithril-storage',
    }
  ),
  ...entries(['MITHRIL_PARTIAL_SYNC_STATUS_CHANNEL'], {
    constructorOwner: 'source/main/ipc/mithrilPartialSyncChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/mithrilPartialSyncChannel.ts',
    registrationOwner: 'source/main/ipc/mithrilPartialSyncChannel.ts',
    callerOwners: ['source/main/ipc/mithrilPartialSyncChannel.ts'],
    capability: 'mithril-storage',
    direction: 'bidirectional',
    settlement: 'awaited',
    callerCount: 1,
  }),
  ...entries(
    [
      'SET_CHAIN_STORAGE_DIRECTORY_CHANNEL',
      'GET_CHAIN_STORAGE_DIRECTORY_CHANNEL',
      'VALIDATE_CHAIN_STORAGE_DIRECTORY_CHANNEL',
      'PREPARE_CHAIN_STORAGE_LOCATION_CHANGE_CHANNEL',
    ],
    {
      constructorOwner: 'source/main/ipc/chainStorageChannel.ts',
      rendererOwner: 'source/renderer/app/ipc/chainStorageChannel.ts',
      registrationOwner: 'source/main/ipc/chainStorageChannel.ts',
      capability: 'filesystem-storage',
    }
  ),
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
  ...entries(['GET_BLOCK_SYNC_PROGRESS_CHANNEL'], {
    constructorOwner: 'source/main/ipc/get-block-sync-progress.ts',
    rendererOwner: 'source/renderer/app/ipc/getBlockSyncChannel.ts',
    registrationOwner: null,
    callerOwners: ['source/main/utils/handleCheckBlockReplayProgress.ts'],
    capability: 'cardano-state',
    direction: 'main-to-renderer',
    settlement: 'fire-and-forget-owned',
    receive: 'none',
    callerCount: 1,
  }),
  ...entries(
    [
      'GET_HARDWARE_WALLET_TRANSPORT_CHANNEL',
      'GET_EXTENDED_PUBLIC_KEY_CHANNEL',
      'GET_CARDANO_ADA_APP_CHANNEL',
      'SIGN_TRANSACTION_LEDGER_CHANNEL',
      'SIGN_TRANSACTION_TREZOR_CHANNEL',
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
      registrationOwner: 'source/main/ipc/getHardwareWalletChannel.ts',
      capability: 'hardware-wallet',
    }
  ),
  ...entries(['GET_HARDWARE_WALLET_CONNECTION_CHANNEL'], {
    constructorOwner: 'source/main/ipc/createHardwareWalletIPCChannels.ts',
    rendererOwner: 'source/renderer/app/ipc/getHardwareWalletChannel.ts',
    registrationOwner: null,
    callerOwners: ['source/main/ipc/getHardwareWalletChannel.ts'],
    capability: 'hardware-wallet',
    direction: 'main-to-renderer',
    settlement: 'fire-and-forget-owned',
    receive: 'none',
    callerCount: 4,
  }),
  ...entries(['TOGGLE_RTS_FLAGS_MODE_CHANNEL'], {
    constructorOwner: 'source/main/ipc/toggleRTSFlagsModeChannel.ts',
    rendererOwner: 'source/renderer/app/ipc/toggleRTSFlagsModeChannel.ts',
    registrationOwner: 'source/main/index.ts',
    capability: 'system',
    receive: 'broadcast',
  }),
];
