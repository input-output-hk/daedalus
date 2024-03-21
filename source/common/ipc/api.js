'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.GET_HARDWARE_WALLET_CONNECTION_CHANNEL = exports.GET_CARDANO_ADA_APP_CHANNEL = exports.GET_EXTENDED_PUBLIC_KEY_CHANNEL = exports.GET_HARDWARE_WALLET_TRANSPORT_CHANNEL = exports.GET_BLOCK_SYNC_PROGRESS_CHANNEL = exports.INTROSPECT_ADDRESS_CHANNEL = exports.MANAGE_APP_UPDATE = exports.CHECK_FILE_EXISTS = exports.CLEAR_DOWNLOAD_LOCAL_DATA = exports.GET_DOWNLOADS_LOCAL_DATA = exports.GET_DOWNLOAD_LOCAL_DATA = exports.DELETE_DOWNLOADED_FILE = exports.RESUME_DOWNLOAD = exports.REQUEST_DOWNLOAD = exports.ELECTRON_STORE_CHANNEL = exports.SHOW_SAVE_DIALOG_CHANNEL = exports.SHOW_OPEN_DIALOG_CHANNEL = exports.GET_WASM_BINARY_CHANNEL = exports.GENERATE_WALLET_MIGRATION_REPORT_CHANNEL = exports.EXPORT_WALLETS_CHANNEL = exports.SET_CACHED_CARDANO_STATUS_CHANNEL = exports.GET_CACHED_CARDANO_STATUS_CHANNEL = exports.CARDANO_FAULT_INJECTION_CHANNEL = exports.CARDANO_RESTART_CHANNEL = exports.CARDANO_TLS_CONFIG_CHANNEL = exports.CARDANO_STATE_CHANNEL = exports.CARDANO_AWAIT_UPDATE_CHANNEL = exports.GENERATE_QRCODE_CHANNEL = exports.GENERATE_CSV_CHANNEL = exports.GENERATE_VOTING_PDF_CHANNEL = exports.GENERATE_ADDRESS_PDF_CHANNEL = exports.GENERATE_PAPER_WALLET_CHANNEL = exports.GENERATE_FILE_META_CHANNEL = exports.REBUILD_APP_MENU_CHANNEL = exports.WalletSettingsStateEnum = exports.SUBMIT_BUG_REPORT_REQUEST_CHANNEL = exports.OPEN_LOCAL_DIRECTORY_CHANNEL = exports.OPEN_EXTERNAL_URL_CHANNEL = exports.LOAD_ASSET_CHANNEL = exports.SET_STATE_SNAPSHOT_LOG_CHANNEL = exports.GET_SYSTEM_LOCALE_CHANNEL = exports.GET_DESKTOP_DIRECTORY_PATH_CHANNEL = exports.GET_STATE_DIRECTORY_PATH_CHANNEL = exports.GET_DISK_SPACE_STATUS_CHANNEL = exports.TOGGLE_UI_PART_CHANNEL = exports.SHOW_UI_PART_CHANNEL = exports.GET_GPU_STATUS_CHANNEL = exports.DOWNLOAD_LOGS_CHANNEL = exports.COMPRESS_LOGS_CHANNEL = exports.GET_LOGS_CHANNEL = void 0;
exports.WAIT_FOR_LEDGER_DEVICES = exports.DEVICE_NOT_CONNECTED = exports.TOGGLE_RTS_FLAGS_MODE_CHANNEL = exports.SHOW_ADDRESS_CHANNEL = exports.DERIVE_ADDRESS_CHANNEL = exports.RESET_ACTION_TREZOR_CHANNEL = exports.DERIVE_XPUB_CHANNEL = exports.GET_INIT_LEDGER_CONNECT_CHANNEL = exports.GET_INIT_TREZOR_CONNECT_CHANNEL = exports.SIGN_TRANSACTION_TREZOR_CHANNEL = exports.SIGN_TRANSACTION_LEDGER_CHANNEL = void 0;
/**
 * ======================= IPC CHANNELS API =========================
 * This is the ipc-api contract between main and renderer process
 * which defines channel names and their possible message types.
 * Complex types are referenced from common/types to keep this api readable.
 * ==================================================================
 */
exports.GET_LOGS_CHANNEL = 'GET_LOGS_CHANNEL';
exports.COMPRESS_LOGS_CHANNEL = 'COMPRESS_LOGS_CHANNEL';
exports.DOWNLOAD_LOGS_CHANNEL = 'DOWNLOAD_LOGS_CHANNEL';
exports.GET_GPU_STATUS_CHANNEL = 'GET_GPU_STATUS_CHANNEL';
/**
 * Channel for showing ui parts specified via constants
 */
exports.SHOW_UI_PART_CHANNEL = 'SHOW_UI_PART_CHANNEL';
/**
 * Channel for toggling ui parts specified via constants
 */
exports.TOGGLE_UI_PART_CHANNEL = 'TOGGLE_UI_PART_CHANNEL';
/**
 * Channel for checking the disk space available
 */
exports.GET_DISK_SPACE_STATUS_CHANNEL = 'GetDiskSpaceStatusChannel';
/**
 * Channel for checking the state directory path
 */
exports.GET_STATE_DIRECTORY_PATH_CHANNEL = 'GetStateDirectoryPathChannel';
/**
 * Channel for checking the desktop directory path
 */
exports.GET_DESKTOP_DIRECTORY_PATH_CHANNEL = 'GetDesktopDirectoryPathChannel';
/**
 * Channel for checking the system locale
 */
exports.GET_SYSTEM_LOCALE_CHANNEL = 'GetSystemLocaleChannel';
/**
 * Channel for setting log state snapshot
 */
exports.SET_STATE_SNAPSHOT_LOG_CHANNEL = 'SetStateSnapshotLogChannel';
/**
 * Channel for loading a base64 encoded asset from within the `source/renderer` folder
 */
exports.LOAD_ASSET_CHANNEL = 'LoadAssetChannel';
/**
 * Channel for opening an external url in the default browser
 */
exports.OPEN_EXTERNAL_URL_CHANNEL = 'OPEN_EXTERNAL_URL_CHANNEL';
/**
 * Channel for opening a local directory in the default desktop explorer
 */
exports.OPEN_LOCAL_DIRECTORY_CHANNEL = 'OpenLocalDirectoryChannel';
/**
 * Channel to send bug report requests
 */
exports.SUBMIT_BUG_REPORT_REQUEST_CHANNEL = 'SUBMIT_BUG_REPORT_REQUEST_CHANNEL';
/**
 * Channel to rebuild the electron application menu after the language setting changes
 */
var WalletSettingsStateEnum;
(function (WalletSettingsStateEnum) {
  WalletSettingsStateEnum['hidden'] = 'hidden';
  WalletSettingsStateEnum['disabled'] = 'disabled';
  WalletSettingsStateEnum['enabled'] = 'enabled';
})(
  (WalletSettingsStateEnum =
    exports.WalletSettingsStateEnum || (exports.WalletSettingsStateEnum = {}))
);
exports.REBUILD_APP_MENU_CHANNEL = 'REBUILD_APP_MENU_CHANNEL';
/**
 * Channel to generate file blob
 */
exports.GENERATE_FILE_META_CHANNEL = 'GENERATE_FILE_META_CHANNEL';
/**
 * Channel to generate and save a paper wallet certificate
 */
exports.GENERATE_PAPER_WALLET_CHANNEL = 'GENERATE_PAPER_WALLET_CHANNEL';
/**
 * Channel to generate and save a share address PDF
 */
exports.GENERATE_ADDRESS_PDF_CHANNEL = 'GENERATE_ADDRESS_PDF_CHANNEL';
/**
 * Channel to generate and save a share voting PDF
 */
exports.GENERATE_VOTING_PDF_CHANNEL = 'GENERATE_VOTING_PDF_CHANNEL';
/**
 * Channel to generate and save a csv file
 */
exports.GENERATE_CSV_CHANNEL = 'GENERATE_CSV_CHANNEL';
/**
 * Channel to generate and save a QR code
 */
exports.GENERATE_QRCODE_CHANNEL = 'GENERATE_QRCODE_CHANNEL';
/**
 * ====================== CARDANO IPC CHANNELS ======================
 * This is the ipc-api contract between main & renderer process
 * to communicate with the cardano-node manager code.
 * ==================================================================
 */
/**
 * Channel to indicate that cardano-node will exit for updating
 */
exports.CARDANO_AWAIT_UPDATE_CHANNEL = 'CARDANO_AWAIT_UPDATE_CHANNEL';
/**
 * Channel where main process tells the renderer about cardano-node state updates
 */
exports.CARDANO_STATE_CHANNEL = 'CARDANO_STATE_CHANNEL';
/**
 * Channel to exchange tls config between main and renderer process
 */
exports.CARDANO_TLS_CONFIG_CHANNEL = 'CARDANO_TLS_CONFIG_CHANNEL';
/**
 * Channel where renderer can request a cardano-node restart
 */
exports.CARDANO_RESTART_CHANNEL = 'CARDANO_RESTART_CHANNEL';
/**
 * Channel where render process can toggle cardano-node fault injections
 */
exports.CARDANO_FAULT_INJECTION_CHANNEL = 'CARDANO_FAULT_INJECTION_CHANNEL';
/**
 * Channel where renderer can ask for the last cached cardano-node status
 */
exports.GET_CACHED_CARDANO_STATUS_CHANNEL = 'GET_CACHED_CARDANO_STATUS_CHANNEL';
/**
 * Channel where renderer and main process can exchange cardano-node status info
 */
exports.SET_CACHED_CARDANO_STATUS_CHANNEL = 'SET_CACHED_CARDANO_STATUS_CHANNEL';
/**
 * Channel where renderer can ask main process to export wallets
 */
exports.EXPORT_WALLETS_CHANNEL = 'EXPORT_WALLETS_CHANNEL';
/**
 * Channel for generating wallet migration report
 */
exports.GENERATE_WALLET_MIGRATION_REPORT_CHANNEL =
  'GENERATE_WALLET_MIGRATION_REPORT_CHANNEL';
/**
 * Channel for generating wallet migration report
 */
exports.GET_WASM_BINARY_CHANNEL = 'GET_WASM_BINARY_CHANNEL';
/**
 * Channel for showing open dialog
 */
exports.SHOW_OPEN_DIALOG_CHANNEL = 'SHOW_OPEN_DIALOG_CHANNEL';
/**
 * Channel for showing save dialog
 */
exports.SHOW_SAVE_DIALOG_CHANNEL = 'SHOW_SAVE_DIALOG_CHANNEL';
/**
 * Channel for electron-store
 */
exports.ELECTRON_STORE_CHANNEL = 'ELECTRON_STORE_CHANNEL';
/**
 * Channel for requesting a new download
 */
exports.REQUEST_DOWNLOAD = 'REQUEST_DOWNLOAD';
/**
 * Channel for resuming an existing download
 */
exports.RESUME_DOWNLOAD = 'RESUME_DOWNLOAD';
/**
 * Channel for resuming an existing download
 */
exports.DELETE_DOWNLOADED_FILE = 'DELETE_DOWNLOADED_FILE';
/**
 * Channel for initiating the download manager
 */
exports.GET_DOWNLOAD_LOCAL_DATA = 'GET_DOWNLOAD_LOCAL_DATA';
/**
 * Channel for initiating the download manager
 */
exports.GET_DOWNLOADS_LOCAL_DATA = 'GET_DOWNLOADS_LOCAL_DATA';
/**
 * Channel for initiating the download manager
 */
exports.CLEAR_DOWNLOAD_LOCAL_DATA = 'CLEAR_DOWNLOAD_LOCAL_DATA';
/**
 * Channel for checking if the downloaded file still exists
 */
exports.CHECK_FILE_EXISTS = 'CHECK_FILE_EXISTS';
/**
 * Channel for quitting Daedalus and installing update
 */
exports.MANAGE_APP_UPDATE = 'MANAGE_APP_UPDATE';
/**
 * Channel for introspecting an address
 */
exports.INTROSPECT_ADDRESS_CHANNEL = 'INTROSPECT_ADDRESS_CHANNEL';
/**
 * Channel for checking block replay progress
 */
exports.GET_BLOCK_SYNC_PROGRESS_CHANNEL = 'GetBlockSyncProgressChannel';
/**
 * Channels for connecting / interacting with Hardware Wallet devices
 */
exports.GET_HARDWARE_WALLET_TRANSPORT_CHANNEL =
  'GET_HARDWARE_WALLET_TRANSPORT_CHANNEL';
exports.GET_EXTENDED_PUBLIC_KEY_CHANNEL = 'GET_EXTENDED_PUBLIC_KEY_CHANNEL';
exports.GET_CARDANO_ADA_APP_CHANNEL = 'GET_CARDANO_ADA_APP_CHANNEL';
exports.GET_HARDWARE_WALLET_CONNECTION_CHANNEL =
  'GET_HARDWARE_WALLET_CONNECTION_CHANNEL';
exports.SIGN_TRANSACTION_LEDGER_CHANNEL = 'SIGN_TRANSACTION_LEDGER_CHANNEL';
exports.SIGN_TRANSACTION_TREZOR_CHANNEL = 'SIGN_TRANSACTION_TREZOR_CHANNEL';
exports.GET_INIT_TREZOR_CONNECT_CHANNEL = 'GET_INIT_TREZOR_CONNECT_CHANNEL';
exports.GET_INIT_LEDGER_CONNECT_CHANNEL = 'GET_INIT_LEDGER_CONNECT_CHANNEL';
exports.DERIVE_XPUB_CHANNEL = 'DERIVE_XPUB_CHANNEL';
exports.RESET_ACTION_TREZOR_CHANNEL = 'RESET_ACTION_TREZOR_CHANNEL';
exports.DERIVE_ADDRESS_CHANNEL = 'DERIVE_ADDRESS_CHANNEL';
exports.SHOW_ADDRESS_CHANNEL = 'SHOW_ADDRESS_CHANNEL';
exports.TOGGLE_RTS_FLAGS_MODE_CHANNEL = 'TOGGLE_RTS_FLAGS_MODE_CHANNEL';
exports.DEVICE_NOT_CONNECTED = 'DEVICE_NOT_CONNECTED';
exports.WAIT_FOR_LEDGER_DEVICES = 'WAIT_FOR_LEDGER_DEVICES';
//# sourceMappingURL=api.js.map
