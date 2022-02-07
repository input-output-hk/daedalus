import type {
  BugReportRequestHttpOptions,
  BugReportRequestPayload,
} from '../types/bug-report-request.types';
import type { GenerateFileMetaParams } from '../types/file-meta-request.types';
import type { GeneratePaperWalletParams } from '../types/paper-wallet-request.types';
import type {
  FileDialogRequestParams,
  OpenFileDialogResponseParams,
  SaveFileDialogResponseParams,
} from '../types/file-dialog.types';
import type { GenerateAddressPDFParams } from '../types/address-pdf-request.types';
import type { GenerateVotingPDFParams } from '../types/voting-pdf-request.types';
import type { GenerateCsvParams } from '../types/csv-request.types';
import type { GenerateQRCodeParams } from '../types/save-qrCode.types';
import type {
  CardanoNodeState,
  CardanoStatus,
  FaultInjectionIpcRequest,
  TlsConfig,
} from '../types/cardano-node.types';
import type { CheckDiskSpaceResponse } from '../types/no-disk-space.types';
import type { LogFiles } from '../../renderer/app/types/LogTypes';
import type { GpuStatus } from '../../renderer/app/types/gpuStatus';
import type { ExportedByronWallet } from '../../renderer/app/types/walletExportTypes';
import type {
  MessageBody,
  StateSnapshotLogParams,
  WalletMigrationReportData,
} from '../types/logging.types';
import type { Locale } from '../types/locales.types';
import type {
  DownloadLocalDataRequest,
  DownloadLocalDataResponse,
  DownloadsLocalDataRequest,
  DownloadsLocalDataResponse,
  DownloadRequest,
  DownloadResponse,
  ResumeDownloadRequest,
  ResumeDownloadResponse,
  ClearDownloadLocalDataRequest,
  ClearDownloadLocalDataResponse,
  DeleteDownloadedFileRequest,
  DeleteDownloadedFileResponse,
  CheckFileExistsRequest,
} from '../types/downloadManager.types';
import type { StoreMessage } from '../types/electron-store.types';
import type {
  IntrospectAddressRequest,
  IntrospectAddressResponse,
} from '../types/address-introspection.types';
import type {
  HardwareWalletTransportDeviceRequest,
  HardwareWalletTransportDeviceResponse,
  HardwareWalletExtendedPublicKeyRequest,
  HardwareWalletExtendedPublicKeyResponse,
  HardwareWalletCardanoAdaAppResponse,
  LedgerSignTransactionRequest,
  LedgerSignTransactionResponse,
  TrezorSignTransactionRequest,
  TrezorSignTransactionResponse,
  HardwareWalletConnectionRequest,
} from '../types/hardware-wallets.types';

/**
 * ======================= IPC CHANNELS API =========================
 * This is the ipc-api contract between main and renderer process
 * which defines channel names and their possible message types.
 * Complex types are referenced from common/types to keep this api readable.
 * ==================================================================
 */
export const GET_LOGS_CHANNEL = 'GET_LOGS_CHANNEL';
export type GetLogsRendererRequest = void;
export type GetLogsMainResponse = LogFiles;
export const COMPRESS_LOGS_CHANNEL = 'COMPRESS_LOGS_CHANNEL';
export type CompressLogsRendererRequest = {
  logs: LogFiles;
  compressedFileName: string;
};
export type CompressLogsMainResponse = string;
export const DOWNLOAD_LOGS_CHANNEL = 'DOWNLOAD_LOGS_CHANNEL';
export type DownloadLogsRendererRequest = {
  compressedLogsFilePath: string;
  destinationPath: string;
};
export type DownloadLogsMainResponse = void;
export const GET_GPU_STATUS_CHANNEL = 'GET_GPU_STATUS_CHANNEL';
export type GetGPUStatusRendererRequest = void;
export type GetGPUStatusMainResponse = GpuStatus;

/**
 * Channel for showing ui parts specified via constants
 */
export const SHOW_UI_PART_CHANNEL = 'SHOW_UI_PART_CHANNEL';
export type ShowUiPartMainRequest = string;
export type ShowUiPartRendererResponse = void;

/**
 * Channel for toggling ui parts specified via constants
 */
export const TOGGLE_UI_PART_CHANNEL = 'TOGGLE_UI_PART_CHANNEL';
export type ToggleUiPartMainRequest = string;
export type ToggleUiPartRendererResponse = void;

/**
 * Channel for checking the disk space available
 */
export const GET_DISK_SPACE_STATUS_CHANNEL = 'GetDiskSpaceStatusChannel';
export type GetDiskSpaceStatusRendererRequest = void;
export type GetDiskSpaceStatusMainResponse = CheckDiskSpaceResponse;

/**
 * Channel for checking the state directory path
 */
export const GET_STATE_DIRECTORY_PATH_CHANNEL = 'GetStateDirectoryPathChannel';
export type GetStateDirectoryPathRendererRequest = string | any;
export type GetStateDirectoryPathMainResponse = any;

/**
 * Channel for checking the desktop directory path
 */
export const GET_DESKTOP_DIRECTORY_PATH_CHANNEL =
  'GetDesktopDirectoryPathChannel';
export type GetDesktopDirectoryPathRendererRequest = void;
export type GetDesktopDirectoryPathMainResponse = string;

/**
 * Channel for checking the system locale
 */
export const GET_SYSTEM_LOCALE_CHANNEL = 'GetSystemLocaleChannel';
export type GetSystemLocaleRendererRequest = void;
export type GetSystemLocaleMainResponse = Locale;

/**
 * Channel for setting log state snapshot
 */
export const SET_STATE_SNAPSHOT_LOG_CHANNEL = 'SetStateSnapshotLogChannel';
export type SetStateSnapshotLogRendererRequest =
  | StateSnapshotLogParams
  | MessageBody;
export type SetStateSnapshotLogMainResponse = StateSnapshotLogParams;

/**
 * Channel for loading a base64 encoded asset from within the `source/renderer` folder
 */
export const LOAD_ASSET_CHANNEL = 'LoadAssetChannel';
export type LoadAssetRendererRequest = {
  fileName: string;
};
export type LoadAssetMainResponse = string;

/**
 * Channel for opening an external url in the default browser
 */
export const OPEN_EXTERNAL_URL_CHANNEL = 'OPEN_EXTERNAL_URL_CHANNEL';
export type OpenExternalUrlRendererRequest = string;
export type OpenExternalUrlMainResponse = void;

/**
 * Channel for opening a local directory in the default desktop explorer
 */
export const OPEN_LOCAL_DIRECTORY_CHANNEL = 'OpenLocalDirectoryChannel';
export type OpenLocalDirectoryRendererRequest = string;
export type OpenLocalDirectoryMainResponse = void;

/**
 * Channel to send bug report requests
 */
export const SUBMIT_BUG_REPORT_REQUEST_CHANNEL =
  'SUBMIT_BUG_REPORT_REQUEST_CHANNEL';
export type SubmitBugReportRendererRequest = {
  httpOptions: BugReportRequestHttpOptions;
  requestPayload?: BugReportRequestPayload;
};
export type SubmitBugReportRequestMainResponse = void;

/**
 * Channel to rebuild the electron application menu after the language setting changes
 */
export const REBUILD_APP_MENU_CHANNEL = 'REBUILD_APP_MENU_CHANNEL';
export type RebuildAppMenuRendererRequest = {
  isNavigationEnabled: boolean;
};
export type RebuildAppMenuMainResponse = void;

/**
 * Channel to generate file blob
 */
export const GENERATE_FILE_META_CHANNEL = 'GENERATE_FILE_META_CHANNEL';
export type GenerateFileMetaRendererRequest = GenerateFileMetaParams;
export type GenerateFileMetaMainResponse = any;

/**
 * Channel to generate and save a paper wallet certificate
 */
export const GENERATE_PAPER_WALLET_CHANNEL = 'GENERATE_PAPER_WALLET_CHANNEL';
export type GeneratePaperWalletRendererRequest = GeneratePaperWalletParams;
export type GeneratePaperWalletMainResponse = void;

/**
 * Channel to generate and save a share address PDF
 */
export const GENERATE_ADDRESS_PDF_CHANNEL = 'GENERATE_ADDRESS_PDF_CHANNEL';
export type GenerateAddressPDFRendererRequest = GenerateAddressPDFParams;
export type GenerateAddressPDFMainResponse = void;

/**
 * Channel to generate and save a share voting PDF
 */
export const GENERATE_VOTING_PDF_CHANNEL = 'GENERATE_VOTING_PDF_CHANNEL';
export type GenerateVotingPDFRendererRequest = GenerateVotingPDFParams;
export type GenerateVotingPDFMainResponse = void;

/**
 * Channel to generate and save a csv file
 */
export const GENERATE_CSV_CHANNEL = 'GENERATE_CSV_CHANNEL';
export type GenerateCsvRendererRequest = GenerateCsvParams;
export type GenerateCsvMainResponse = void;

/**
 * Channel to generate and save a QR code
 */
export const GENERATE_QRCODE_CHANNEL = 'GENERATE_QRCODE_CHANNEL';
export type GenerateQRCodeRendererRequest = GenerateQRCodeParams;
export type GenerateQRCodeMainResponse = void;

/**
 * ====================== CARDANO IPC CHANNELS ======================
 * This is the ipc-api contract between main & renderer process
 * to communicate with the cardano-node manager code.
 * ==================================================================
 */

/**
 * Channel to indicate that cardano-node will exit for updating
 */
export const CARDANO_AWAIT_UPDATE_CHANNEL = 'CARDANO_AWAIT_UPDATE_CHANNEL';
export type CardanoAwaitUpdateRendererRequest = void;
export type CardanoAwaitUpdateMainResponse = void;

/**
 * Channel where main process tells the renderer about cardano-node state updates
 */
export const CARDANO_STATE_CHANNEL = 'CARDANO_STATE_CHANNEL';
export type CardanoStateRendererRequest = void;
export type CardanoStateRendererResponse = CardanoNodeState;

/**
 * Channel to exchange tls config between main and renderer process
 */
export const CARDANO_TLS_CONFIG_CHANNEL = 'CARDANO_TLS_CONFIG_CHANNEL';
export type CardanoTlsConfigRendererRequest = void;
export type CardanoTlsConfigMainResponse = TlsConfig | null | undefined;

/**
 * Channel where renderer can request a cardano-node restart
 */
export const CARDANO_RESTART_CHANNEL = 'CARDANO_RESTART_CHANNEL';
export type CardanoRestartRendererRequest = void;
export type CardanoRestartMainResponse = void;

/**
 * Channel where render process can toggle cardano-node fault injections
 */
export const CARDANO_FAULT_INJECTION_CHANNEL =
  'CARDANO_FAULT_INJECTION_CHANNEL';
export type CardanoFaultInjectionRendererRequest = FaultInjectionIpcRequest;
export type CardanoFaultInjectionMainResponse = void;

/**
 * Channel where renderer can ask for the last cached cardano-node status
 */
export const GET_CACHED_CARDANO_STATUS_CHANNEL =
  'GET_CACHED_CARDANO_STATUS_CHANNEL';
export type GetCachedCardanoStatusRendererRequest = void;
export type GetCachedCardanoStatusMainResponse =
  | CardanoStatus
  | null
  | undefined;

/**
 * Channel where renderer and main process can exchange cardano-node status info
 */
export const SET_CACHED_CARDANO_STATUS_CHANNEL =
  'SET_CACHED_CARDANO_STATUS_CHANNEL';
export type SetCachedCardanoStatusRendererRequest =
  | CardanoStatus
  | null
  | undefined;
export type SetCachedCardanoStatusMainResponse = void;

/**
 * Channel where renderer can ask main process to export wallets
 */
export const EXPORT_WALLETS_CHANNEL = 'EXPORT_WALLETS_CHANNEL';
export type ExportWalletsRendererRequest = {
  exportSourcePath: string;
  locale: string;
};
export type ExportWalletsMainResponse = {
  wallets: Array<ExportedByronWallet>;
  errors: string;
};

/**
 * Channel for generating wallet migration report
 */
export const GENERATE_WALLET_MIGRATION_REPORT_CHANNEL =
  'GENERATE_WALLET_MIGRATION_REPORT_CHANNEL';
export type GenerateWalletMigrationReportRendererRequest = WalletMigrationReportData;
export type GenerateWalletMigrationReportMainResponse = void;

/**
 * Channel for enabling application menu navigation
 */
export const ENABLE_APPLICATION_MENU_NAVIGATION_CHANNEL =
  'ENABLE_APPLICATION_MENU_NAVIGATION_CHANNEL';
export type EnableApplicationMenuNavigationRendererRequest = void;
export type EnableApplicationMenuNavigationMainResponse = void;

/**
 * Channel for generating wallet migration report
 */
export const GET_WASM_BINARY_CHANNEL = 'GET_WASM_BINARY_CHANNEL';
export type getRecoveryWalletIdRendererRequest = Array<string>;
export type getRecoveryWalletIdMainResponse = string;

/**
 * Channel for showing open dialog
 */
export const SHOW_OPEN_DIALOG_CHANNEL = 'SHOW_OPEN_DIALOG_CHANNEL';
export type ShowOpenDialogRendererRequest = FileDialogRequestParams;
export type ShowOpenDialogMainResponse = OpenFileDialogResponseParams;

/**
 * Channel for showing save dialog
 */
export const SHOW_SAVE_DIALOG_CHANNEL = 'SHOW_SAVE_DIALOG_CHANNEL';
export type ShowSaveDialogRendererRequest = FileDialogRequestParams;
export type ShowSaveDialogMainResponse = SaveFileDialogResponseParams;

/**
 * Channel for electron-store
 */
export const ELECTRON_STORE_CHANNEL = 'ELECTRON_STORE_CHANNEL';
export type ElectronStoreMessage = StoreMessage;

/**
 * Channel for requesting a new download
 */
export const REQUEST_DOWNLOAD = 'REQUEST_DOWNLOAD';
export type DownloadRendererRequest = DownloadRequest;
export type DownloadMainResponse = DownloadResponse;

/**
 * Channel for resuming an existing download
 */
export const RESUME_DOWNLOAD = 'RESUME_DOWNLOAD';
export type ResumeDownloadRendererRequest = ResumeDownloadRequest;
export type ResumeDownloadMainResponse = ResumeDownloadResponse | void;

/**
 * Channel for resuming an existing download
 */
export const DELETE_DOWNLOADED_FILE = 'DELETE_DOWNLOADED_FILE';
export type DeleteDownloadedFileRendererRequest = DeleteDownloadedFileRequest;
export type DeleteDownloadedFileMainResponse = DeleteDownloadedFileResponse | void;

/**
 * Channel for initiating the download manager
 */
export const GET_DOWNLOAD_LOCAL_DATA = 'GET_DOWNLOAD_LOCAL_DATA';
export type DownloadLocalDataRendererRequest = DownloadLocalDataRequest;
export type DownloadLocalDataMainResponse = DownloadLocalDataResponse;

/**
 * Channel for initiating the download manager
 */
export const GET_DOWNLOADS_LOCAL_DATA = 'GET_DOWNLOADS_LOCAL_DATA';
export type DownloadsLocalDataRendererRequest = DownloadsLocalDataRequest | void;
export type DownloadsLocalDataMainResponse = DownloadsLocalDataResponse | void;

/**
 * Channel for initiating the download manager
 */
export const CLEAR_DOWNLOAD_LOCAL_DATA = 'CLEAR_DOWNLOAD_LOCAL_DATA';
export type ClearDownloadLocalDataRendererRequest = ClearDownloadLocalDataRequest;
export type ClearDownloadLocalDataMainResponse = ClearDownloadLocalDataResponse;

/**
 * Channel for checking if the downloaded file still exists
 */
export const CHECK_FILE_EXISTS = 'CHECK_FILE_EXISTS';
export type CheckFileExistsRendererRequest = CheckFileExistsRequest;
export type CheckFileExistsMainResponse = boolean;

/**
 * Channel for quitting Daedalus and installing update
 */
export const MANAGE_APP_UPDATE = 'MANAGE_APP_UPDATE';
export type ManageAppUpdateRendererRequest = {
  filePath: string;
  hash: string;
};
export type ManageAppUpdateMainResponse = {
  status: 'progress' | 'success' | 'error';
  data: {
    message?: string;
    progress?: number;
    code?: number;
    error?: Error;
    info?: Record<string, any>;
  };
};
export type DeriveXpubRendererRequestType = {
  parentXpubHex: string;
  lastIndex: number;
  derivationScheme: number;
};
export type StakingBlockchainPointer = {
  blockIndex: number;
  txIndex: number;
  certificateIndex: number;
};
export type deriveAddressRendererRequestType = {
  devicePath: string | null | undefined;
  addressType: number;
  networkId: number;
  protocolMagic: number;
  spendingPathStr: string;
  stakingPathStr: string | null | undefined;
  isTrezor: boolean;
};
export type showAddressRendererRequestType = {
  devicePath: string | null | undefined;
  addressType: number;
  networkId: number;
  protocolMagic: number;
  spendingPathStr: string;
  stakingPathStr: string | null | undefined;
  isTrezor: boolean;
};

/**
 * Channel for introspecting an address
 */
export const INTROSPECT_ADDRESS_CHANNEL = 'INTROSPECT_ADDRESS_CHANNEL';
export type IntrospectAddressRendererRequest = IntrospectAddressRequest;
export type IntrospectAddressMainResponse = IntrospectAddressResponse;

/**
 * Channel for checking block replay progress
 */
export const GET_BLOCK_REPLAY_STATUS_CHANNEL = 'GetBlockReplayProgressChannel';
export type GetBlockReplayProgressRendererRequest = void;
export type GetBlockReplayProgressMainResponse = number;

/**
 * Channels for connecting / interacting with Hardware Wallet devices
 */
export const GET_HARDWARE_WALLET_TRANSPORT_CHANNEL =
  'GET_HARDWARE_WALLET_TRANSPORT_CHANNEL';
export type getHardwareWalletTransportRendererRequest = HardwareWalletTransportDeviceRequest;
export type getHardwareWalletTransportMainResponse = HardwareWalletTransportDeviceResponse;
export const GET_EXTENDED_PUBLIC_KEY_CHANNEL =
  'GET_EXTENDED_PUBLIC_KEY_CHANNEL';
export type getExtendedPublicKeyRendererRequest = HardwareWalletExtendedPublicKeyRequest;
export type getExtendedPublicKeyMainResponse = HardwareWalletExtendedPublicKeyResponse;
export const GET_CARDANO_ADA_APP_CHANNEL = 'GET_CARDANO_ADA_APP_CHANNEL';
export type getCardanoAdaAppRendererRequest = {
  path: string | null | undefined;
};
export type getCardanoAdaAppMainResponse = HardwareWalletCardanoAdaAppResponse;
export const GET_HARDWARE_WALLET_CONNECTION_CHANNEL =
  'GET_HARDWARE_WALLET_CONNECTION_CHANNEL';
export type getHardwareWalletConnectiontMainRequest = HardwareWalletConnectionRequest;
export type getHardwareWalletConnectiontRendererResponse = Record<string, any>;
export const SIGN_TRANSACTION_LEDGER_CHANNEL =
  'SIGN_TRANSACTION_LEDGER_CHANNEL';
export type signTransactionLedgerRendererRequest = LedgerSignTransactionRequest;
export type signTransactionLedgerMainResponse = LedgerSignTransactionResponse;
export const SIGN_TRANSACTION_TREZOR_CHANNEL =
  'SIGN_TRANSACTION_TREZOR_CHANNEL';
export type signTransactionTrezorRendererRequest = TrezorSignTransactionRequest;
export type signTransactionTrezorMainResponse = TrezorSignTransactionResponse;
export const GET_INIT_TREZOR_CONNECT_CHANNEL =
  'GET_INIT_TREZOR_CONNECT_CHANNEL';
export type handleInitTrezorConnectRendererRequest = void;
export type handleInitTrezorConnectMainResponse = void;
export const GET_INIT_LEDGER_CONNECT_CHANNEL =
  'GET_INIT_LEDGER_CONNECT_CHANNEL';
export type handleInitLedgerConnectRendererRequest = void;
export type handleInitLedgerConnectMainResponse = void;
export const DERIVE_XPUB_CHANNEL = 'DERIVE_XPUB_CHANNEL';
export type deriveXpubRendererRequest = DeriveXpubRendererRequestType;
export type deriveXpubMainResponse = string;
export const RESET_ACTION_TREZOR_CHANNEL = 'RESET_ACTION_TREZOR_CHANNEL';
export type resetTrezorActionRendererRequest = void;
export type resetTrezorActionMainResponse = void;
export const DERIVE_ADDRESS_CHANNEL = 'DERIVE_ADDRESS_CHANNEL';
export type deriveAddressRendererRequest = deriveAddressRendererRequestType;
export type deriveAddressMainResponse = string;
export const SHOW_ADDRESS_CHANNEL = 'SHOW_ADDRESS_CHANNEL';
export type showAddressRendererRequest = showAddressRendererRequestType;
export type showAddressMainResponse = void;
