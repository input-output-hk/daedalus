// @flow
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
import type { GenerateRewardsCsvParams } from '../types/rewards-csv-request.types';
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
  StateSnapshotLogParams,
  WalletMigrationReportData,
} from '../types/logging.types';

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
  logs: LogFiles,
  compressedFileName: string,
};
export type CompressLogsMainResponse = string;

export const DOWNLOAD_LOGS_CHANNEL = 'DOWNLOAD_LOGS_CHANNEL';
export type DownloadLogsRendererRequest = {
  compressedLogsFilePath: string,
  destinationPath: string,
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
export type GetDiskSpaceStatusRendererRequest = number | any;
export type GetDiskSpaceStatusMainResponse = CheckDiskSpaceResponse;

/**
 * Channel for checking the state directory path
 */
export const GET_STATE_DIRECTORY_PATH_CHANNEL = 'GetStateDirectoryPathChannel';
export type GetStateDirectoryPathRendererRequest = string | any;
export type GetStateDirectoryPathMainResponse = any;

/**
 * Channel for setting log state snapshot
 */
export const SET_STATE_SNAPSHOT_LOG_CHANNEL = 'SetStateSnapshotLogChannel';
export type SetStateSnapshotLogRendererRequest = StateSnapshotLogParams | any;
export type SetStateSnapshotLogMainResponse = StateSnapshotLogParams | any;

/**
 * Channel for loading a base64 encoded asset from within the `source/renderer` folder
 */
export const LOAD_ASSET_CHANNEL = 'LoadAssetChannel';
export type LoadAssetRendererRequest = { fileName: string };
export type LoadAssetMainResponse = string;

/**
 * Channel for opening an external url in the default browser
 */
export const OPEN_EXTERNAL_URL_CHANNEL = 'OPEN_EXTERNAL_URL_CHANNEL';
export type OpenExternalUrlRendererRequest = string;
export type OpenExternalUrlMainResponse = void;

/**
 * Channel for opening an local directory in the default desktop explorer
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
  httpOptions: BugReportRequestHttpOptions,
  requestPayload?: BugReportRequestPayload,
};
export type SubmitBugReportRequestMainResponse = void;

/**
 * Channel to rebuild the electron application menu after the language setting changes
 */
export const REBUILD_APP_MENU_CHANNEL = 'REBUILD_APP_MENU_CHANNEL';
export type RebuildAppMenuRendererRequest = { isUpdateAvailable: boolean };
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
 * Channel to generate and save a rewards csv
 */
export const GENERATE_REWARDS_CSV_CHANNEL = 'GENERATE_REWARDS_CSV_CHANNEL';
export type GenerateRewardsCsvRendererRequest = GenerateRewardsCsvParams;
export type GenerateRewardsCsvMainResponse = void;

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
export type CardanoTlsConfigMainResponse = ?TlsConfig;

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
export type GetCachedCardanoStatusMainResponse = ?CardanoStatus;

/**
 * Channel where renderer and main process can exchange cardano-node status info
 */
export const SET_CACHED_CARDANO_STATUS_CHANNEL =
  'SET_CACHED_CARDANO_STATUS_CHANNEL';
export type SetCachedCardanoStatusRendererRequest = ?CardanoStatus;
export type SetCachedCardanoStatusMainResponse = void;

/**
 * Channel where renderer can ask main process for the result of electron's app.getLocale()
 */
export const DETECT_SYSTEM_LOCALE_CHANNEL = 'DETECT_SYSTEM_LOCALE_CHANNEL';
export type DetectSystemLocaleRendererRequest = void;
export type DetectSystemLocaleMainResponse = string;

/**
 * Channel where renderer can ask main process to export wallets
 */
export const EXPORT_WALLETS_CHANNEL = 'EXPORT_WALLETS_CHANNEL';
export type ExportWalletsRendererRequest = void;
export type ExportWalletsMainResponse = {
  wallets: Array<ExportedByronWallet>,
  errors: string,
};

/**
 * Channel for generating wallet migration report
 */
export const GENERATE_WALLET_MIGRATION_REPORT_CHANNEL =
  'GENERATE_WALLET_MIGRATION_REPORT_CHANNEL';
export type GenerateWalletMigrationReportRendererRequest = WalletMigrationReportData;
export type GenerateWalletMigrationReportMainResponse = void;

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
export type ElectronStoreMessage = {
  type: 'get' | 'set' | 'delete',
  key: string,
  data?: any,
};
