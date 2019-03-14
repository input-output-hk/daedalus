// @flow
import type {
  BugReportRequestHttpOptions,
  BugReportRequestPayload
} from '../types/bug-report-request.types';
import type { GeneratePaperWalletParams } from '../types/paper-wallet-request.types';
import type {
  CardanoNodeState,
  CardanoStatus,
  FaultInjectionIpcRequest,
  TlsConfig
} from '../types/cardano-node.types';
import type { AdaRedemptionCode, AdaRedemptionDecryptionKey } from '../types/ada-redemption.types';
import type { RedemptionTypeChoices } from '../../renderer/app/types/redemptionTypes';
import type { CheckDiskSpaceResponse } from '../types/no-disk-space.types';
import type { LogFiles } from '../../renderer/app/types/LogTypes';
import type { GpuStatus } from '../../renderer/app/types/gpuStatus';

/**
 * ======================= IPC CHANNELS API =========================
 * This is the ipc-api contract between main and renderer process
 * which defines channel names and their possible message types.
 * Complex types are referenced from common/types to keep this api readable.
 * ==================================================================
 */

export const GET_LOGS_CHANNEL = 'GET_LOGS_CHANNEL';
export type GetLogsRequest = void;
export type GetLogsResponse = LogFiles;

export const COMPRESS_LOGS_CHANNEL = 'COMPRESS_LOGS_CHANNEL';
export type CompressLogsRequest = {
  logs: LogFiles,
  compressedFileName: string,
};
export type CompressLogsResponse = string;

export const DOWNLOAD_LOGS_CHANNEL = 'DOWNLOAD_LOGS_CHANNEL';
export type DownloadLogsRequest = {
  compressedLogsFilePath: string,
  destinationPath: string,
};
export type DownloadLogsResponse = void;

export const GET_GPU_STATUS_CHANNEL = 'GET_GPU_STATUS_CHANNEL';
export type GetGPUStatusRequest = void;
export type GetGPUStatusResponse = GpuStatus;

/**
 * Channel for showing ui parts specified via constants
 */
export const SHOW_UI_PART_CHANNEL = 'SHOW_UI_PART_CHANNEL';
export type ShowUiPartRequest = string;

/**
 * Channel for toggling ui parts specified via constants
 */
export const TOGGLE_UI_PART_CHANNEL = 'TOGGLE_UI_PART_CHANNEL';
export type ToggleUiPartRequest = string;

/**
 * Channel for checking the disk space available
 */
export const GET_DISK_SPACE_STATUS_CHANNEL = 'GetDiskSpaceStatusChannel';
export type GetDiskSpaceStatusRendererRequest = number | any;
export type GetDiskSpaceStatusMainResponse = CheckDiskSpaceResponse;

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
export type OpenExternalUrlRequest = string;
export type OpenExternalUrlResponse = void;

/**
 * Channel to send bug report requests
 */
export const SUBMIT_BUG_REPORT_REQUEST_CHANNEL = 'SUBMIT_BUG_REPORT_REQUEST_CHANNEL';
export type SubmitBugReportRequest = {
  httpOptions: BugReportRequestHttpOptions,
  requestPayload?: BugReportRequestPayload
}
export type SubmitBugReportRequestResponse = void;

/**
 * Channel to rebuild the electron application menu after the language setting changes
 */
export const REBUILD_APP_MENU_CHANNEL = 'REBUILD_APP_MENU_CHANNEL';

/**
 * Channel to get the number of epochs consolidated
 */
export const GET_CONSOLIDATED_EPOCHS_COUNT_CHANNEL = 'GET_CONSOLIDATED_EPOCHS_COUNT_CHANNEL';
export type GetConsolidatedEpochsCountResponse = number;

/**
 * Channel to get the system start time
 */
export const GET_SYSTEM_START_TIME_CHANNEL = 'GET_SYSTEM_START_TIME_CHANNEL';
export type GetSystemStartTimeResponse = number;

/**
 * Channel where renderer can ask the main process to parse the redemption
 * code from a given certificate, providing the file path, decryption key
 * and type of redemption that is required.
 */
export const PARSE_REDEMPTION_CODE_CHANNEL = 'PARSE_REDEMPTION_CODE_CHANNEL';
export type ParseRedemptionCodeRequest = {
  certificateFilePath: string,
  decryptionKey: ?AdaRedemptionDecryptionKey,
  redemptionType: RedemptionTypeChoices
};
export type ParseRedemptionCodeResponse = AdaRedemptionCode;

/**
 * Channel to generate and save a paper wallet certificate
 */
export const GENERATE_PAPER_WALLET_CHANNEL = 'GENERATE_PAPER_WALLET_CHANNEL';
export type GeneratePaperWalletRequest = GeneratePaperWalletParams;
export type GeneratePaperWalletResponse = void;

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
export type CardanoAwaitUpdateRequest = void;
export type CardanoAwaitUpdateResponse = void;

/**
 * Channel where main process tells the renderer about cardano-node state updates
 */
export const CARDANO_STATE_CHANNEL = 'CARDANO_STATE_CHANNEL';
export type CardanoStateRequest = void;
export type CardanoStateResponse = CardanoNodeState;

/**
 * Channel to exchange tls config between main and renderer process
 */
export const CARDANO_TLS_CONFIG_CHANNEL = 'CARDANO_TLS_CONFIG_CHANNEL';
export type CardanoTlsConfigRequest = void;
export type CardanoTlsConfigResponse = ?TlsConfig;

/**
 * Channel where renderer can request a cardano-node restart
 */
export const CARDANO_RESTART_CHANNEL = 'CARDANO_RESTART_CHANNEL';
export type CardanoRestartRequest = void;
export type CardanoRestartResponse = void;

/**
 * Channel where render process can toggle cardano-node fault injections
 */
export const CARDANO_FAULT_INJECTION_CHANNEL = 'CARDANO_FAULT_INJECTION_CHANNEL';
export type CardanoFaultInjectionRequest = FaultInjectionIpcRequest;
export type CardanoFaultInjectionResponse = void;

/**
 * Channel where renderer can ask for the last cached cardano-node status.
 */
export const GET_CACHED_CARDANO_STATUS_CHANNEL = 'GET_CACHED_CARDANO_STATUS_CHANNEL';
export type GetCachedCardanoStatusRequest = void;
export type GetCachedCardanoStatusResponse = ?CardanoStatus;

/**
 * Channel where renderer and main process can exchange cardano-node status info.
 */
export const SET_CACHED_CARDANO_STATUS_CHANNEL = 'SET_CACHED_CARDANO_STATUS_CHANNEL';
export type SetCachedCardanoStatusRequest = ?CardanoStatus;
export type SetCachedCardanoStatusResponse = void;

/**
 * Channel where renderer can ask main process for the result of electron's app.getLocale()
 */
export const DETECT_SYSTEM_LOCALE_CHANNEL = 'DETECT_SYSTEM_LOCALE_CHANNEL';
export type DetectSystemLocaleResponse = string;
