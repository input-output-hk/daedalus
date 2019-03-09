// @flow
/**
 * The ipc api contract between main and renderer process.
 * Defines channel names and their possible message types.
 * Complex types are referenced from the common/types folder to keep this api readable.
 */
import type {
  ReportRequestHttpOptions,
  ReportRequestPayload
} from '../types/report-request.types';
import type { GeneratePaperWalletParams } from '../types/paper-wallet-request.types';
import type { CheckDiskSpaceResponse } from '../types/no-disk-space.types';
import type { ExtractedWallets } from '../types/wallet-importer.types';

/**
 * Channel for loading an base64 encoded asset from within the `source/renderer` folder
 */
export const LoadAssetChannelName = 'LoadAssetChannel';
export type LoadAssetRendererRequest = { fileName: string };
export type LoadAssetMainResponse = string;

/**
 * Channel for opening a url in the default browser
 */
export const OpenExternalUrlChannelName = 'OpenExternalUrlChannel';
export type OpenExternalUrlRendererRequest = string;
export type OpenExternalUrlMainResponse = void;

/**
 * Channel for checking the disk space available
 */
export const GetDiskSpaceStatusChannelName = 'GetDiskSpaceStatusChannel';
export type GetDiskSpaceStatusRendererRequest = number | any;
export type GetDiskSpaceStatusMainResponse = CheckDiskSpaceResponse;

/**
 * Channel to send bug report requests
 */
export const ReportRequestChannelName = 'ReportRequestChannel';
export type ReportRequestRendererRequest = {
  httpOptions: ReportRequestHttpOptions,
  requestPayload?: ReportRequestPayload
}
export type ReportRequestMainResponse = void;

/**
 * Channel to generate and save a paper wallet certificate
 */
export const GeneratePaperWalletChannelName = 'GeneratePaperWalletChannel';
export type GeneratePaperWalletRendererRequest = GeneratePaperWalletParams;
export type GeneratePaperWalletMainResponse = void;

/**
 * Channel to rebuild the electron application menu after the language setting changes
 */
export const RebuildApplicationMenu = 'RebuildApplicationMenu';

/**
 * Channel to get the number of epochs consolidated
 */
export const GetNumberOfEpochsConsolidatedChannel = 'GetNumberOfEpochsConsolidatedChannel';
export type GetNumberOfEpochsConsolidatedChannelResponse = number;

/**
 * Channel to get the system start time
 */
export const GetSystemStartTimeChannel = 'GetSystemStartTimeChannel';
export type GetSystemStartTimeResponse = number;

/**
 * Channel for extracting wallets from secret.key file
 */
export const ExtractWalletsChannelName = 'ExtractWalletsChannel';
export type ExtractWalletsRendererRequest = { secretKeyFilePath: string };
export type ExtractWalletsMainResponse = ExtractedWallets;

/**
 * Channel for matching extracted wallets passwords
 */
export const MatchWalletsPasswordsChannelName = 'MatchWalletsPasswordsChannel';
export type MatchWalletsPasswordsRendererRequest = {
  wallets: ExtractedWallets,
  passwords: Array<string>,
};
export type MatchWalletsPasswordsMainResponse = ExtractedWallets;

// CardanoNode ipc channels
// TODO: refactor to improved structure above
export const CARDANO_AWAIT_UPDATE_CHANNEL = 'CARDANO_AWAIT_UPDATE_CHANNEL';
export const CARDANO_STATE_CHANGE_CHANNEL = 'CARDANO_STATE_CHANGE_CHANNEL';
export const CARDANO_TLS_CONFIG_CHANNEL = 'CARDANO_TLS_CONFIG_CHANNEL';
export const CARDANO_RESTART_CHANNEL = 'CARDANO_RESTART_CHANNEL';
export const CARDANO_FAULT_INJECTION_CHANNEL = 'CARDANO_FAULT_INJECTION_CHANNEL';
export const CARDANO_STATUS_CHANNEL = 'CARDANO_STATUS_CHANNEL';

export const GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL = 'GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL';

export const TOGGLE_ABOUT_DIALOG_CHANNEL = 'TOGGLE_ABOUT_DIALOG_CHANNEL';
export const TOGGLE_NETWORK_STATUS_DIALOG_CHANNEL = 'TOGGLE_NETWORK_STATUS_DIALOG_CHANNEL';
export const TOGGLE_BLOCK_CONSOLIDATION_STATUS_SCREEN_CHANNEL = 'TOGGLE_BLOCK_CONSOLIDATION_STATUS';

export const GO_TO_WALLET_IMPORTER_SCREEN_CHANNEL = 'GO_TO_WALLET_IMPORTER_SCREEN_CHANNEL';
