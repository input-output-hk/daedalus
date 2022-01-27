// @flow

/**
 * ======================= IPC CHANNELS API =========================
 * This is the ipc-api contract between main and renderer process
 * which defines channel names and their possible message types.
 * Complex types are referenced from common/types to keep this api readable.
 * ==================================================================
 */

// Create / restore Light Wallet
export const CREATE_WALLET_CHANNEL = 'CREATE_WALLET_CHANNEL';
export type createWalletRendererRequest = void;
export type createWalletMainResponse = void;