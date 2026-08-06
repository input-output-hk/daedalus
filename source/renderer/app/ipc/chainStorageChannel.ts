import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  VALIDATE_CHAIN_STORAGE_CHANNEL,
  CONFIRM_CHAIN_STORAGE_CHANNEL,
} from '../../../common/ipc/api';
import type {
  ValidateChainStorageRendererRequest,
  ValidateChainStorageMainResponse,
  ConfirmChainStorageRendererRequest,
  ConfirmChainStorageMainResponse,
} from '../../../common/ipc/api';

// RendererIpcChannel<Incoming, Outgoing>: Incoming = response from main, Outgoing = sent to main
export const validateChainStorageChannel = new RendererIpcChannel<
  ValidateChainStorageMainResponse,
  ValidateChainStorageRendererRequest
>(VALIDATE_CHAIN_STORAGE_CHANNEL);

export const confirmChainStorageChannel = new RendererIpcChannel<
  ConfirmChainStorageMainResponse,
  ConfirmChainStorageRendererRequest
>(CONFIRM_CHAIN_STORAGE_CHANNEL);
