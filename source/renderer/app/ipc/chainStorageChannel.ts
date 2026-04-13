import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  SET_CHAIN_STORAGE_DIRECTORY_CHANNEL,
  GET_CHAIN_STORAGE_DIRECTORY_CHANNEL,
  VALIDATE_CHAIN_STORAGE_DIRECTORY_CHANNEL,
} from '../../../common/ipc/api';
import type {
  SetChainStorageDirectoryMainResponse,
  SetChainStorageDirectoryRendererRequest,
  GetChainStorageDirectoryMainResponse,
  GetChainStorageDirectoryRendererRequest,
  ValidateChainStorageDirectoryMainResponse,
  ValidateChainStorageDirectoryRendererRequest,
} from '../../../common/ipc/api';

export const setChainStorageDirectoryChannel: RendererIpcChannel<
  SetChainStorageDirectoryMainResponse,
  SetChainStorageDirectoryRendererRequest
> = new RendererIpcChannel(SET_CHAIN_STORAGE_DIRECTORY_CHANNEL);

export const getChainStorageDirectoryChannel: RendererIpcChannel<
  GetChainStorageDirectoryMainResponse,
  GetChainStorageDirectoryRendererRequest
> = new RendererIpcChannel(GET_CHAIN_STORAGE_DIRECTORY_CHANNEL);

export const validateChainStorageDirectoryChannel: RendererIpcChannel<
  ValidateChainStorageDirectoryMainResponse,
  ValidateChainStorageDirectoryRendererRequest
> = new RendererIpcChannel(VALIDATE_CHAIN_STORAGE_DIRECTORY_CHANNEL);
