import { MainIpcChannel } from './lib/MainIpcChannel';
import { backendLifecycle } from '../BackendLifecycle';
import { requestElectronStore } from './electronStoreConversation';
import { logger } from '../utils/logging';
import { stateDirectoryPath } from '../config';
import {
  STORAGE_KEYS as keys,
  STORAGE_TYPES as types,
} from '../../common/config/electron-store.config';
import {
  VALIDATE_CHAIN_STORAGE_CHANNEL,
  CONFIRM_CHAIN_STORAGE_CHANNEL,
} from '../../common/ipc/api';
import type {
  ValidateChainStorageRendererRequest,
  ValidateChainStorageMainResponse,
  ConfirmChainStorageRendererRequest,
  ConfirmChainStorageMainResponse,
} from '../../common/ipc/api';
import { validatePath } from '../utils/chainStorageValidate';

const validateChannel = new MainIpcChannel<
  ValidateChainStorageRendererRequest,
  ValidateChainStorageMainResponse
>(VALIDATE_CHAIN_STORAGE_CHANNEL);

const confirmChannel = new MainIpcChannel<
  ConfirmChainStorageRendererRequest,
  ConfirmChainStorageMainResponse
>(CONFIRM_CHAIN_STORAGE_CHANNEL);

export function handleChainStorageRequests(): void {
  validateChannel.onRequest(async ({ path: candidatePath }) => {
    logger.info('chainStorage: validating path', { path: candidatePath });
    return validatePath(candidatePath, stateDirectoryPath);
  });

  confirmChannel.onRequest(async ({ customPath }) => {
    logger.info('chainStorage: confirming path', { customPath });

    if (customPath != null) {
      requestElectronStore({
        type: types.SET,
        key: keys.CUSTOM_CHAIN_PATH,
        data: customPath,
      });
    } else {
      requestElectronStore({ type: types.DELETE, key: keys.CUSTOM_CHAIN_PATH });
    }

    await backendLifecycle.setCustomChainPath(customPath);
  });
}
