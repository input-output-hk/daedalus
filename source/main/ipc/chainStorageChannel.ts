import { MainIpcChannel } from './lib/MainIpcChannel';
import { backendLifecycle } from '../BackendLifecycle';
import { requestElectronStore } from './electronStoreConversation';
import { logger } from '../utils/logging';
import { networkMagic, stateDirectoryPath } from '../config';
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
    return validatePath(candidatePath, stateDirectoryPath, networkMagic);
  });

  confirmChannel.onRequest(async ({ customPath, rebuild }) => {
    logger.info('chainStorage: confirming path', { customPath, rebuild });

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
    // Sent straight after the location, so the watchdog holds the rebuild
    // while it stops the node for the change and runs it in place of the
    // restart: no node starts on the new location before the rebuild.
    if (rebuild === true) {
      backendLifecycle.sendMithrilCommand({
        cmd: 'start_mithril',
        wipe_chain: true,
      });
    }
  });
}
