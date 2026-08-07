import fs from 'fs';
import path from 'path';
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
import type { ChainStorageValidation } from '../../common/types/watchdog.types';

const validateChannel = new MainIpcChannel<
  ValidateChainStorageRendererRequest,
  ValidateChainStorageMainResponse
>(VALIDATE_CHAIN_STORAGE_CHANNEL);

const confirmChannel = new MainIpcChannel<
  ConfirmChainStorageRendererRequest,
  ConfirmChainStorageMainResponse
>(CONFIRM_CHAIN_STORAGE_CHANNEL);

function getAvailableSpaceBytes(targetPath: string): number | undefined {
  try {
    // Use statvfs on the target path's mount point
    // @ts-ignore — fs.statfsSync is available in Node 19+ / Electron 28+
    const stats = fs.statfsSync(targetPath);
    return stats.bfree * stats.bsize;
  } catch {
    return undefined;
  }
}

function validatePath(
  candidatePath: string,
  stateDir: string
): ChainStorageValidation {
  const resolved = path.resolve(candidatePath);

  // Must not be a file
  try {
    const stat = fs.statSync(resolved);
    if (stat.isFile()) {
      return { isValid: false, path: candidatePath, reason: 'path-is-file' };
    }
  } catch {
    // Path doesn't exist yet — check parent exists
    const parent = path.dirname(resolved);
    try {
      const parentStat = fs.statSync(parent);
      if (!parentStat.isDirectory()) {
        return {
          isValid: false,
          path: candidatePath,
          reason: 'path-not-found',
        };
      }
    } catch {
      return { isValid: false, path: candidatePath, reason: 'path-not-found' };
    }
  }

  // Must not be inside the state dir
  const normalizedState = path.resolve(stateDir);
  const normalizedCandidate = resolved;
  if (
    normalizedCandidate === normalizedState ||
    normalizedCandidate.startsWith(normalizedState + path.sep)
  ) {
    return { isValid: false, path: candidatePath, reason: 'inside-state-dir' };
  }

  // Check writability of the target (or its parent if it doesn't exist)
  const writeCheckTarget = fs.existsSync(resolved)
    ? resolved
    : path.dirname(resolved);
  try {
    fs.accessSync(writeCheckTarget, fs.constants.W_OK);
  } catch {
    return { isValid: false, path: candidatePath, reason: 'not-writable' };
  }

  const chainSubdir = path.join(resolved, 'chain');
  const chainSubdirectoryStatus: ChainStorageValidation['chainSubdirectoryStatus'] =
    fs.existsSync(chainSubdir) ? 'existing-directory' : 'will-create';

  const availableSpaceBytes = getAvailableSpaceBytes(writeCheckTarget);

  return {
    isValid: true,
    path: candidatePath,
    resolvedPath: resolved,
    availableSpaceBytes,
    chainSubdirectoryStatus,
  };
}

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
