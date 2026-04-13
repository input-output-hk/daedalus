import path from 'path';
import checkDiskSpace from 'check-disk-space';
import { DISK_SPACE_REQUIRED } from '../config';
import type {
  ChainStorageConfig,
  ChainStorageValidation,
} from '../../common/types/mithril-bootstrap.types';
import { logger } from './logging';
import { validateChainStorageDirectory } from './chainStorageValidation';
import type {
  ChainStorageDefaults,
  ChainStorageManagerContext,
} from './chainStorageManagerShared';

export async function getDefaultStorageConfig(
  ctx: ChainStorageManagerContext
): Promise<ChainStorageDefaults> {
  const {
    exists: stateDirectoryExists,
    resolvedPath: resolvedStatePath,
  } = await ctx._resolveStateDirectoryPath();
  const diskCheckPath = stateDirectoryExists
    ? resolvedStatePath
    : path.dirname(resolvedStatePath);
  const { free } = await checkDiskSpace(diskCheckPath);

  return {
    defaultPath: path.join(resolvedStatePath, 'chain'),
    availableSpaceBytes: free,
    requiredSpaceBytes: DISK_SPACE_REQUIRED,
  };
}

export async function getConfig(
  ctx: ChainStorageManagerContext
): Promise<ChainStorageConfig> {
  try {
    const defaultStorageConfig = await ctx._getDefaultStorageConfig();
    const chainState = await ctx._captureChainPathState();
    let customPath = null;

    if (chainState.type === 'symlink' && chainState.resolvedPath) {
      customPath = path.dirname(chainState.resolvedPath);
    }

    return {
      customPath,
      ...defaultStorageConfig,
      isRecoveryFallback: ctx._isRecoveryFallback || undefined,
    };
  } catch (error) {
    logger.warn('ChainStorageManager: failed to derive storage config', {
      error,
      chainPath: ctx._chainPath,
    });

    try {
      const fallbackConfig = await ctx._getDefaultStorageConfig();
      return {
        customPath: null,
        ...fallbackConfig,
        isRecoveryFallback: ctx._isRecoveryFallback || undefined,
      };
    } catch {
      return {
        customPath: null,
        defaultPath: ctx._chainPath,
        availableSpaceBytes: Number.NaN,
        requiredSpaceBytes: DISK_SPACE_REQUIRED,
        isRecoveryFallback: ctx._isRecoveryFallback || undefined,
      };
    }
  }
}

export async function validate(
  ctx: ChainStorageManagerContext,
  targetDir: string | null
): Promise<ChainStorageValidation> {
  const config = await ctx.getConfig();

  return validateChainStorageDirectory(
    targetDir,
    ctx._stateDirectoryPath,
    () => ctx._getDefaultStorageConfig(),
    DISK_SPACE_REQUIRED,
    {
      currentCustomPath: config.customPath,
    }
  );
}
