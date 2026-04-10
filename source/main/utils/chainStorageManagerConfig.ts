import path from 'path';
import fs from 'fs-extra';
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
    const configExists = await fs.pathExists(ctx._configPath);

    if (!configExists) {
      return {
        customPath: null,
        ...defaultStorageConfig,
      };
    }

    const parsed = await fs.readJson(ctx._configPath);
    const customPath =
      typeof parsed?.customPath === 'string' && parsed.customPath.trim()
        ? parsed.customPath.trim()
        : null;
    const setAt =
      typeof parsed?.setAt === 'string' && parsed.setAt.trim()
        ? parsed.setAt.trim()
        : undefined;

    return {
      customPath,
      ...defaultStorageConfig,
      setAt,
    };
  } catch (error) {
    logger.warn('ChainStorageManager: failed to read storage config', {
      error,
      configPath: ctx._configPath,
    });

    try {
      const fallbackConfig = await ctx._getDefaultStorageConfig();
      return { customPath: null, ...fallbackConfig };
    } catch {
      return {
        customPath: null,
        defaultPath: ctx._chainPath,
        availableSpaceBytes: Number.NaN,
        requiredSpaceBytes: DISK_SPACE_REQUIRED,
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

export async function verifySymlink(
  ctx: ChainStorageManagerContext
): Promise<ChainStorageValidation> {
  const config = await ctx.getConfig();
  if (!config.customPath) {
    return {
      isValid: true,
      path: null,
    };
  }

  const resolvedCustomPath = await ctx._resolveRealPathOrInput(
    config.customPath
  );
  const expectedManagedChainPath = ctx._getManagedChainPath(resolvedCustomPath);
  const chainState = await ctx._captureChainPathState();

  if (chainState.type !== 'symlink') {
    return {
      isValid: false,
      path: config.customPath,
      reason: 'unknown',
      message: 'Chain path is not a symlink or junction.',
    };
  }

  if (!chainState.resolvedPath) {
    return {
      isValid: false,
      path: config.customPath,
      reason: 'path-not-found',
      message: 'Chain symlink target is unavailable.',
    };
  }

  if (!ctx._isSamePath(chainState.resolvedPath, expectedManagedChainPath)) {
    return {
      isValid: false,
      path: config.customPath,
      resolvedPath: chainState.resolvedPath,
      reason: 'unknown',
      message:
        'Chain symlink target does not match the configured managed chain directory.',
    };
  }

  return {
    isValid: true,
    path: config.customPath,
    resolvedPath: chainState.resolvedPath,
    chainSubdirectoryStatus: 'existing-directory',
  };
}
