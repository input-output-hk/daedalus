import path from 'path';
import fs from 'fs-extra';
import { ChainStorageConfig } from '../../common/types/mithril-bootstrap.types';
import { logger } from './logging';

const CHAIN_DIRECTORY_NAME = 'chain';
const CHAIN_STORAGE_CONFIG_FILE = 'chain-storage-config.json';

export type ResolvedStateDirectory = {
  exists: boolean;
  resolvedPath: string;
};

/**
 * Resolves the Daedalus state directory path, accounting for the directory
 * not existing yet.
 */
export async function resolveStateDirectoryPath(
  stateDir: string
): Promise<ResolvedStateDirectory> {
  const exists = await fs.pathExists(stateDir);

  return {
    exists,
    resolvedPath: exists ? await fs.realpath(stateDir) : path.resolve(stateDir),
  };
}

/**
 * Resolves the actual chain storage path, preferring the symlink target if
 * present, falling back to the custom-path in config, then stateDir.
 */
export async function resolveChainStoragePath(
  stateDir: string,
  getConfig: () => Promise<ChainStorageConfig>
): Promise<string> {
  const chainPath = path.join(stateDir, CHAIN_DIRECTORY_NAME);

  try {
    const chainPathExists = await fs.pathExists(chainPath);
    if (chainPathExists) {
      return await fs.realpath(chainPath);
    }
  } catch (error) {
    logger.warn('ChainStorageManager: failed to resolve chain path', {
      error,
      chainPath,
    });
  }

  const config = await getConfig();
  if (config.customPath) {
    try {
      return await fs.realpath(config.customPath);
    } catch (error) {
      logger.warn(
        'ChainStorageManager: failed to resolve configured custom path',
        {
          error,
          customPath: config.customPath,
        }
      );
    }
  }

  return stateDir;
}

/**
 * Resolves the working directory that Mithril should use. Returns the
 * resolved custom path when configured, otherwise the state directory.
 */
export async function resolveMithrilWorkDir(
  stateDir: string,
  getConfig: () => Promise<ChainStorageConfig>
): Promise<string> {
  const config = await getConfig();

  if (!config.customPath) {
    return stateDir;
  }

  try {
    return await fs.realpath(config.customPath);
  } catch (error) {
    logger.warn('ChainStorageManager: failed to resolve Mithril work dir', {
      error,
      customPath: config.customPath,
    });
    return stateDir;
  }
}

/**
 * Resolves the current source of chain data: the symlink target, the local
 * chain directory, or null if neither is present.
 */
export async function resolveCurrentChainSource(
  chainPath: string
): Promise<string | null> {
  const chainPathExists = await fs.pathExists(chainPath);
  if (!chainPathExists) {
    return null;
  }

  const chainStats = await fs.lstat(chainPath);
  if (chainStats.isSymbolicLink()) {
    try {
      return await fs.realpath(chainPath);
    } catch (error) {
      logger.warn('ChainStorageManager: unable to resolve chain symlink', {
        error,
        chainPath,
      });
      return null;
    }
  }

  if (chainStats.isDirectory()) {
    return chainPath;
  }

  return null;
}
