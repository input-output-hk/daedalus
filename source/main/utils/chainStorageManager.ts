import path from 'path';
import fs from 'fs-extra';
import checkDiskSpace from 'check-disk-space';
import { DISK_SPACE_REQUIRED, stateDirectoryPath } from '../config';
import {
  ChainStorageConfig,
  ChainStorageValidation,
} from '../../common/types/mithril-bootstrap.types';
import { logger } from './logging';

const CHAIN_STORAGE_CONFIG_FILE = 'chain-storage-config.json';
const CHAIN_DIRECTORY_NAME = 'chain';

type ChainPathState = {
  type: 'missing' | 'directory' | 'symlink';
  resolvedPath?: string;
};

type ResolvedStateDirectory = {
  exists: boolean;
  resolvedPath: string;
};

export class ChainStorageManager {
  _stateDirectoryPath: string;
  _configPath: string;
  _chainPath: string;

  constructor(daedalusStateDirectoryPath: string = stateDirectoryPath) {
    this._stateDirectoryPath = daedalusStateDirectoryPath;
    this._chainPath = path.join(this._stateDirectoryPath, CHAIN_DIRECTORY_NAME);
    this._configPath = path.join(
      this._stateDirectoryPath,
      CHAIN_STORAGE_CONFIG_FILE
    );
  }

  async setDirectory(
    targetDir: string | null
  ): Promise<ChainStorageValidation> {
    const normalizedTargetDir =
      typeof targetDir === 'string' && targetDir.trim().length > 0
        ? targetDir.trim()
        : null;

    if (normalizedTargetDir == null) {
      return this.resetToDefault();
    }

    if (this._isSamePath(normalizedTargetDir, this._chainPath)) {
      return this.resetToDefault();
    }

    const validation = await this.validate(normalizedTargetDir);
    if (!validation.isValid || validation.path == null) {
      return validation;
    }

    const resolvedTargetPath = validation.resolvedPath
      ? validation.resolvedPath
      : await fs.realpath(validation.path);

    const previousState = await this._captureChainPathState();
    const previousConfig = await this.getConfig();

    if (previousState.type === 'directory') {
      const entries = await fs.readdir(this._chainPath);
      if (entries.length > 0) {
        return {
          isValid: false,
          path: validation.path,
          resolvedPath: resolvedTargetPath,
          reason: 'unknown',
          message:
            'Existing blockchain data must be cleared before switching storage locations.',
        };
      }
    }

    try {
      await fs.remove(this._chainPath);
      await fs.ensureDir(resolvedTargetPath);
      await fs.symlink(
        resolvedTargetPath,
        this._chainPath,
        process.platform === 'win32' ? 'junction' : 'dir'
      );

      const setAt = new Date().toISOString();
      await fs.writeJson(
        this._configPath,
        {
          customPath: validation.path,
          setAt,
        },
        { spaces: 2 }
      );
    } catch (error) {
      await this._rollbackSetDirectory({
        previousState,
        previousConfig,
        targetPath: resolvedTargetPath,
      });
      throw error;
    }

    return {
      ...validation,
      resolvedPath: resolvedTargetPath,
    };
  }

  async resetToDefault(): Promise<ChainStorageValidation> {
    const chainPathExists = await fs.pathExists(this._chainPath);

    if (chainPathExists) {
      const chainStats = await fs.lstat(this._chainPath);
      if (chainStats.isSymbolicLink()) {
        await fs.remove(this._chainPath);
      }
    }

    await fs.ensureDir(this._chainPath);

    await fs.remove(this._configPath);

    const defaultStorageConfig = await this._getDefaultStorageConfig();

    return {
      isValid: true,
      path: null,
      resolvedPath: defaultStorageConfig.defaultPath,
      availableSpaceBytes: defaultStorageConfig.availableSpaceBytes,
      requiredSpaceBytes: defaultStorageConfig.requiredSpaceBytes,
    };
  }

  async verifySymlink(): Promise<ChainStorageValidation> {
    const config = await this.getConfig();
    if (!config.customPath) {
      return {
        isValid: true,
        path: null,
      };
    }

    const configuredPath = config.customPath;

    try {
      const chainPathExists = await fs.pathExists(this._chainPath);
      if (!chainPathExists) {
        return {
          isValid: false,
          path: configuredPath,
          reason: 'path-not-found',
          message: 'Chain symlink is missing.',
        };
      }

      const chainStats = await fs.lstat(this._chainPath);
      if (!chainStats.isSymbolicLink()) {
        return {
          isValid: false,
          path: configuredPath,
          reason: 'unknown',
          message: 'Chain path is not a symlink.',
        };
      }

      const symlinkTarget = await fs.realpath(this._chainPath);
      const configuredResolved = await fs.realpath(configuredPath);
      if (path.resolve(symlinkTarget) !== path.resolve(configuredResolved)) {
        return {
          isValid: false,
          path: configuredPath,
          resolvedPath: symlinkTarget,
          reason: 'unknown',
          message: 'Chain symlink target does not match configured path.',
        };
      }

      return {
        isValid: true,
        path: configuredPath,
        resolvedPath: symlinkTarget,
      };
    } catch (error) {
      logger.warn('ChainStorageManager: verify symlink failed', {
        error,
        chainPath: this._chainPath,
      });
      return {
        isValid: false,
        path: configuredPath,
        reason: 'path-not-found',
        message: 'Configured chain storage target is unavailable.',
      };
    }
  }

  async resolveChainStoragePath(): Promise<string> {
    try {
      const chainPathExists = await fs.pathExists(this._chainPath);
      if (chainPathExists) {
        return await fs.realpath(this._chainPath);
      }
    } catch (error) {
      logger.warn('ChainStorageManager: failed to resolve chain path', {
        error,
        chainPath: this._chainPath,
      });
    }

    const config = await this.getConfig();
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

    return this._stateDirectoryPath;
  }

  async resolveMithrilWorkDir(): Promise<string> {
    const config = await this.getConfig();

    if (!config.customPath) {
      return this._stateDirectoryPath;
    }

    try {
      return await fs.realpath(config.customPath);
    } catch (error) {
      logger.warn('ChainStorageManager: failed to resolve Mithril work dir', {
        error,
        customPath: config.customPath,
      });
      return this._stateDirectoryPath;
    }
  }

  async migrateData(
    fromPath: string,
    toPath: string,
    options: {
      preserveSourceRoot?: boolean;
    } = {}
  ): Promise<void> {
    const source = path.resolve(fromPath);
    const target = path.resolve(toPath);
    const { preserveSourceRoot = false } = options;

    if (source === target) {
      return;
    }

    const sourceExists = await fs.pathExists(source);
    if (!sourceExists) {
      return;
    }

    const sourceStats = await fs.lstat(source);
    if (!sourceStats.isDirectory()) {
      throw new Error('Chain storage source must be a directory.');
    }

    await fs.ensureDir(target);
    const entries = await fs.readdir(source);

    for (const entry of entries) {
      const sourceEntry = path.join(source, entry);
      const targetEntry = path.join(target, entry);

      try {
        await fs.move(sourceEntry, targetEntry, { overwrite: true });
      } catch (error) {
        if ((error as NodeJS.ErrnoException)?.code !== 'EXDEV') {
          throw error;
        }

        // Cross-device moves are handled via copy + remove fallback.
        await fs.copy(sourceEntry, targetEntry, { overwrite: true });
        await fs.remove(sourceEntry);
      }
    }

    if (!preserveSourceRoot) {
      await fs.remove(source);
    }
  }

  async _getDefaultStorageConfig(): Promise<
    Pick<
      ChainStorageConfig,
      'defaultPath' | 'availableSpaceBytes' | 'requiredSpaceBytes'
    >
  > {
    const {
      exists: stateDirectoryExists,
      resolvedPath: resolvedStatePath,
    } = await this._resolveStateDirectoryPath();
    const diskCheckPath = stateDirectoryExists
      ? resolvedStatePath
      : path.dirname(resolvedStatePath);
    const { free } = await checkDiskSpace(diskCheckPath);

    return {
      defaultPath: path.join(resolvedStatePath, CHAIN_DIRECTORY_NAME),
      availableSpaceBytes: free,
      requiredSpaceBytes: DISK_SPACE_REQUIRED,
    };
  }

  async getConfig(): Promise<ChainStorageConfig> {
    try {
      const defaultStorageConfig = await this._getDefaultStorageConfig();
      const exists = await fs.pathExists(this._configPath);
      if (!exists) {
        return {
          customPath: null,
          ...defaultStorageConfig,
        };
      }

      const parsed = await fs.readJson(this._configPath);
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
        configPath: this._configPath,
      });

      try {
        const fallbackConfig = await this._getDefaultStorageConfig();
        return { customPath: null, ...fallbackConfig };
      } catch {
        return {
          customPath: null,
          defaultPath: this._chainPath,
          availableSpaceBytes: Number.NaN,
          requiredSpaceBytes: DISK_SPACE_REQUIRED,
        };
      }
    }
  }

  async validate(targetDir: string | null): Promise<ChainStorageValidation> {
    const normalizedPath =
      typeof targetDir === 'string' && targetDir.trim().length > 0
        ? targetDir.trim()
        : null;

    if (normalizedPath == null) {
      return {
        isValid: true,
        path: null,
      };
    }

    const defaultValidation: ChainStorageValidation = {
      isValid: false,
      path: normalizedPath,
    };

    try {
      if (this._isSamePath(normalizedPath, this._chainPath)) {
        const defaultStorageConfig = await this._getDefaultStorageConfig();

        return {
          isValid: true,
          path: null,
          resolvedPath: defaultStorageConfig.defaultPath,
          availableSpaceBytes: defaultStorageConfig.availableSpaceBytes,
          requiredSpaceBytes: defaultStorageConfig.requiredSpaceBytes,
        };
      }

      const exists = await fs.pathExists(normalizedPath);
      if (!exists) {
        return {
          ...defaultValidation,
          reason: 'path-not-found',
          message: 'Selected directory does not exist.',
        };
      }

      const resolvedPath = await fs.realpath(normalizedPath);
      const targetStats = await fs.stat(resolvedPath);
      if (!targetStats.isDirectory()) {
        return {
          ...defaultValidation,
          resolvedPath,
          reason: 'not-writable',
          message: 'Selected path must be a directory.',
        };
      }

      const {
        resolvedPath: statePath,
      } = await this._resolveStateDirectoryPath();
      if (this._isSubPath(resolvedPath, statePath)) {
        return {
          ...defaultValidation,
          resolvedPath,
          reason: 'inside-state-dir',
          message: 'Selected directory cannot be inside Daedalus state dir.',
        };
      }

      await fs.access(resolvedPath, fs.constants.W_OK);

      const { free } = await checkDiskSpace(resolvedPath);
      if (free < DISK_SPACE_REQUIRED) {
        return {
          ...defaultValidation,
          resolvedPath,
          availableSpaceBytes: free,
          requiredSpaceBytes: DISK_SPACE_REQUIRED,
          reason: 'insufficient-space',
          message: 'Selected directory does not have enough free space.',
        };
      }

      return {
        isValid: true,
        path: normalizedPath,
        resolvedPath,
        availableSpaceBytes: free,
        requiredSpaceBytes: DISK_SPACE_REQUIRED,
      };
    } catch (error) {
      logger.warn('ChainStorageManager: validation failed', {
        error,
        targetDir: normalizedPath,
      });
      return {
        ...defaultValidation,
        reason: 'unknown',
        message: 'Unable to validate selected directory.',
      };
    }
  }

  _isSubPath(targetPath: string, basePath: string): boolean {
    const normalizedTarget = path.resolve(targetPath);
    const normalizedBase = path.resolve(basePath);

    if (normalizedTarget === normalizedBase) {
      return true;
    }

    return normalizedTarget.startsWith(`${normalizedBase}${path.sep}`);
  }

  _isSamePath(firstPath: string, secondPath: string): boolean {
    const normalizedFirstPath = path.resolve(firstPath);
    const normalizedSecondPath = path.resolve(secondPath);

    if (process.platform === 'win32') {
      return (
        normalizedFirstPath.toLowerCase() === normalizedSecondPath.toLowerCase()
      );
    }

    return normalizedFirstPath === normalizedSecondPath;
  }

  async _resolveStateDirectoryPath(): Promise<ResolvedStateDirectory> {
    const exists = await fs.pathExists(this._stateDirectoryPath);

    return {
      exists,
      resolvedPath: exists
        ? await fs.realpath(this._stateDirectoryPath)
        : path.resolve(this._stateDirectoryPath),
    };
  }

  async _resolveCurrentChainSource(): Promise<string | null> {
    const chainPathExists = await fs.pathExists(this._chainPath);
    if (!chainPathExists) {
      return null;
    }

    const chainStats = await fs.lstat(this._chainPath);
    if (chainStats.isSymbolicLink()) {
      try {
        return await fs.realpath(this._chainPath);
      } catch (error) {
        logger.warn('ChainStorageManager: unable to resolve chain symlink', {
          error,
          chainPath: this._chainPath,
        });
        return null;
      }
    }

    if (chainStats.isDirectory()) {
      return this._chainPath;
    }

    return null;
  }

  async _captureChainPathState(): Promise<ChainPathState> {
    const exists = await fs.pathExists(this._chainPath);
    if (!exists) {
      return { type: 'missing' };
    }

    const stats = await fs.lstat(this._chainPath);
    if (stats.isSymbolicLink()) {
      try {
        return {
          type: 'symlink',
          resolvedPath: await fs.realpath(this._chainPath),
        };
      } catch (error) {
        logger.warn('ChainStorageManager: failed to snapshot symlink state', {
          error,
          chainPath: this._chainPath,
        });
        return { type: 'symlink' };
      }
    }

    if (stats.isDirectory()) {
      return {
        type: 'directory',
        resolvedPath: this._chainPath,
      };
    }

    return { type: 'missing' };
  }

  async _rollbackSetDirectory({
    previousState,
    previousConfig,
    targetPath,
  }: {
    previousState: ChainPathState;
    previousConfig: ChainStorageConfig;
    targetPath: string;
  }): Promise<void> {
    logger.warn('ChainStorageManager: rolling back setDirectory change', {
      chainPath: this._chainPath,
      targetPath,
      previousState: previousState.type,
    });

    try {
      switch (previousState.type) {
        case 'symlink': {
          const rollbackTarget = previousState.resolvedPath;
          if (!rollbackTarget) {
            break;
          }
          await fs.ensureDir(rollbackTarget);

          await fs.remove(this._chainPath);
          await fs.symlink(
            rollbackTarget,
            this._chainPath,
            process.platform === 'win32' ? 'junction' : 'dir'
          );
          break;
        }

        case 'directory': {
          await fs.remove(this._chainPath);
          await fs.ensureDir(this._chainPath);
          break;
        }

        default:
          await fs.remove(this._chainPath);
          await fs.ensureDir(this._chainPath);
      }

      if (previousConfig.customPath) {
        await fs.writeJson(
          this._configPath,
          {
            customPath: previousConfig.customPath,
            setAt: previousConfig.setAt,
          },
          { spaces: 2 }
        );
      } else {
        await fs.remove(this._configPath);
      }
    } catch (rollbackError) {
      logger.error('ChainStorageManager: rollback failed', {
        rollbackError,
        chainPath: this._chainPath,
      });
    }
  }
}
