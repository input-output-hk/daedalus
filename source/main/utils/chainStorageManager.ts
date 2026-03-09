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
    if (targetDir == null || targetDir.trim().length === 0) {
      return this.resetToDefault();
    }

    const validation = await this.validate(targetDir);
    if (!validation.isValid || validation.path == null) {
      return validation;
    }

    const resolvedTargetPath = validation.resolvedPath
      ? validation.resolvedPath
      : await fs.realpath(validation.path);

    const previousState = await this._captureChainPathState();
    const previousConfig = await this.getConfig();

    const sourcePath = await this._resolveCurrentChainSource();
    try {
      if (
        sourcePath &&
        path.resolve(sourcePath) !== path.resolve(resolvedTargetPath)
      ) {
        await this.migrateData(sourcePath, resolvedTargetPath);
      }

      await fs.remove(this._chainPath);
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
    let sourcePath: string | null = null;

    if (chainPathExists) {
      const chainStats = await fs.lstat(this._chainPath);
      if (chainStats.isSymbolicLink()) {
        try {
          sourcePath = await fs.realpath(this._chainPath);
        } catch (error) {
          logger.warn('ChainStorageManager: unable to resolve chain symlink', {
            error,
            chainPath: this._chainPath,
          });
        }
        await fs.remove(this._chainPath);
      }
    }

    await fs.ensureDir(this._chainPath);

    if (
      sourcePath &&
      path.resolve(sourcePath) !== path.resolve(this._chainPath)
    ) {
      await this.migrateData(sourcePath, this._chainPath);
    }

    await fs.remove(this._configPath);

    return {
      isValid: true,
      path: null,
      resolvedPath: this._chainPath,
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

  async migrateData(fromPath: string, toPath: string): Promise<void> {
    const source = path.resolve(fromPath);
    const target = path.resolve(toPath);

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

    await fs.remove(source);
  }

  async getConfig(): Promise<ChainStorageConfig> {
    try {
      const exists = await fs.pathExists(this._configPath);
      if (!exists) {
        return { customPath: null };
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
        setAt,
      };
    } catch (error) {
      logger.warn('ChainStorageManager: failed to read storage config', {
        error,
        configPath: this._configPath,
      });
      return { customPath: null };
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

      const statePath = await fs.realpath(this._stateDirectoryPath);
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

          if (path.resolve(targetPath) !== path.resolve(rollbackTarget)) {
            await this.migrateData(targetPath, rollbackTarget);
          }

          await fs.remove(this._chainPath);
          await fs.symlink(
            rollbackTarget,
            this._chainPath,
            process.platform === 'win32' ? 'junction' : 'dir'
          );
          break;
        }

        case 'directory': {
          await fs.ensureDir(this._chainPath);
          if (path.resolve(targetPath) !== path.resolve(this._chainPath)) {
            await this.migrateData(targetPath, this._chainPath);
          }
          break;
        }

        default:
          await fs.ensureDir(this._chainPath);
          if (path.resolve(targetPath) !== path.resolve(this._chainPath)) {
            await this.migrateData(targetPath, this._chainPath);
          }
      }

      if (previousConfig.customPath) {
        await fs.writeJson(this._configPath, previousConfig, { spaces: 2 });
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
