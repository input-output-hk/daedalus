// @flow
import * as fs from 'fs-extra';
import { spawn } from 'child_process';
import { join, normalize } from 'path';
import { Logger } from '../../utils/logging';

export function configureJormungandrDeps() {
  // block0.bin is now part of the installer on selfnode
}

export async function createBlock0({
  cliBin,
  block0Path,
  genesisInstalledPath,
}: {
  cliBin: string,
  block0Path: string,
  genesisInstalledPath: string,
}) {
  const genesisDevPath = 'utils/jormungandr/selfnode/genesis.yaml';

  // dev path is ./
  const genesisPath =
    genesisInstalledPath.length > 2 ? genesisInstalledPath : genesisDevPath;

  Logger.info('Genesis file path', { genesisPath });

  const genesisFileExists = await fs.pathExists(genesisPath);
  if (!genesisFileExists) {
    throw new Error('Genesis file not found');
  }

  await new Promise((resolve, reject) => {
    const inputPath = normalize(genesisPath);
    const outputPath = normalize(block0Path);

    Logger.info('block0 creation params', { inputPath, outputPath });

    const result = spawn(cliBin, [
      'genesis',
      'encode',
      '--input',
      inputPath,
      '--output',
      outputPath,
    ]);

    result.on('exit', code => {
      if (code === 0) {
        resolve();
      } else {
        reject();
      }
    });
  });
}

export async function createConfig(configOutputPath: string): Promise<void> {
  const installDir = process.env.DAEDALUS_INSTALL_DIRECTORY || '';
  const configDevPath = 'utils/jormungandr/selfnode/config.yaml';
  const configInstalledPath = join(installDir, 'config.yaml');

  // dev path is ./
  const configInputPath =
    installDir.length > 2 ? configInstalledPath : configDevPath;

  Logger.info('Config file input path', { configInputPath });

  const configFileExists = await fs.pathExists(configInputPath);
  if (!configFileExists) {
    throw new Error('Config input file not found');
  }

  Logger.info('Creating config file for selfnode', { configOutputPath });
  await fs.copyFile(configInputPath, configOutputPath);
}

export async function createSecret(secretOutputPath: string): Promise<void> {
  const installDir = process.env.DAEDALUS_INSTALL_DIRECTORY || '';
  const secretDevPath = 'utils/jormungandr/selfnode/secret.yaml';
  const secretInstalledPath = join(installDir, 'secret.yaml');

  // dev path is ./
  const secretInputPath =
    installDir.length > 2 ? secretInstalledPath : secretDevPath;

  Logger.info('Secret file path', { secretInputPath });

  const secretInputFileExists = await fs.pathExists(secretInputPath);
  if (!secretInputFileExists) {
    throw new Error('Secret input file not found');
  }

  Logger.info('Creating secret file for selfnode', { secretOutputPath });
  await fs.copyFile(secretInputPath, secretOutputPath);
}
