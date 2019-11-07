// @flow
import * as fs from 'fs-extra';
import { spawn } from 'child_process';
import { join, normalize } from 'path';
import { Logger } from '../../utils/logging';

export async function configureJormungandrDeps(
  cliBin: string,
  stateDir: string
) {
  const secretPath = join(stateDir, 'secret.yaml');
  const genesisPath = join(stateDir, 'genesis.yaml');
  const block0Path = join(stateDir, 'block0.bin');

  // block0 is the last file created, so if it does exist, nothing
  // else is required
  const block0Exists = await fs.pathExists(block0Path);
  if (block0Exists) {
    Logger.info('Block0 exists. Moving on.', {});
    return;
  }

  await fs.remove(secretPath);
  await fs.remove(genesisPath);
  await fs.remove(block0Path);

  const secret = await createAndWriteClientSecret(cliBin, secretPath);
  await createBlock0({ cliBin, genesisPath, block0Path, secret });
}

export async function createAndWriteClientSecret(
  cliBin: string,
  secretPath: string
): Promise<string> {
  Logger.info('Creating client secret for selfnode', {});
  const secret: string = await new Promise((resolve, reject) => {
    const result = spawn(cliBin, ['key', 'generate', '--type=Ed25519']);
    let buffer = '';
    result.stdout.setEncoding('utf8');
    result.stdout.on('data', data => {
      buffer += data;
    });
    result.on('exit', code => {
      if (code === 0) {
        resolve(buffer);
      } else {
        reject(new Error('failed to run jcli'));
      }
    });
  });

  const secretFileContents = `bft:
  signing_key: ${secret}
`;

  await fs.writeFile(secretPath, secretFileContents);
  return secret;
}

export async function createBlock0({
  cliBin,
  genesisPath,
  block0Path,
  secret,
}: {
  cliBin: string,
  genesisPath: string,
  block0Path: string,
  secret: string,
}) {
  const installDir = process.env.DAEDALUS_INSTALL_DIRECTORY || '';
  const genesisDefaultPath = 'utils/jormungandr/selfnode/genesis.yaml';
  const genesisInstalledPath = join(installDir, 'genesis.yaml');

  // dev path is ./
  const genesisYamlPath =
    installDir.length > 2 ? genesisInstalledPath : genesisDefaultPath;

  Logger.info('Genesis file path', { genesisYamlPath });

  const networkGenesisFileExists = await fs.pathExists(genesisYamlPath);
  if (!networkGenesisFileExists) {
    throw new Error(`No genesis file exists for testnet`);
  }

  const publicKey = await new Promise((resolve, reject) => {
    const result = spawn(cliBin, ['key', 'to-public'], {
      stdio: ['pipe', 'pipe', 'inherit'],
    });
    result.stdin.write(secret);
    result.stdin.end();

    let buffer = '';
    result.stdout.setEncoding('utf8');
    result.stdout.on('data', data => {
      buffer += data;
    });
    result.on('exit', code => {
      if (code === 0) {
        resolve(buffer.split('\n')[0]);
      } else {
        reject(new Error('failed to run jcli key to-public'));
      }
    });
  });

  Logger.info('Selfnode public key', { publicKey });
  const genesisFile = (await fs.readFile(genesisYamlPath)).toString('utf8');
  const KEY_PLACEHOLDER = '!!CONSENSUS_ID_OVERRIDE!!';
  const [pre, post] = genesisFile.split(KEY_PLACEHOLDER);
  const genesisFileWithLeaderKey = [pre, publicKey, post].join('');
  await fs.writeFile(genesisPath, genesisFileWithLeaderKey);

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
