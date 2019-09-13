// @flow
import * as fs from 'fs-extra';
import { exec } from 'child_process';
import type { WalletOpts } from '../CardanoWalletLauncher';

export function buildJormungandrNodeOpts({
  nodePort,
  stateDir,
  networkMode,
}: WalletOpts) {
  const network = networkMode === 'local' ? 'testnet' : networkMode;

  return [
    'launch',
    '--node-port',
    String(nodePort),
    '--state-dir',
    stateDir,
    // NOTE: --random-port is the value we will use
    // in production. For early development (and to enable the seed script)
    // we will fix the port
    // '--random-port',
    '--port',
    '8088',
    '--genesis-block',
    `utils/jormungandr/${network}/block0.bin`,
    '--bft-leaders',
    `${stateDir}/secret.yaml`,
  ];
}

export async function createClientSecret(cliPath: string, secretPath: string) {
  const secretFileExists = await fs.pathExists(secretPath);
  if (secretFileExists) {
    return;
  }

  const secret = await new Promise((resolve, reject) => {
    exec(`${cliPath} key generate --type=Ed25519`, (err, stdout, stderr) => {
      if (err || stderr) {
        return err ? reject(err) : reject(stderr);
      }

      return resolve(stdout);
    });
  });

  const secretFileContents = `
bft:
  signing_key: ${secret}
  `;

  await fs.writeFile(secretPath, secretFileContents);
}
