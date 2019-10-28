// @flow
import * as fs from 'fs-extra';
import { exec } from 'child_process';
import type { WalletOpts } from '../CardanoWalletLauncher';

export async function configureJormungandrDeps(
  cliPath: string,
  stateDir: string
) {
  const secretPath = `${stateDir}/secret.yaml`;
  const genesisPath = `${stateDir}/genesis.yaml`;
  const block0Path = `${stateDir}/block0.bin`;

  const secretFileExists = await fs.pathExists(secretPath);
  if (secretFileExists) {
    return;
  }

  const secret = await createAndWriteClientSecret(cliPath, secretPath);
  await createBlock0({ cliPath, genesisPath, block0Path, secret });
}

export async function createAndWriteClientSecret(
  cliPath: string,
  secretPath: string
): Promise<string> {
  const secret: string = await new Promise((resolve, reject) => {
    exec(`${cliPath} key generate --type=Ed25519`, (err, stdout, stderr) => {
      if (err || stderr) {
        return err ? reject(err) : reject(stderr);
      }

      return resolve(stdout);
    });
  });

  const secretFileContents = `bft:
  signing_key: ${secret}
`;

  await fs.writeFile(secretPath, secretFileContents);
  return secret;
}

export async function createBlock0({
  cliPath,
  genesisPath,
  block0Path,
  secret,
}: {
  cliPath: string,
  genesisPath: string,
  block0Path: string,
  secret: string,
}) {
  // The block0 we create here is only used for the selfnode
  const genesisDefaultPath = `utils/jormungandr/selfnode/genesis.yaml`;
  const networkGenesisFileExists = await fs.pathExists(genesisDefaultPath);
  if (!networkGenesisFileExists) {
    throw new Error(`No genesis file exists for testnet`);
  }

  const publicKey = await new Promise((resolve, reject) => {
    exec(
      `echo "${secret}" | ${cliPath} key to-public`,
      (err, stdout, stderr) => {
        if (err || stderr) {
          return err ? reject(err) : reject(stderr);
        }

        return resolve(stdout.split('\n')[0]);
      }
    );
  });

  const genesisFile = (await fs.readFile(genesisDefaultPath)).toString('utf8');
  const KEY_PLACEHOLDER = '!!CONSENSUS_ID_OVERRIDE!!';
  const [pre, post] = genesisFile.split(KEY_PLACEHOLDER);
  const genesisFileWithLeaderKey = [pre, publicKey, post].join('');
  await fs.writeFile(genesisPath, genesisFileWithLeaderKey);

  const pathEscaper = str => str.replace(/(\s+)/g, '\\$1');
  await new Promise((resolve, reject) => {
    const inputPath = pathEscaper(genesisPath);
    const outputPath = pathEscaper(block0Path);

    exec(
      `${cliPath} genesis encode --input ${inputPath} --output ${outputPath}`,
      (err, stdout, stderr) => {
        if (err || stderr) {
          return err ? reject(err) : reject(stderr);
        }

        return resolve(stdout);
      }
    );
  });
}
