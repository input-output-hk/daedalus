// @flow
import * as fs from 'fs-extra';
import { exec } from 'child_process';
import type { WalletOpts } from '../CardanoWalletLauncher';

export function buildJormungandrNodeOpts(
  { nodePort, stateDir }: WalletOpts,
  isJormungandrTestnet: boolean
) {
  const baseArgs = [
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
  ];

  return isJormungandrTestnet
    ? [
        ...baseArgs,
        '--genesis-block-hash',
        'adbdd5ede31637f6c9bad5c271eec0bc3d0cb9efb86a5b913bb55cba549d0770',
        '--',
        '--trusted-peer',
        '/ip4/3.115.194.22/tcp/3000@ed25519_pk1npsal4j9p9nlfs0fsmfjyga9uqk5gcslyuvxy6pexxr0j34j83rsf98wl2',
        '--trusted-peer',
        '/ip4/13.113.10.64/tcp/3000@ed25519_pk16pw2st5wgx4558c6temj8tzv0pqc37qqjpy53fstdyzwxaypveys3qcpfl',
        '--trusted-peer',
        '/ip4/52.57.214.174/tcp/3000@ed25519_pk1v4cj0edgmp8f2m5gex85jglrs2ruvu4z7xgy8fvhr0ma2lmyhtyszxtejz',
        '--trusted-peer',
        '/ip4/3.120.96.93/tcp/3000@ed25519_pk10gmg0zkxpuzkghxc39n3a646pdru6xc24rch987cgw7zq5pmytmszjdmvh',
        '--trusted-peer',
        '/ip4/52.28.134.8/tcp/3000@ed25519_pk1unu66eej6h6uxv4j4e9crfarnm6jknmtx9eknvq5vzsqpq6a9vxqr78xrw',
        '--trusted-peer',
        '/ip4/13.52.208.132/tcp/3000@ed25519_pk15ppd5xlg6tylamskqkxh4rzum26w9acph8gzg86w4dd9a88qpjms26g5q9',
        '--trusted-peer',
        '/ip4/54.153.19.202/tcp/3000@ed25519_pk1j9nj2u0amlg28k27pw24hre0vtyp3ge0xhq6h9mxwqeur48u463s0crpfk',
      ]
    : [
        ...baseArgs,
        '--genesis-block',
        `${stateDir}/block0.bin`,
        '--',
        '--secret',
        `${stateDir}/secret.yaml`,
      ];
}

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
