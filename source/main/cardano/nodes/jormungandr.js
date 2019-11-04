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
        'ae57995b8fe086ba590c36dc930f2aa9b52b2ffa92c0698fff2347adafe8dc65',
        '--',
        '--trusted-peer',
        '/ip4/13.230.137.72/tcp/3000@ed25519_pk1w6f2sclsauhfd6r9ydgvn0yvpvg4p3x3u2m2n7thknwghrfpdu5sgvrql9',
        '--trusted-peer',
        '/ip4/13.230.48.191/tcp/3000@ed25519_pk1lzrdh0pcmhwcnqdl5cgcu7n0c76pm7g7p6pdey7wup54vz32gy6qlz5vnq',
        '--trusted-peer',
        '/ip4/18.196.168.220/tcp/3000@ed25519_pk1uufkgu0t9xm8ry04wnddtnku5gjg8typf5z6ehh65uc6nz4j8n4spq0xrl',
        '--trusted-peer',
        '/ip4/3.124.132.123/tcp/3000@ed25519_pk14tqkqnz3eydn0c8c8gmmyzxgnf2dztpy5dnrx09mhfzv0dh93s3qszqgpc',
        '--trusted-peer',
        '/ip4/18.184.181.30/tcp/3000@ed25519_pk178ge2jn6c40vgmrewgmg26nmtda47nk2jncukzj327mp3a9g2qzss2d44f',
        '--trusted-peer',
        '/ip4/184.169.162.15/tcp/3000@ed25519_pk1nk0ne8ez66w5tp2g8ctcakthjpz89eveyg0egcpylenhet83n0sq2jqz8q',
        '--trusted-peer',
        '/ip4/13.56.87.134/tcp/3000@ed25519_pk1ce450zrtn04eaevcn9csz0thpjuhxrysdrq6qlr9pq7e0wd842nsxy6r5k',
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
  cliBin: string,
  stateDir: string
) {
  const secretPath = `${stateDir}/secret.yaml`;
  const genesisPath = `${stateDir}/genesis.yaml`;
  const block0Path = `${stateDir}/block0.bin`;

  const secretFileExists = await fs.pathExists(secretPath);
  if (secretFileExists) {
    return;
  }

  const secret = await createAndWriteClientSecret(cliBin, secretPath);
  await createBlock0({ cliBin, genesisPath, block0Path, secret });
}

export async function createAndWriteClientSecret(
  cliBin: string,
  secretPath: string
): Promise<string> {
  const secret: string = await new Promise((resolve, reject) => {
    exec(`${cliBin} key generate --type=Ed25519`, (err, stdout, stderr) => {
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
  // The block0 we create here is only used for the selfnode
  const genesisDefaultPath = `utils/jormungandr/selfnode/genesis.yaml`;
  const networkGenesisFileExists = await fs.pathExists(genesisDefaultPath);
  if (!networkGenesisFileExists) {
    throw new Error(`No genesis file exists for testnet`);
  }

  const publicKey = await new Promise((resolve, reject) => {
    exec(
      `echo "${secret}" | ${cliBin} key to-public`,
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
      `${cliBin} genesis encode --input ${inputPath} --output ${outputPath}`,
      (err, stdout, stderr) => {
        if (err || stderr) {
          return err ? reject(err) : reject(stderr);
        }

        return resolve(stdout);
      }
    );
  });
}
