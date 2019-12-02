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

  await createAndWriteClientSecret(secretPath);
  await createBlock0({ cliBin, genesisPath, block0Path });
}

export async function createAndWriteClientSecret(
  secretPath: string
): Promise<()> {
  Logger.info('Creating client secret for selfnode', {});
  const secretFileContents = `genesis:
  sig_key: kes25519-12-sk1qqqqqqp2cfay5eyfggmd83f7mwehaq9pjlkx3t89vylnygj2w3s2p8x5jhfh7ukqywgz0tl4ke95aaekd8z9sc75tvqc0929w5pn3k8nyak26erpxkflp59ph9sg6krxrtmlfdfkdwgc6l682vgge7mcal85j7su09ktrqte0t3t7gywexqktru32z3dd23f6cqkt45akjjyl07sjn2pm0hysdk9a6ex9qvv77880dutzudn3n62t5u9p3vhw6n3td6ammcssnez00a8347n3yp73n9xemcekfr0sy5y4ukd6nruveqqg3u5egepfr0k93vn3yd2g3rzpd5yd938aq4qfx2j5jp3afgwqg6c9mhkatnz6zydnmp56q62enpq79px6cg4udftzkhtjwhjmq4w0s7ke8gghxenfmq2lyjsfd0m567kvnfz8krsp8k8astzxgr9wlpqk7lx3xr6m63fvc08vhsjxh9r38dzm2zn5lkezw9fscdtpg5nvxrk7namh780zfuz8k5k86ghhmtyypmvzcsljq63qkl4pn57lcj798qt468qnyln88y6s949rdksm5tx85fsujujvjcvfnktfmex7fcpl33shktq6ffh74ycemc5k7m7s3uwngs9vtpw0qku387fdv6xf3yax4v3gt799trwk0su9c6cs80udk95a6x4m72mnxzs7ye4rvlzd6fskfdwcm4d8l0w6e072uxe9tws64s0n6hhqrgs5u9gftcyp9404ps099etgr4lh24kjap7een4taemudnp8glmpsmnppw5fahrdt52fggjmkeyz8zl0q980urr62fk7hsz4anvvyz8q667m3deulkz8yhnzl6jze93hfy35m7telmsyjlg9q2n596uvhkusa7cv4eg6y9zhxqs962fqdsv4r6uae0myhx2uaztstmqla3h45qfaaat7ydjrsau05xawg0mfdf88xsdftklt7h8hxlztw532uys7ktvy4uu5vs9djl4vwlt29easrzm5qv053tdllmp96hnhwh0xt0n3tkrqmunuvrnspup03mp4qdhlw8qwvax8wzhr50slfdfeavreh343k49vsnkwu0fd3aarjxpz6594a3rvw7dh37al9cqyjh5lee8xk63thyxh45w7px8f9jcyaknav5tcmwav06epnvef4a7zctz4tsh7fulu66sg5valemkqkrx65uy257axlmjcq3eqfa07kmyknhhxe5ugkrr63dsrpu4g46sxwxc7vnketglkypc0zx7lhsqsd0hxa2jfxmlvxqcaudrnqpzsqk0g65s5k29pxvjpt3agxzqz8p8muz6gz02rkkxrq7ll5asqan4d5hqqkpwl63zr7qe86g5fxx06p0rf3xyn9gjf62wztaznwg53cajx2ujed388y663e7jd6gdjfnzmqqpqpm5xf2g5dqez6qxgtmak2fn37y8a99dvwekcstvvqxycgye2pv26q9x6glmk06zqhl95nz83m70ymvd8lkr9djxcc7vst65d0ywxvt9r0cgmneg67f4k6vy8r28tskylc2d86pfcz3jfj4r7tdx9kcw5mffc70f7st07ha0kdm72j2art92tvwkfqkkdlap0guls7m43y24jtnc25awx9dtxewhm8u77h7dvjx4w236k8kpm3xjrcg2smfadwkhyxs536csxchcmzhd62suysg4t8e9vave5pxvr55el3un09dwgdy7hp3lkx6f4mjmp4a5tk5rxm9580pvhl2cvg94g97zgaytwy46zt70ymqtrp860eaxffj9pefjtly4lm4ksaqd5qdzj02espx7zg2gy87ga967frxt6fckxa0h0vd4nc32vhn0jhzje9geyxy8f4zhwjanawujjfr0a2l5uzcz2u3nfmcmz6fzt5u3wm79q0rzx6
  vrf_key: vrf_sk1tffpnzfeyz9m5c84m9dlrc9gqttz2e6fnp5zjtv7j76xefz7pvzsvqs2yl
  node_id: a09ae0da1e618eeb09e7b78d73e265af18f87d4d5320386ebf0235f54ecd0347
bft:
  signing_key: ed25519_sk18nwpteu09tvwn7nwppvh83u7ruk9sw92vhdl5dc5kz79vtg0u78qhd4yey
`;

  await fs.writeFile(secretPath, secretFileContents);
}

export async function createBlock0({
  cliBin,
  genesisPath,
  block0Path,
}: {
  cliBin: string,
  genesisPath: string,
  block0Path: string,
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
