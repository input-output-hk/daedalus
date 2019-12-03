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
  const block0Path = join(stateDir, 'block0.bin');

  // block0 is the last file created, so if it does exist, nothing
  // else is required
  const block0Exists = await fs.pathExists(block0Path);
  if (block0Exists) {
    Logger.info('Block0 exists. Moving on.', {});
    return;
  }

  await fs.remove(secretPath);
  await fs.remove(block0Path);

  await createAndWriteClientSecret(secretPath);
  await createBlock0({ cliBin, block0Path });
}

export async function createAndWriteClientSecret(
  secretPath: string
): Promise<void> {
  Logger.info('Creating client secret for selfnode', {});
  const secretFileContents = `genesis:
  sig_key: kes25519-12-sk1qqqqqqy2e9k99shyezvxv7kgsj0q7t9wglhvdv7l670zxm0x5ws0axu6nkp9e7ssz7xxpsf9zgfrj8qlyndzc0sj740c3dnk5a6xkhj38h6ytqlgun34f4q4wshm6qde93cqtxwwpkt46qf39k7yh00qpt7r206gd85dha8l89v0ml3sgner4tm7950krct7rr64ktm4dfxj7cp0heneq0vf5rxz6jezqxc9q7c9uu9ff8fe7dd8w4svecdq6kcqjvjzynvnq33zac8kgs2pdmgjr5h422790ssax2l7et4rdmuhctc8gny3sv7w6zacs2a9ygfncrc8fnvsg2fnxtj0ufllxt8zllgvmh8nny2hhnp8zwsnp3z65vhlnky04mmav3wz6yku7jxlrv8d3ng8847p4v9t0e4eyual53zm53q4v5lgs3p3pvug8dz8ead5tu44re5l7gll2jw9cl0wezxmukx6mvw0f66qgtpcngwdlh9q0pkpfq2vs8whcffwa6j4np82zd6aesc6yfgyanqtagnp3xv6gaggpe34fyve8eltz73x0aq58n3vsnplsagfucaltg0ah6vkdcrzjfau54ynen2d98gm2v6rpj80t94wgufwaaxnuhuh0mj9qhmmrfw320l7v7p36nnlxzqwz8temvlu07zgyxv3hn0y6svd382z7mwsjyk36vyge2k2fw9lfmjmwnpthat3au6ef099n9llw00unn86aewkh3lre846jc84tc4zcwdj76gtjwnrhe2r30gtek2m07s2m0v0343leq6cdp5a4486rnrhl9y8hsaj9rp3cyzad5se60ypuj49a99z4gf56e2exkxfewnyu2hvwmpa32hfq5ck7xxy39t74k78vt39nzd7m94cpulucnjl63kdngpdmczgp4wzs2p7cnr6wedwtgl222qjetkw9zh6um54eud3axp2hdpe07cus58ezaejp8runcvdv55wkazh0yrh6fgg239yl5e2fy4ppknxl48g4xcljcscsevp7pfm75x6wlvux6em45qs8wq3cntgt8u8tuhjlzxx9pqx3jhznnkaj754wc88r6wwlwj9nmhn272x4v4289gvtrn5gp6sf0vawdjdk4yt207ev2jmx4048zwz2nt4wacdwsps8yhty794487msvvafzx5zlxmg2fun4fn88sp4wjw22rcggfyzw0qjth5s6mgyh86zqtcccxpy5fpywgurujd5tp7zt64lz9kw6nhg6672y7lg32lwmtgevfqp5x676576f0r6zutp6x40sdefrrsshqvtysqy9dvwv86cav7htr0s84tjzpkc0x5vx27rgpvref99q6qaglutyjkm4k8ccww7a58y73neg4gc6jzptw4lps8adau4kaxtxyya352qztcluaglllv46zsfp3cdyx0l0vud066l45ckamq60nl64nguseza9hw8ku24wtq7czjp4nrggkze58al04kmc9x5mhjexxwr5ge59qn47xfeg6sdk7qphx3r520y4wgtn4vursewq7qga0zp6qw07udpc526crj3rk6me9nsuc2lt4xdrm4t0wfxqdjp4m86vqyxv0v855a98m6e03v2k7mvruq6melkmsncjcp48rqa399m3y84xdu65ms06nda6ycceqrkzghadegtx6xxha93jn2dcravw5d678htcc85qf3uetulw55u0eclk7eyxmhfjtz3qw3nxg0vxe7x6pfm540l6e0gt84r28chfurh6jr4yt43h2hxzr2z7xry849t3m9pufj9jcd3ltj60w5xcs2vrka79srdtx538e86pd83ljpg0v7az78qum83sz5c0r05fw5xp33v0vu4xamack4p9r8zzxxyznthlurfz7qyqp23nm94ecjyefm698d322ht3glwm
  vrf_key: vrf_sk19xmdk588rc4p5aa454cm4lqrm3s33njfsfm2rkecazmf63weg5zqfnstvv
  node_id: 4f8d686a02c6e625b5a59cc9e234f32e5d72987012f9c25c9a6b60ddade197d1
bft:
  signing_key: ed25519_sk1gg2pjtmldtsukmwgnqjjjdhlty2vxd7lyxy4rp5sz5m60pq7gkdsjvm4j4
`;
  await fs.writeFile(secretPath, secretFileContents);
}

export async function createBlock0({
  cliBin,
  block0Path,
}: {
  cliBin: string,
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
    throw new Error('No genesis file exists for testnet');
  }

  await new Promise((resolve, reject) => {
    const inputPath = normalize(genesisYamlPath);
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
