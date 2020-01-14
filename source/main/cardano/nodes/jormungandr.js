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
  sig_key: kes25519-12-sk1qqqqqqyrh3kvlxhv6f9rnudputjmfjky82rtzknej9d7au75emh6fzrhhmyfhjuyhapmhqjftl8sgetfd9kqd3jmcr5nf83mt8j8kq3v3dx89er54g7kpvzg5yv7rthxpr0qe6ehlllz8yme4ss39yjr2u4jruasyxrjjpy7xj8yq8dyv6nrxr7a4qj04t73f6gsxnvt7lja75wnq9hzmq8ghc37kw4ztupsqazg7ge9q2hzwwwz6eswt2l5pp8c5c3avp3zfnjhhtdkcdl9dw6z86z4tlh8vzpw24sstu644cqg8gcd98upd87epzl84s05jlc3ja8fn5u8sdmm4zw60scmul54ge4f7j3ev23a4kfqrpy60hxu2hhg8kr55d3aaqmt8yur20qdwdqc2guws7twuxms48knpe5mrlyzuchnjwcc8jj30ckqgmzuaxgj9eswplpheuzwx7gmrleyftxnctz2fcgywzwwvyxa5c89jdexn8ryx099xgxepznjfpjrjm2xz24m7pd9p3xqnkxev7g8ge3xsek4fnqrmhyz5wpt3amgujcm9l7gyr820axwkyaujftwwasduxhxem3wf3lfn03jzl6e9ktsgvkxxj43ry6gaew9uu6axmlx2ngss6gwzucv9tgv57fd7zg05v55mr7pdrdgnl7kwfkjzx4mxfc53ej0h5ady6ku4509aa4z7y8pehlun487ywy38v63pvy36za7v54nehl640qm8xp49qfmzlc65f9ra648lc5xmy7js9497cf0q5u5xzcssm69u6nkerm0rg2fw2nfh2ledkhnrar3amqsnnktcrswtyct4s6zspyxnq5v27wz2tuakmxpclueg6ugg4c802esegfqxkjmj77cg6w06gkvuk9c5t8hm8fsuqemmvft4kyzjmeww95g3uu5fjh3fk2yn034k8r0r2mjqqs44sdsymwjf063qj64fr5kvh9puaa45pylev2ar2xae7dr93w2d44xqwec4ljmmaux684jflvulz43fy8e4mcft5fs2kz502jmerfgt93xmd03fwe8nnkk8c4darl07ej8qmejlme5lf838me0m65r7w3dsvw3d6c8u4npmfqrfj4skhwhed52qwayrlz6lcmg6x72zdf0rt7jfp9a45xly6k5thp6yc5q6l22n9dq446ur670p9pfpdhjmet782dnfqf0prvu3rsv3x7tsjl58wuzf90u7pr9d95kcpkxt0qwjdy78dv7g7cz9j95cu4jf8utf3ynksynxg8347rwjjhxyu0g77t65clr424cr68vu8lzlq5ur0563kaq038ra5dt2m58wcjsz3hntrh3masg03jtyhantxlzvs05axjuryu76h0m6rmyvkkywd7mzgp42nmwnr20fhdhwzzhupllep3chsvvyvsdkntqtc69ytq7jkgaznc200wke8tukpksuaa9u0kew8r4zllt7zqp445wnke583ssd653seht5mkwmc2pr7jv0ayl37g9j52x2zxj6quxf56mgpw5pew075faqgg5egzmwp8uetzgw4a0r3c4hk2q6eff33gz7v4u8yp0rukyahg0ur8hr9jjefckc95klpq98zs0qpwr5m6jraeg56sm8pf3zn5n68tphdtzfpyl8lnr7wlwypphkgrqfnncjk7ajdudfy8rp2qzyrmudh6xgz3js00uh0q2rxkdf26engu28vmwwp6vvz25wnp7fqmfwx8a8jhzmc7muvq2lug59lmyhkpxja4z427eczmxkmkkj40yce99a2nvdnuwytqa5d943cgnt0h3yst5n8dlsethtsp8h8yr0tz4jd74xxs4vf335u0dkkg45j3n5jmsy5kdlnkg5heu3mx78dyvshcl37rll4yqhd56eeuac6n3j35dxc5tkk07se
  vrf_key: vrf_sk1h27uuye0vt3v4c793k0hn5rpyd9gpqlanw9xzwf38uspf9a53urqa34ngh
  node_id: 712dd028687560506e3b0b3874adbd929ab892591bfdee1221b5ee3796b79b70
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
