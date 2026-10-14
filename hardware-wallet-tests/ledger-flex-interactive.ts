import { spawn } from 'child_process';
import { createInterface, Interface } from 'readline';
import { stdin, stdout } from 'process';
import TransportNodeHid, {
  getDevices,
} from '@ledgerhq/hw-transport-node-hid-noevents';
import { identifyUSBProductId } from '@ledgerhq/devices';
import AppAda, {
  AddressType,
} from '@cardano-foundation/ledgerjs-hw-app-cardano';

const PAYMENT_PATH = [0x8000073c, 0x80000717, 0x80000000, 0, 0];
const STAKE_PATH = [0x8000073c, 0x80000717, 0x80000000, 2, 0];

const errorDetails = (error: unknown) => {
  if (!error || typeof error !== 'object') return { error: String(error) };
  const value = error as Record<string, unknown>;
  return {
    name: value.name,
    message: value.message,
    id: value.id,
    code: value.code,
    statusCode: value.statusCode,
  };
};

const question = (input: Interface, prompt: string) =>
  new Promise<void>((resolve) => input.question(prompt, () => resolve()));

const runTransactionProbe = () =>
  new Promise<void>((resolve, reject) => {
    const child = spawn(
      process.execPath,
      [
        '-r',
        '@swc-node/register',
        'hardware-wallet-tests/task607-ledger.ts',
        'sign-tx',
        '--operator',
        'interactive-flex-x2',
        '--target',
        'flex-app7',
      ],
      { stdio: 'inherit' }
    );
    child.once('error', reject);
    child.once('exit', (code) => {
      if (code === 0) resolve();
      else reject(new Error(`Transaction probe exited with status ${code}`));
    });
  });

const run = async (): Promise<void> => {
  const devices = getDevices().filter(
    (device) => identifyUSBProductId(device.productId)?.id === 'europa'
  );
  if (devices.length !== 1)
    throw new Error(`Expected one Ledger Flex, found ${devices.length}`);

  const input = createInterface({ input: stdin, output: stdout });
  stdout.write(`Ledger Flex found at ${devices[0].path}\n`);
  let transport: TransportNodeHid | undefined;
  try {
    await question(
      input,
      'Close Daedalus, unlock the Ledger, open Cardano, then press Enter: '
    );

    transport = await TransportNodeHid.open(devices[0].path);
    const app = new AppAda(transport);
    const { version } = await app.getVersion();
    stdout.write(
      `PASS open/getVersion: Cardano ${version.major}.${version.minor}.${version.patch}\n`
    );
    await question(
      input,
      'Press Enter, then approve public-key export on the Ledger: '
    );
    const key = await app.getExtendedPublicKey({ path: PAYMENT_PATH });
    if (!/^[0-9a-f]{64}$/u.test(key.publicKeyHex))
      throw new Error('Ledger returned an invalid public key');
    stdout.write('PASS getExtendedPublicKey\n');

    await question(
      input,
      'Press Enter, then verify and approve the address on the Ledger: '
    );
    await app.showAddress({
      network: { networkId: 1, protocolMagic: 764824073 },
      address: {
        type: AddressType.BASE_PAYMENT_KEY_STAKE_KEY,
        params: {
          spendingPath: PAYMENT_PATH,
          stakingPath: STAKE_PATH,
        },
      },
    });
    stdout.write('PASS showAddress\n');
  } finally {
    input.close();
    if (transport) await transport.close().catch(() => undefined);
  }

  stdout.write(
    'Starting an isolated transaction-signing probe. Approve the transaction on the Ledger.\n'
  );
  await runTransactionProbe();
  stdout.write('PASS signTransaction\nALL LEDGER FLEX CHECKS PASSED\n');
};

run().catch((error) => {
  process.stderr.write(
    `LEDGER FLEX CHECK FAILED ${JSON.stringify(errorDetails(error))}\n`
  );
  process.exitCode = 1;
});
