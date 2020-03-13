import * as process from 'process';
import * as tmp from 'tmp-promise';
import * as path from 'path';

import { delay, expectProcessToBeGone } from './utils';
import { spawn } from 'child_process';

// TODO: for Windows, the cli needs to be a .cmd file. I have no idea
// how to set that up with typescript. So skipping the tests for now.
// https://github.com/input-output-hk/cardano-launcher/issues/12
const describeCLITests = process.platform === 'win32' ? xdescribe : describe;

describeCLITests('CLI tests', () => {
  const killTest = (args: string[]) => async () => {
    const stateDir = (
      await tmp.dir({ unsafeCleanup: true, prefix: 'launcher-cli-test' })
    ).path;
    const proc = spawn('./bin/cardano-launcher', args.concat([stateDir]), {
      stdio: ['inherit', 'inherit', 'inherit', 'ipc'],
    });
    let nodePid: number | null = null;
    let walletPid: number | null = null;
    proc.on('message', (message: any) => {
      console.log('received message', message);
      if (message.node) {
        nodePid = message.node;
      }
      if (message.wallet) {
        walletPid = message.wallet;
      }
    });
    await delay(1000);
    expect(nodePid).not.toBeNull();
    expect(walletPid).not.toBeNull();
    proc.kill();
    await delay(1000);
    expectProcessToBeGone(nodePid as any, 9);
    expectProcessToBeGone(walletPid as any, 9);
  };

  const jormungandr = [
    'jormungandr',
    'self',
    path.join('test', 'data', 'jormungandr'),
  ];
  const byron = ['byron', 'mainnet', '' + process.env.BYRON_CONFIGS];

  it(
    'when the parent process is killed, child jormungandr gets stopped',
    killTest(jormungandr) as any
  );
  it(
    'when the parent process is killed, cardano-node gets stopped',
    killTest(byron)
  );
});
