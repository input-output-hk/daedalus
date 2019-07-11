// @flow
import { spawn } from 'child_process';
import type { ChildProcess } from 'child_process';

export type WalletOpts = {
  path: string,
  networkMode: string,
  nodePort: number,
  stateDir: string,
  logStream: any,
};

export function CardanoWalletLauncher({
  logStream,
  // networkMode,
  // nodePort,
  path,
  stateDir,
}: WalletOpts): ChildProcess {
  /*
  NOTE: These are the options we want, but due to the local http-bridge 
  being unable to connect to the local demo cluster, we'll rely on the
  http-bridge spun up as part of the docker cluster, and run the wallet
  in serve mode. We will change this once we have access to the new nodes

  const opts = [
    'launch',
    '--network',
    networkMode,
    '--backend-port',
    String(nodePort),
    '--state-dir',
    stateDir,
    // TODO: Neither of these are working for some reason
    // '--random-port',
    // '--port',
    // '8889',
  ];
  */

  // Hardcoded values until we can uncomment the block above
  const opts = [
    'serve',
    '--network',
    'testnet',
    '--port',
    '8088',
    '--backend-port',
    '8090',
    '--database',
    `${stateDir}/wallet.db`,
  ];

  const walletStdio: string[] = ['inherit', logStream, logStream, 'ipc'];
  return spawn(path, opts, { stdio: walletStdio });
}
