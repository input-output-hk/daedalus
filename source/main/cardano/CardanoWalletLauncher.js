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
  nodePort,
  path,
  stateDir,
}: WalletOpts): ChildProcess {
  // Note: Network Mode doesn't currently apply to cardano-wallet-jormungandr
  // It will be re-applied when it does
  const opts = [
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
    // TODO: Move Jormangandr opts to config
    '--genesis-block',
    'utils/jormungandr/block0.bin',
    '--bft-leaders',
    // TODO: This needs to be generated for builds
    'utils/jormungandr/secret.yaml',
  ];

  const walletStdio: string[] = ['inherit', logStream, logStream, 'ipc'];
  return spawn(path, opts, { stdio: walletStdio });
}
