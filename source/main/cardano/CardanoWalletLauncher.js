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
  networkMode,
  nodePort,
  path,
  stateDir,
}: WalletOpts): ChildProcess {
  const networkOpt =
    networkMode === 'local' ? ['--local-network'] : ['--network', networkMode];

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
  ].concat(networkOpt);

  const walletStdio: string[] = ['inherit', logStream, logStream, 'ipc'];
  return spawn(path, opts, { stdio: walletStdio });
}
