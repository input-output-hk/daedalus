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

  const walletStdio: string[] = ['inherit', logStream, logStream, 'ipc'];
  return spawn(path, opts, { stdio: walletStdio });
}
