// @flow
import { spawn } from 'child_process';
import type { ChildProcess } from 'child_process';

/*
  cardano-wallet launch
    [--network=STRING]
    [(--port=INT | --random-port)]
    [--node-port=INT]
    [--state-dir=DIR]
    [(--quiet | --verbose )]
*/
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
    '--network',
    networkMode,
    '--node-port',
    String(nodePort),
    '--state-dir',
    stateDir,
    '--random-port',
  ];

  const walletStdio: string[] = ['inherit', logStream, logStream, 'ipc'];
  return spawn(`${path} launch`, opts, { stdio: walletStdio });
}
