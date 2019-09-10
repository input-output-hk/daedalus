// @flow
import { spawn } from 'child_process';
import type { ChildProcess } from 'child_process';
import {
  createClientSecret,
  buildHttpBridgeNodeOpts,
  buildJormungandrNodeOpts,
} from './nodes';

export type WalletOpts = {
  path: string,
  cliPath: string,
  nodeImplementation: 'cardano-http-bridge' | 'jormungandr' | 'cardano-node',
  networkMode: string,
  nodePort: number,
  stateDir: string,
  logStream: any,
};

export async function CardanoWalletLauncher(
  walletOpts: WalletOpts
): Promise<ChildProcess> {
  const { logStream, nodeImplementation, cliPath, stateDir, path } = walletOpts;

  let nodeOpts: string[] = [];
  switch (nodeImplementation) {
    case 'cardano-http-bridge':
      nodeOpts = buildHttpBridgeNodeOpts(walletOpts);
      break;
    case 'cardano-node':
      break;
    case 'jormungandr':
      await createClientSecret(cliPath, `${stateDir}/secret.yaml`);
      nodeOpts = buildJormungandrNodeOpts(walletOpts);
      break;
    default:
      break;
  }

  const walletStdio: string[] = ['inherit', logStream, logStream, 'ipc'];
  return spawn(path, nodeOpts, { stdio: walletStdio });
}
