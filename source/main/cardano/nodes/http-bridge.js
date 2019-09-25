// @flow
import type { WalletOpts } from '../CardanoWalletLauncher';

export function buildHttpBridgeNodeOpts({
  networkMode,
  nodePort,
  stateDir,
}: WalletOpts): string[] {
  const networkOpt =
    networkMode === 'local' ? ['--local-network'] : ['--network', networkMode];

  return [
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
}
