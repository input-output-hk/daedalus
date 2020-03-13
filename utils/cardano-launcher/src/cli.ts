/**
 * `cardano-launcher` command-line interface.
 *
 * This tool can be used for testing.
 *
 * See also: the entrypoint script `bin/cardano-launcher`.
 *
 * @packageDocumentation
 */

import process from 'process';
import _ from 'lodash';

import {
  Launcher,
  ExitStatus,
  ServiceExitStatus,
  serviceExitStatusMessage,
} from './cardanoLauncher';

import * as byron from './byron';
import * as jormungandr from './jormungandr';

function combineStatus(statuses: ServiceExitStatus[]): number {
  let code = _.reduce(
    statuses,
    (res: number | null, status) => (res === null ? status.code : res),
    null
  );
  let signal = _.reduce(
    statuses,
    (res: string | null, status) => (res === null ? status.signal : res),
    null
  );
  // let err = _.reduce(statuses, (res, status) => res === null ? status.err : res, null);

  return code === null ? (signal === null ? 0 : 127) : code;
}

/**
 * Main function of the CLI.
 *
 * Is just a very basic interface for testing things.
 */
export function cli(args: string[]) {
  const waitForExit = setInterval(function() {}, 3600000);

  args.shift(); // /usr/bin/node
  args.shift(); // cardano-launcher

  if (args.length < 4) {
    usage();
  }

  const backend = args.shift() as string;
  const networkName = args.shift() as string;
  const configurationDir = args.shift() as string;
  const stateDir = args.shift() as string;

  let nodeConfig: any;

  if (backend === 'byron') {
    if (!(networkName in byron.networks)) {
      console.error(`unknown network: ${networkName}`);
      process.exit(2);
    }
    const network = byron.networks[networkName];
    nodeConfig = {
      kind: backend,
      configurationDir,
      network,
    };
  } else if (backend === 'jormungandr') {
    if (!(networkName in jormungandr.networks)) {
      console.error(`unknown network: ${networkName}`);
      process.exit(2);
    }
    const network = jormungandr.networks[networkName];
    nodeConfig = {
      kind: backend,
      configurationDir,
      network,
    };
  } else {
    usage();
  }

  const launcher = new Launcher({ stateDir, nodeConfig, networkName }, console);

  launcher.start();

  // inform tests of subprocess pids
  launcher.nodeService.start().then(pid => sendMaybe({ node: pid }));
  launcher.walletService.start().then(pid => sendMaybe({ wallet: pid }));

  launcher.walletBackend.events.on('exit', (status: ExitStatus) => {
    console.log(serviceExitStatusMessage(status.wallet));
    console.log(serviceExitStatusMessage(status.node));
    clearInterval(waitForExit);
    process.exit(combineStatus([status.wallet, status.node]));
  });
}

function usage() {
  console.log('usage: cardano-launcher BACKEND NETWORK CONFIG-DIR STATE-DIR');
  console.log('  BACKEND    - either jormungandr or byron');
  console.log(
    '  NETWORK    - depends on backend, e.g. mainnet, itn_rewards_v1'
  );
  console.log(
    '  CONFIG-DIR - directory which contains config files for a backend'
  );
  console.log('  STATE-DIR  - directory to put blockchains, databases, etc.');
  process.exit(1);
}

function sendMaybe(message: object) {
  if (process.send) {
    process.send(message);
  }
}
