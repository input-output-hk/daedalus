// @flow
import { spawn } from 'child_process';
import find from 'find-process';
import tcpPortUsed from 'tcp-port-used';
import type { ChildProcess } from 'child_process';
import type { Process } from '../utils/processes';
import type { CardanoNodeProcessNames } from '../../common/types/cardano-node.types';
import { logger } from '../utils/logging';

export type SelfnodeOptions = {
  selfnodeBin: string,
  processName: CardanoNodeProcessNames,
  onStop: Function,
};

export type Selfnode = {
  pid: number,
  wpid: number,
  stop: Function,
  connected: boolean,
};

const CARDANO_WALLET_START_TIMEOUT = 60 * 1000; // 60 seconds | unit: milliseconds
const CARDANO_WALLET_START_CHECK_INTERVAL = 500; // 500 ms | unit: milliseconds
const CARDANO_WALLET_PORT = 8088;
const SHELLEY_TEST_DATA = '../../utils/cardano/selfnode';

export async function CardanoSelfnodeLauncher(
  selfnodeOptions: SelfnodeOptions
): Promise<{
  node: Selfnode,
  replyPort: number,
}> {
  return new Promise(async (resolve, reject) => {
    const { selfnodeBin, processName, onStop } = selfnodeOptions;
    const processList: Array<Process> = await find('port', CARDANO_WALLET_PORT);
    const isSelfnodeRunning =
      processList.length && processList[0].name === processName;
    if (isSelfnodeRunning) {
      logger.info('Cardano-node is already running...');
      const nodeProcess: Process = processList[0];
      const node: Selfnode = setupSelfnode({
        processName,
        nodeProcess,
        onStop,
        connected: true,
      });
      resolve({ node, replyPort: CARDANO_WALLET_PORT });
    } else {
      logger.info('Starting cardano-node now...');
      const nodeProcess: ChildProcess = spawn(selfnodeBin, [], {
        env: { ...process.env, CARDANO_WALLET_PORT, SHELLEY_TEST_DATA },
        detached: true, // allows Daedalus to exit independently of selfnode (1/3)
        stdio: 'ignore', // allows Daedalus to exit independently of selfnode (2/3)
      });
      nodeProcess.unref(); // allows Daedalus to exit independently of selfnode (3/3)
      const node: Selfnode = setupSelfnode({
        processName,
        nodeProcess,
        onStop,
        connected: false, // selfnode is kept in disconnected state until it starts responding
      });
      tcpPortUsed
        .waitUntilUsed(
          CARDANO_WALLET_PORT,
          CARDANO_WALLET_START_CHECK_INTERVAL,
          CARDANO_WALLET_START_TIMEOUT
        )
        .then(
          () => {
            logger.info(
              `CardanoNode#start: cardano-node child process spawned with PID ${node.pid}`,
              { pid: node.pid }
            );
            node.connected = true;
            resolve({ node, replyPort: CARDANO_WALLET_PORT });
          },
          (exitStatus) => {
            reject(exitStatus);
          }
        );
    }
  });
}

const setupSelfnode = ({
  processName,
  nodeProcess,
  onStop,
  connected,
}: {
  processName: CardanoNodeProcessNames,
  nodeProcess: ChildProcess | Process,
  onStop: Function,
  connected: boolean,
}): Selfnode =>
  Object.assign({}, nodeProcess, {
    wpid: nodeProcess.pid,
    stop: async () => {
      await onStop(nodeProcess.pid, processName);
    },
    connected,
  });
