import { spawn } from 'child_process';
import find from 'find-process';
import tcpPortUsed from 'tcp-port-used';
import type { ChildProcess } from 'child_process';
import type { Process } from '../utils/processes';
import type { CardanoNodeProcessNames } from '../../common/types/cardano-node.types';
import { environment } from '../environment';
import { logger } from '../utils/logging';

export type SelfnodeOptions = {
  selfnodeBin: string;
  mockTokenMetadataServerBin: string;
  processName: CardanoNodeProcessNames;
  onStop: (...args: Array<any>) => any;
};
export type Selfnode = {
  pid: number;
  wpid: number;
  stop: (...args: Array<any>) => any;
  connected: boolean;
};
let mockTokenMetadataServer = null;
const platform = String(environment.platform);
const CARDANO_WALLET_PORT = 8088;
const CARDANO_WALLET_START_TIMEOUT = 60 * 1000; // 60 seconds | unit: milliseconds

const CARDANO_WALLET_START_CHECK_INTERVAL = 500; // 500 ms | unit: milliseconds

const SHELLEY_TEST_DATA = '../../utils/cardano/selfnode';
const TOKEN_METADATA_REGISTRY = './utils/cardano/selfnode/token-metadata.json';
const TOKEN_METADATA_SERVER_PORT = 65432;
const TOKEN_METADATA_SERVER = `http://localhost:${TOKEN_METADATA_SERVER_PORT}/`;
const TOKEN_METADATA_SERVER_PROCESS_NAME =
  platform === 'win32'
    ? 'mock-token-metadata-server.exe'
    : 'mock-token-metadata-server';
export async function CardanoSelfnodeLauncher(
  selfnodeOptions: SelfnodeOptions
): Promise<{
  node: Selfnode;
  replyPort: number;
}> {
  return new Promise(async (resolve, reject) => {
    const {
      selfnodeBin,
      mockTokenMetadataServerBin,
      processName,
      onStop,
    } = selfnodeOptions;
    setupMockTokenMetadataServer(mockTokenMetadataServerBin);
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ pid: number; ppid?: number; uid?: number; ... Remove this comment to see the full error message
    const processList: Array<Process> = await find('port', CARDANO_WALLET_PORT);
    const isSelfnodeRunning =
      processList.length && processList[0].name === processName;

    if (isSelfnodeRunning) {
      logger.info('Cardano-node is already running...', {
        selfnodeBin,
        CARDANO_WALLET_PORT,
        SHELLEY_TEST_DATA,
        TOKEN_METADATA_SERVER,
      });
      const nodeProcess: Process = processList[0];
      const node: Selfnode = setupSelfnode({
        processName,
        nodeProcess,
        onStop,
        connected: true,
      });
      resolve({
        node,
        replyPort: CARDANO_WALLET_PORT,
      });
    } else {
      logger.info('Starting cardano-node now...', {
        selfnodeBin,
        CARDANO_WALLET_PORT,
        SHELLEY_TEST_DATA,
        TOKEN_METADATA_SERVER,
      });
      const nodeProcess: ChildProcess = spawn(selfnodeBin, [], {
        env: {
          ...process.env,
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          CARDANO_WALLET_PORT,
          SHELLEY_TEST_DATA,
          TOKEN_METADATA_SERVER,
        },
        detached: true,
        // allows Daedalus to exit independently of selfnode (1/3)
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
        .then(() => {
          node.connected = true;
          resolve({
            node,
            replyPort: CARDANO_WALLET_PORT,
          });
        })
        .catch((exitStatus) => {
          reject(exitStatus);
        });
    }
  });
}

const setupSelfnode = ({
  processName,
  nodeProcess,
  onStop,
  connected,
}: {
  processName: CardanoNodeProcessNames;
  nodeProcess: ChildProcess | Process;
  onStop: (...args: Array<any>) => any;
  connected: boolean;
}): Selfnode =>
  // @ts-ignore ts-migrate(2322) FIXME: Type '(ChildProcess | Process) & { wpid: number; s... Remove this comment to see the full error message
  Object.assign({}, nodeProcess, {
    wpid: nodeProcess.pid,
    stop: async () => {
      if (mockTokenMetadataServer !== null) {
        await onStop(
          mockTokenMetadataServer.pid,
          TOKEN_METADATA_SERVER_PROCESS_NAME
        );
      }

      await onStop(nodeProcess.pid, processName);
    },
    connected,
  });

const setupMockTokenMetadataServer = async (
  mockTokenMetadataServerBin: string
) => {
  // @ts-ignore ts-migrate(2322) FIXME: Type '{ pid: number; ppid?: number; uid?: number; ... Remove this comment to see the full error message
  const processList: Array<Process> = await find(
    'port',
    TOKEN_METADATA_SERVER_PORT
  );
  const isMockTokenMetadataServerRunning =
    processList.length &&
    processList[0].name === TOKEN_METADATA_SERVER_PROCESS_NAME;

  if (isMockTokenMetadataServerRunning) {
    logger.info('Mock-token-metadata-server is already running...', {
      mockTokenMetadataServerBin,
      TOKEN_METADATA_SERVER_PORT,
      TOKEN_METADATA_REGISTRY,
      TOKEN_METADATA_SERVER,
    });
    mockTokenMetadataServer = Object.assign({}, processList[0]);
  } else {
    logger.info('Starting mock-token-metadata-server now...', {
      mockTokenMetadataServerBin,
      TOKEN_METADATA_SERVER_PORT,
      TOKEN_METADATA_REGISTRY,
      TOKEN_METADATA_SERVER,
    });
    const mockTokenMetadataServerProcess = spawn(
      mockTokenMetadataServerBin,
      [
        '--port',
        TOKEN_METADATA_SERVER_PORT.toString(),
        TOKEN_METADATA_REGISTRY,
      ],
      {
        env: { ...process.env },
        detached: true,
        stdio: 'ignore',
      }
    );
    mockTokenMetadataServerProcess.unref();
    mockTokenMetadataServer = mockTokenMetadataServerProcess;
  }
};
