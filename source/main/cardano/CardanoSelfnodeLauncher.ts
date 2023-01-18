import { spawn } from 'child_process';
import path from 'path';
import fs, { WriteStream } from 'fs';
import which from 'which';
import tcpPortUsed from 'tcp-port-used';
import type { ChildProcess } from 'child_process';
import type { Process } from '../utils/processes';
import type { CardanoNodeProcessNames } from '../../common/types/cardano-node.types';
import { environment } from '../environment';
import { logger } from '../utils/logging';
import { wrappedFind } from '../utils/processes';

export type SelfnodeOptions = {
  selfnodeBin: string;
  walletLogFile: WriteStream;
  mockTokenMetadataServerBin: string;
  mockTokenMetadataServerLogFile: WriteStream;
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
const CARDANO_WALLET_PORT = 8088;
const CARDANO_WALLET_START_TIMEOUT = 60 * 1000; // 60 seconds | unit: milliseconds

const CARDANO_WALLET_START_CHECK_INTERVAL = 500; // 500 ms | unit: milliseconds

const TOKEN_METADATA_SERVER_PORT = 65432;
const TOKEN_METADATA_SERVER = `http://localhost:${TOKEN_METADATA_SERVER_PORT}/`;
const TOKEN_METADATA_SERVER_PROCESS_NAME = environment.isWindows
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
      walletLogFile,
      mockTokenMetadataServerBin,
      mockTokenMetadataServerLogFile,
      processName,
      onStop,
    } = selfnodeOptions;

    const SHELLEY_TEST_DATA = (() => {
      const binDir = path.dirname(which.sync(selfnodeBin));
      return firstExisting('SHELLEY_TEST_DATA', [
        path.resolve(path.join(binDir, 'data', 'cardano-node-shelley')), // Windows installer
        path.resolve(
          path.join(binDir, '..', 'Resources', 'data', 'cardano-node-shelley')
        ), // Darwin installer
        // Linux installer substitutes SHELLEY_TEST_DATA in the local-cluster Bash wrapper
        '../../utils/cardano/selfnode', // nix-shell? but nix-shell has the substitute ↑ as well… Some other scenario?
      ]);
    })();

    setupMockTokenMetadataServer(
      mockTokenMetadataServerBin,
      mockTokenMetadataServerLogFile
    );
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ pid: number; ppid?: number; uid?: number; ... Remove this comment to see the full error message
    const processList: Array<Process> = await wrappedFind(
      'port',
      CARDANO_WALLET_PORT
    );
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
        detached: environment.isDev, // XXX: detaching breaks Windows launching the local-cluster.exe + cardano-launcher
        // will not allow you to start another Daedalus when previous local-cluster.exe is running, especially awkward
        // on macOS without stdout/stderr logging; I’m not sure if this is ever needed in `nix-shell`, but leaving as-is
        // for now – @michalrus

        // allows Daedalus to exit independently of selfnode (1/3)
        stdio: environment.isDev ? 'ignore' : 'pipe', // 'ignore' allows Daedalus to exit independently of selfnode, 'pipe' kills it on exit (2/3)
      });
      if (environment.isDev) {
        nodeProcess.unref(); // allows Daedalus to exit independently of selfnode (3/3)
      }
      if (
        !environment.isDev &&
        nodeProcess.stdout &&
        nodeProcess.stderr &&
        walletLogFile
      ) {
        nodeProcess.stdout.on('data', (data) => {
          walletLogFile.write(data);
        });
        nodeProcess.stderr.on('data', (data) => {
          walletLogFile.write(data);
        });
      }

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

/**
 * Return the first existing file path among candidates. If none exist, return the last one, and log a warning an identifier.
 */
const firstExisting = (
  identifier: string,
  candidates: Array<string>
): string => {
  const existing = candidates.filter(fs.existsSync);
  if (existing.length > 0) return existing[0];
  const fallback = candidates[candidates.length - 1];
  logger.warn(`${identifier} candidates don’t exist, will use fallback`, {
    identifier,
    candidates,
    fallback,
  });
  return fallback;
};

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
  mockTokenMetadataServerBin: string,
  mockTokenMetadataServerLogFile: WriteStream
) => {
  const TOKEN_METADATA_REGISTRY = (() => {
    const binDir = path.dirname(which.sync(mockTokenMetadataServerBin));
    return firstExisting('TOKEN_METADATA_REGISTRY', [
      path.resolve(path.join(binDir, 'token-metadata.json')), // Windows and Linux installers
      path.resolve(path.join(binDir, '..', 'Resources', 'token-metadata.json')), // Darwin installer
      './utils/cardano/selfnode/token-metadata.json', // nix-shell
    ]);
  })();

  // @ts-ignore ts-migrate(2322) FIXME: Type '{ pid: number; ppid?: number; uid?: number; ... Remove this comment to see the full error message
  const processList: Array<Process> = await wrappedFind(
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
        detached: environment.isDev,
        stdio: environment.isDev ? 'ignore' : 'pipe',
      }
    );
    if (environment.isDev) {
      mockTokenMetadataServerProcess.unref();
    }
    if (
      !environment.isDev &&
      mockTokenMetadataServerProcess.stdout &&
      mockTokenMetadataServerProcess.stderr &&
      mockTokenMetadataServerLogFile
    ) {
      mockTokenMetadataServerProcess.stdout.on('data', (data) => {
        mockTokenMetadataServerLogFile.write(data);
      });
      mockTokenMetadataServerProcess.stderr.on('data', (data) => {
        mockTokenMetadataServerLogFile.write(data);
      });
    }
    mockTokenMetadataServer = mockTokenMetadataServerProcess;
  }
};
