'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.CardanoSelfnodeLauncher = void 0;
const child_process_1 = require('child_process');
const path_1 = __importDefault(require('path'));
const fs_1 = __importDefault(require('fs'));
const which_1 = __importDefault(require('which'));
const tcp_port_used_1 = __importDefault(require('tcp-port-used'));
const environment_1 = require('../environment');
const logging_1 = require('../utils/logging');
const processes_1 = require('../utils/processes');
let mockTokenMetadataServer = null;
const CARDANO_WALLET_PORT = 8088;
const CARDANO_WALLET_START_TIMEOUT = 60 * 1000; // 60 seconds | unit: milliseconds
const CARDANO_WALLET_START_CHECK_INTERVAL = 500; // 500 ms | unit: milliseconds
const TOKEN_METADATA_SERVER_PORT = 65432;
// “localhost” breaks under new electron, which prefers ::1 (IPv6)
const TOKEN_METADATA_SERVER = `http://127.0.0.1:${TOKEN_METADATA_SERVER_PORT}/`;
const TOKEN_METADATA_SERVER_PROCESS_NAME = environment_1.environment.isWindows
  ? 'mock-token-metadata-server.exe'
  : 'mock-token-metadata-server';
async function CardanoSelfnodeLauncher(selfnodeOptions) {
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
      const binDir = path_1.default.dirname(which_1.default.sync(selfnodeBin));
      return firstExisting('SHELLEY_TEST_DATA', [
        path_1.default.resolve(
          path_1.default.join(binDir, 'data', 'cardano-node-shelley')
        ),
        path_1.default.resolve(
          path_1.default.join(
            binDir,
            '..',
            'Resources',
            'data',
            'cardano-node-shelley'
          )
        ),
        // Linux installer substitutes SHELLEY_TEST_DATA in the local-cluster Bash wrapper
        '../../utils/cardano/selfnode', // nix-shell? but nix-shell has the substitute ↑ as well… Some other scenario?
      ]);
    })();
    setupMockTokenMetadataServer(
      mockTokenMetadataServerBin,
      mockTokenMetadataServerLogFile
    );
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ pid: number; ppid?: number; uid?: number; ... Remove this comment to see the full error message
    const processList = await (0, processes_1.wrappedFind)(
      'port',
      CARDANO_WALLET_PORT
    );
    const isSelfnodeRunning =
      processList.length && processList[0].name === processName;
    if (isSelfnodeRunning) {
      logging_1.logger.info('Cardano-node is already running...', {
        selfnodeBin,
        CARDANO_WALLET_PORT,
        SHELLEY_TEST_DATA,
        TOKEN_METADATA_SERVER,
      });
      const nodeProcess = processList[0];
      const node = setupSelfnode({
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
      logging_1.logger.info('Starting cardano-node now...', {
        selfnodeBin,
        CARDANO_WALLET_PORT,
        SHELLEY_TEST_DATA,
        TOKEN_METADATA_SERVER,
      });
      const nodeProcess = (0, child_process_1.spawn)(selfnodeBin, [], {
        env: {
          ...process.env,
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          CARDANO_WALLET_PORT,
          SHELLEY_TEST_DATA,
          TOKEN_METADATA_SERVER,
        },
        detached: environment_1.environment.isDev,
        // will not allow you to start another Daedalus when previous local-cluster.exe is running, especially awkward
        // on macOS without stdout/stderr logging; I’m not sure if this is ever needed in `nix-shell`, but leaving as-is
        // for now – @michalrus
        // allows Daedalus to exit independently of selfnode (1/3)
        stdio: environment_1.environment.isDev ? 'ignore' : 'pipe', // 'ignore' allows Daedalus to exit independently of selfnode, 'pipe' kills it on exit (2/3)
      });
      if (environment_1.environment.isDev) {
        nodeProcess.unref(); // allows Daedalus to exit independently of selfnode (3/3)
      }
      if (
        !environment_1.environment.isDev &&
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
      const node = setupSelfnode({
        processName,
        nodeProcess,
        onStop,
        connected: false, // selfnode is kept in disconnected state until it starts responding
      });
      tcp_port_used_1.default
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
exports.CardanoSelfnodeLauncher = CardanoSelfnodeLauncher;
/**
 * Return the first existing file path among candidates. If none exist, return the last one, and log a warning an identifier.
 */
const firstExisting = (identifier, candidates) => {
  const existing = candidates.filter(fs_1.default.existsSync);
  if (existing.length > 0) return existing[0];
  const fallback = candidates[candidates.length - 1];
  logging_1.logger.warn(
    `${identifier} candidates don’t exist, will use fallback`,
    {
      identifier,
      candidates,
      fallback,
    }
  );
  return fallback;
};
const setupSelfnode = ({ processName, nodeProcess, onStop, connected }) =>
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
  mockTokenMetadataServerBin,
  mockTokenMetadataServerLogFile
) => {
  const TOKEN_METADATA_REGISTRY = (() => {
    const binDir = path_1.default.dirname(
      which_1.default.sync(mockTokenMetadataServerBin)
    );
    return firstExisting('TOKEN_METADATA_REGISTRY', [
      path_1.default.resolve(
        path_1.default.join(binDir, 'token-metadata.json')
      ),
      path_1.default.resolve(
        path_1.default.join(binDir, '..', 'Resources', 'token-metadata.json')
      ),
      './utils/cardano/selfnode/token-metadata.json', // nix-shell
    ]);
  })();
  // @ts-ignore ts-migrate(2322) FIXME: Type '{ pid: number; ppid?: number; uid?: number; ... Remove this comment to see the full error message
  const processList = await (0, processes_1.wrappedFind)(
    'port',
    TOKEN_METADATA_SERVER_PORT
  );
  const isMockTokenMetadataServerRunning =
    processList.length &&
    processList[0].name === TOKEN_METADATA_SERVER_PROCESS_NAME;
  if (isMockTokenMetadataServerRunning) {
    logging_1.logger.info('Mock-token-metadata-server is already running...', {
      mockTokenMetadataServerBin,
      TOKEN_METADATA_SERVER_PORT,
      TOKEN_METADATA_REGISTRY,
      TOKEN_METADATA_SERVER,
    });
    mockTokenMetadataServer = Object.assign({}, processList[0]);
  } else {
    logging_1.logger.info('Starting mock-token-metadata-server now...', {
      mockTokenMetadataServerBin,
      TOKEN_METADATA_SERVER_PORT,
      TOKEN_METADATA_REGISTRY,
      TOKEN_METADATA_SERVER,
    });
    const mockTokenMetadataServerProcess = (0, child_process_1.spawn)(
      mockTokenMetadataServerBin,
      [
        '--port',
        TOKEN_METADATA_SERVER_PORT.toString(),
        TOKEN_METADATA_REGISTRY,
      ],
      {
        env: { ...process.env },
        detached: environment_1.environment.isDev,
        stdio: environment_1.environment.isDev ? 'ignore' : 'pipe',
      }
    );
    if (environment_1.environment.isDev) {
      mockTokenMetadataServerProcess.unref();
    }
    if (
      !environment_1.environment.isDev &&
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
//# sourceMappingURL=CardanoSelfnodeLauncher.js.map
