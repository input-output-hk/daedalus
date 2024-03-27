'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.CardanoWalletLauncher = void 0;
const lodash_1 = require('lodash');
const path_1 = __importDefault(require('path'));
const cardanoLauncher = __importStar(require('cardano-launcher'));
const config_1 = require('../config');
const environment_types_1 = require('../../common/types/environment.types');
const cardano_node_types_1 = require('../../common/types/cardano-node.types');
const utils_1 = require('./utils');
const logging_1 = require('../utils/logging');
async function CardanoWalletLauncher(
  walletOptions
  // @ts-ignore ts-migrate(1064) FIXME: The return type of an async function or method mus... Remove this comment to see the full error message
) {
  const {
    nodeImplementation,
    nodeConfig,
    // For cardano-node / byron only!
    cluster,
    stateDir,
    tlsPath,
    configPath,
    syncTolerance,
    nodeLogFile,
    walletLogFile,
    cliBin,
    isStaging,
    metadataUrl,
    rtsFlags,
  } = walletOptions;
  // TODO: Update launcher config to pass number
  const syncToleranceSeconds = parseInt(syncTolerance.replace('s', ''), 10);
  // Shared launcher config (node implementations agnostic)
  const launcherConfig = {
    networkName: cluster,
    stateDir,
    nodeConfig: {
      kind: nodeImplementation,
      configurationDir: '',
      network: {
        configFile: configPath,
      },
    },
    syncToleranceSeconds,
    childProcessLogWriteStreams: {
      node: nodeLogFile,
      wallet: walletLogFile,
    },
    installSignalHandlers: false,
  };
  // TLS configuration used only for cardano-node
  const tlsConfiguration = {
    caCert: path_1.default.join(tlsPath, 'server/ca.crt'),
    svCert: path_1.default.join(tlsPath, 'server/server.crt'),
    svKey: path_1.default.join(tlsPath, 'server/server.key'),
  };
  let tokenMetadataServer;
  // This switch statement handles any node specific
  // configuration, prior to spawning the child process
  logging_1.logger.info('Node implementation', {
    nodeImplementation,
  });
  switch (nodeImplementation) {
    case cardano_node_types_1.CardanoNodeImplementationOptions.CARDANO:
      if (cluster === environment_types_1.SELFNODE) {
        const { configFile, genesisFile } = nodeConfig.network;
        const {
          configPath: selfnodeConfigPath,
          genesisPath: selfnodeGenesisPath,
          genesisHash: selfnodeGenesisHash,
        } = await (0, utils_1.createSelfnodeConfig)(
          configFile,
          genesisFile,
          stateDir,
          cliBin
        );
        nodeConfig.network.configFile = selfnodeConfigPath;
        nodeConfig.network.genesisFile = selfnodeGenesisPath;
        nodeConfig.network.genesisHash = selfnodeGenesisHash;
        (0, lodash_1.merge)(launcherConfig, {
          apiPort: 8088,
        });
      }
      if (cluster === environment_types_1.MAINNET) {
        launcherConfig.networkName = environment_types_1.MAINNET;
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info('Launching Wallet with --mainnet flag');
      } else if (isStaging) {
        launcherConfig.networkName = environment_types_1.STAGING;
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info('Launching Wallet with --staging flag');
      } else {
        // All clusters not flagged as staging except for Mainnet are treated as "Testnets"
        launcherConfig.networkName = environment_types_1.TESTNET;
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info('Launching Wallet with --testnet flag');
      }
      if (config_1.MOCK_TOKEN_METADATA_SERVER_PORT) {
        tokenMetadataServer = `${config_1.MOCK_TOKEN_METADATA_SERVER_URL}:${config_1.MOCK_TOKEN_METADATA_SERVER_PORT}`;
      } else if (metadataUrl) {
        tokenMetadataServer = metadataUrl;
      } else {
        tokenMetadataServer = config_1.FALLBACK_TOKEN_METADATA_SERVER_URL;
      }
      logging_1.logger.info(
        'Launching Wallet with --token-metadata-server flag',
        {
          tokenMetadataServer,
        }
      );
      // RTS flags:
      // 1) "-H4G -M6553M -c70"  16.0% peak RSS reduction and a sub-percentile CPU regression
      // 2) "-H4G -M6553M"       18.5% peak RSS reduction and a second-best CPU regression
      if (!!rtsFlags && rtsFlags?.length > 0) {
        nodeConfig.rtsOpts = rtsFlags;
        logging_1.logger.info('Launching Cardano Node with RTS flags', {
          rtsFlags,
        });
      } else {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info('Launching Cardano Node without RTS flags');
      }
      (0, lodash_1.merge)(launcherConfig, {
        nodeConfig,
        tlsConfiguration,
        tokenMetadataServer,
      });
      break;
    default:
      break;
  }
  logging_1.logger.info('Setting up CardanoLauncher now...', {
    walletOptions,
    launcherConfig,
  });
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ networkName: string; stateDir:... Remove this comment to see the full error message
  return new cardanoLauncher.Launcher(launcherConfig, logging_1.logger);
}
exports.CardanoWalletLauncher = CardanoWalletLauncher;
//# sourceMappingURL=CardanoWalletLauncher.js.map
