'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

function _interopDefault (ex) { return (ex && (typeof ex === 'object') && 'default' in ex) ? ex['default'] : ex; }

var path = _interopDefault(require('path'));
var mkdirp = _interopDefault(require('mkdirp'));
var process$1 = _interopDefault(require('process'));
var net = _interopDefault(require('net'));
var _ = _interopDefault(require('lodash'));
var tsee = require('tsee');
var getPort = _interopDefault(require('get-port'));
var child_process = require('child_process');

/**
 * Create a new logger with a context name added.
 *
 * @param logger - existing logger.
 * @param name - context to prepend.
 * @return - a new logger.
 */
function prependName(logger, name) {
  var prefix = function prefix(severity, msg, param) {
    var prefixed = name + ": " + msg;

    if (param) {
      logger[severity](prefixed, param);
    } else {
      logger[severity](prefixed);
    }
  };

  return {
    debug: function debug(msg, param) {
      return prefix('debug', msg, param);
    },
    info: function info(msg, param) {
      return prefix('info', msg, param);
    },
    error: function error(msg, param) {
      return prefix('error', msg, param);
    }
  };
}

/**
 * Functions for starting and stopping an individual backend service.
 *
 * The important function is [[setupService]] which creates a [[Service]].
 *
 * @packageDocumentation
 */
/**
 * Produce an exit message from an exit status.
 * @param res - exit status of service.
 * @return a human readable exit message.
 */

function serviceExitStatusMessage(res) {
  var reason = typeof res.code === 'number' ? "status " + res.code : res.signal ? "signal " + res.signal : "error " + res.err;
  return res.exe + " exited with " + reason;
}

(function (ServiceStatus) {
  /** Initial state. */
  ServiceStatus[ServiceStatus["NotStarted"] = 0] = "NotStarted";
  /** Waiting for [[StartService]] info. */

  ServiceStatus[ServiceStatus["Starting"] = 1] = "Starting";
  /** Subprocess has been started and has a PID. */

  ServiceStatus[ServiceStatus["Started"] = 2] = "Started";
  /** Caller has requested to stop the process. Now waiting for it to exit, or for the timeout to elapse. */

  ServiceStatus[ServiceStatus["Stopping"] = 3] = "Stopping";
  /** Subprocess has exited or been killed. */

  ServiceStatus[ServiceStatus["Stopped"] = 4] = "Stopped";
})(exports.ServiceStatus || (exports.ServiceStatus = {}));
/**
 * Initialise a [[Service]] which can control the lifetime of a
 * backend process.
 *
 * This does not start the process. Use [[Service.start]] for that.
 *
 * @param cfgPromise - a promise which will return the command to run.
 * @param logger - logging object.
 * @return A handle on the [[Service]].
 */


function setupService(cfgPromise, logger) {
  if (logger === void 0) {
    logger = console;
  }

  var events = new tsee.EventEmitter(); // What the current state is.

  var status = exports.ServiceStatus.NotStarted; // Fulfilled promise of service command-line.
  // This will always be defined if status > Starting.

  var cfg; // NodeJS child process object, or null if not running.

  var proc = null; // How the child process exited, or null if it hasn't yet exited.

  var exitStatus; // For cancelling the kill timeout.

  var killTimer = null;
  var startPromise;

  var doStart = function doStart() {
    try {
      var envStr = _.map(cfg.extraEnv, function (value, name) {
        return name + "=" + value + " ";
      }).join('');

      var commandStr = "" + envStr + cfg.command + " " + cfg.args.join(' ');
      logger.info("Service.start: trying to start " + commandStr, cfg);
      var stdio = [cfg.supportsCleanShutdown ? 'pipe' : 'ignore', 'inherit', 'inherit'];
      var cwd = cfg.cwd ? {
        cwd: cfg.cwd
      } : {};
      var env = cfg.extraEnv ? {
        env: Object.assign({}, process.env, cfg.extraEnv)
      } : {};
      var options = Object.assign({
        stdio: stdio
      }, cwd, env);

      try {
        proc = child_process.spawn(cfg.command, cfg.args, options);
      } catch (err) {
        logger.error("Service.start: child_process.spawn() failed: " + err);
        logger.error("Service.start: child_process.spawn(" + cfg.command + ", " + cfg.args.join(' ') + ", ...)", options);
        throw err;
      }

      setStatus(exports.ServiceStatus.Started);
      proc.on('exit', function (code, signal) {
        onStopped(code, signal);
      });
      proc.on('error', function (err) {
        logger.error("Service.start: child_process failed: " + err);
        onStopped(null, null, err);
      });
      return Promise.resolve(proc.pid);
    } catch (e) {
      return Promise.reject(e);
    }
  };

  var doStop = function doStop(timeoutSeconds) {
    logger.info("Service.stop: trying to stop " + cfg.command, cfg);
    setStatus(exports.ServiceStatus.Stopping);

    if (proc) {
      if (cfg.supportsCleanShutdown && proc.stdin) {
        proc.stdin.end();
      } else {
        proc.kill('SIGTERM');
      }
    }

    killTimer = setTimeout(function () {
      if (proc) {
        logger.info("Service.stop: timed out after " + timeoutSeconds + " seconds. Killing process " + proc.pid + ".");
        proc.kill('SIGKILL');
      }
    }, timeoutSeconds * 1000);
  };

  var onStopped = function onStopped(code, signal, err) {
    if (code === void 0) {
      code = null;
    }

    if (signal === void 0) {
      signal = null;
    }

    if (err === void 0) {
      err = null;
    }

    exitStatus = {
      exe: cfg.command,
      code: code,
      signal: signal,
      err: err
    };
    logger.debug("Service onStopped", exitStatus);

    if (killTimer) {
      clearTimeout(killTimer);
      killTimer = null;
    }

    proc = null;
    setStatus(exports.ServiceStatus.Stopped);
  };

  var waitForStop = function waitForStop() {
    return new Promise(function (resolve) {
      logger.debug("Service.stop: waiting for ServiceStatus.Stopped");
      events.on('statusChanged', function (status) {
        if (status === exports.ServiceStatus.Stopped && exitStatus) {
          resolve(exitStatus);
        }
      });
    });
  };

  var waitForExit = function waitForExit() {
    var defaultExitStatus = {
      exe: cfg ? cfg.command : '',
      code: null,
      signal: null,
      err: null
    };

    switch (status) {
      case exports.ServiceStatus.NotStarted:
      case exports.ServiceStatus.Starting:
        return new Promise(function (resolve) {
          status = exports.ServiceStatus.Stopped;
          exitStatus = defaultExitStatus;
          resolve(exitStatus);
        });

      case exports.ServiceStatus.Started:
        return waitForStop();

      case exports.ServiceStatus.Stopping:
        return waitForStop();

      case exports.ServiceStatus.Stopped:
        return new Promise(function (resolve) {
          return resolve(exitStatus || defaultExitStatus);
        });
    }
  };

  var setStatus = function setStatus(newStatus) {
    logger.debug("setStatus " + exports.ServiceStatus[status] + " -> " + exports.ServiceStatus[newStatus]);
    status = newStatus;
    events.emit('statusChanged', status);
  };

  return {
    start: function () {
      try {
        switch (status) {
          case exports.ServiceStatus.NotStarted:
            setStatus(exports.ServiceStatus.Starting);
            startPromise = cfgPromise.then(function (theCfg) {
              cfg = theCfg;
              return doStart();
            });
            return Promise.resolve(startPromise);

          case exports.ServiceStatus.Starting:
            logger.info("Service.start: already starting");
            return Promise.resolve(startPromise);

          case exports.ServiceStatus.Started:
            logger.info("Service.start: already started");
            return Promise.resolve(proc ? proc.pid : -1);

          case exports.ServiceStatus.Stopping:
            logger.info("Service.start: cannot start - already stopping");
            return Promise.resolve(-1);

          case exports.ServiceStatus.Stopped:
            logger.info("Service.start: cannot start - already stopped");
            return Promise.resolve(-1);
        }

        return Promise.resolve();
      } catch (e) {
        return Promise.reject(e);
      }
    },
    stop: function (timeoutSeconds) {
      if (timeoutSeconds === void 0) {
        timeoutSeconds = 60;
      }

      try {
        switch (status) {
          case exports.ServiceStatus.NotStarted:
            logger.info("Service.stop: cannot stop - never started");
            break;

          case exports.ServiceStatus.Starting:
          case exports.ServiceStatus.Started:
            doStop(timeoutSeconds);
            break;

          case exports.ServiceStatus.Stopping:
            if (timeoutSeconds === 0 && proc) {
              logger.info("Service.stop: was already stopping, but will now kill process " + proc.pid + " immediately");
              proc.kill('SIGKILL');
            } else {
              logger.info("Service.stop: already stopping");
            }

            break;

          case exports.ServiceStatus.Stopped:
            logger.info("Service.stop: already stopped");
            break;
        }

        return Promise.resolve(waitForExit());
      } catch (e) {
        return Promise.reject(e);
      }
    },
    waitForExit: waitForExit,
    getStatus: function getStatus() {
      return status;
    },
    events: events
  };
}

/**
 * Configuration for `cardano-node` (Byron)
 *
 * @packageDocumentation
 */
/** Predefined networks. */

/**
 * Chooses the command-line arguments for the node.
 *
 * @param stateDir - directory for node storage, specific to the node type and network.
 * @param config - parameters for starting the node.
 * @return the command-line for starting this node.
 */
var startByronNode = function startByronNode(stateDir, config) {
  try {
    return Promise.resolve(getPort()).then(function (listenPort) {
      var args = makeArgs(stateDir, config, listenPort);
      return {
        command: 'cardano-node',
        args: ['--socket-dir', args.socketDir, '--topology', args.topologyFile, '--database-path', args.databaseDir, '--genesis-file', args.genesis.file, '--genesis-hash', args.genesis.hash, '--port', '' + args.listen.port, '--config', args.configFile].concat(args.listen.address ? ['--host-addr', args.listen.address] : []).concat(args.validateDb || false ? ['--validate-db'] : []).concat(args.signingKey ? ['--signing-key', args.signingKey] : []).concat(args.delegationCertificate ? ['--delegation-certificate', args.delegationCertificate] : []).concat(args.extra || []),
        supportsCleanShutdown: false,
        // set working directory to stateDir -- config file may have relative paths for logs.
        cwd: stateDir
      };
    });
  } catch (e) {
    return Promise.reject(e);
  }
};
var networks = {
  mainnet: {
    configFile: 'configuration-mainnet.yaml',
    genesisFile: 'mainnet-genesis.json',
    genesisHash: '5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb',
    topologyFile: 'mainnet-topology.json'
  }
};
/**
 * Convert a [[ByronNodeConfig]] into command-line arguments
 * ([[ByronNodeArgs]]) for `cardano-node`.
 */

function makeArgs(stateDir, config, listenPort) {
  if (!config.socketDir) {
    config.socketDir = 'sockets'; // relative to working directory
  }

  return {
    socketDir: config.socketDir,
    topologyFile: path.join(config.configurationDir, config.network.topologyFile),
    databaseDir: 'chain',
    genesis: {
      file: path.join(config.configurationDir, config.network.genesisFile),
      hash: config.network.genesisHash
    },
    listen: {
      port: listenPort
    },
    configFile: path.join(config.configurationDir, config.network.configFile)
  };
}

/**
 * Configuration for `cardano-node` (Shelley).
 *
 * @packageDocumentation
 */
var startShelleyNode = function startShelleyNode(config) {
  try {
    throw new Error('shelley backend not implemented'); // return {
    //   command: "cardano-node", args: ["--help"]
    // };
  } catch (e) {
    return Promise.reject(e);
  }
};

/**
 * Configuration for Jörmungandr.
 *
 * @packageDocumentation
 */
/**
 * Pre-defined networks for `jormungandr`. The "self" config is a
 * special one to create a local node.
 *
 * The config files are cached in the `test/data/jormungandr` directory of this repo.
 *
 * Download the latest configs from
 *   https://hydra.iohk.io/job/Cardano/iohk-nix/jormungandr-deployment/latest/download/1/index.html
 *
 */

var startJormungandr = function startJormungandr(stateDir, config) {
  try {
    var _temp3 = function _temp3() {
      var args = makeArgs$1(stateDir, config);
      return {
        command: 'jormungandr',
        args: ['--config', args.configFile, '--storage', args.storageDir].concat(args.restListen ? ['--rest-listen', args.restListen] : []).concat(args.genesisBlock.file ? ['--genesis-block', args.genesisBlock.file] : args.genesisBlock.hash ? ['--genesis-block-hash', args.genesisBlock.hash] : []).concat(_.flatMap(args.secretFile || [], function (secret) {
          return ['--secret', secret];
        })).concat(args.extra || []),
        supportsCleanShutdown: false
      };
    };

    var _temp4 = function () {
      if (!config.restPort) {
        return Promise.resolve(getPort()).then(function (_getPort) {
          config.restPort = _getPort;
        });
      }
    }();

    return Promise.resolve(_temp4 && _temp4.then ? _temp4.then(_temp3) : _temp3(_temp4));
  } catch (e) {
    return Promise.reject(e);
  }
};
var networks$1 = {
  itn_rewards_v1: {
    configFile: 'itn_rewards_v1-config.yaml',
    genesisBlock: {
      hash: '8e4d2a343f3dcf9330ad9035b3e8d168e6728904262f2c434a4f8f934ec7b676'
    }
  },
  self: {
    configFile: 'config.yaml',
    genesisBlock: {
      file: 'block0.bin',
      hash: 'f8c0622ea4b768421fea136a6e5a4e3b4c328fc5f16fad75817e40c8a2a56a56'
    },
    secretFile: ['secret.yaml']
  }
};

function makeArgs$1(stateDir, config) {
  return {
    configFile: path.join(config.configurationDir, config.network.configFile),
    restListen: "127.0.0.1:" + (config.restPort || 0),
    genesisBlock: {
      file: 'file' in config.network.genesisBlock ? path.join(config.configurationDir, config.network.genesisBlock.file) : undefined,
      hash: 'hash' in config.network.genesisBlock ? config.network.genesisBlock.hash : undefined
    },
    storageDir: path.join(stateDir, 'chain'),
    secretFile: _.map(config.network.secretFile || [], function (secret) {
      return path.join(config.configurationDir, secret);
    }),
    extra: config.extraArgs
  };
}

/**
 * Module for starting and managing a Cardano node and wallet backend.
 *
 * The main class is [[Launcher]].
 *
 * @packageDocumentation
 */

var walletExe = function walletExe(baseDir, config, node) {
  try {
    var _temp2 = function _temp2(apiPort) {
      var base = {
        command: "cardano-wallet-" + config.nodeConfig.kind,
        args: ['serve', '--shutdown-handler', '--port', '' + apiPort, '--database', path.join(baseDir, 'wallet')].concat(config.listenAddress ? ['--listen-address', config.listenAddress] : [], config.syncToleranceSeconds ? ['--sync-tolerance', config.syncToleranceSeconds + "s"] : []),
        extraEnv: config.stakePoolRegistryUrl ? {
          CARDANO_WALLET_STAKE_POOL_REGISTRY_URL: config.stakePoolRegistryUrl
        } : undefined,
        supportsCleanShutdown: true,
        apiPort: apiPort
      };

      var addArgs = function addArgs(args) {
        return _.assign(base, {
          args: base.args.concat(args)
        });
      };

      switch (config.nodeConfig.kind) {
        case 'jormungandr':
          return addArgs(['--genesis-block-hash', config.nodeConfig.network.genesisBlock.hash, '--node-port', '' + config.nodeConfig.restPort]);

        case 'byron':
          return addArgs(config.nodeConfig.socketDir ? ['--node-socket', config.nodeConfig.socketDir] : []);

        case 'shelley':
          return base;
      }
    };

    var _config$apiPort2 = config.apiPort;
    return Promise.resolve(_config$apiPort2 ? _temp2(_config$apiPort2) : Promise.resolve(getPort()).then(_temp2));
  } catch (e) {
    return Promise.reject(e);
  }
};
/**
 * This is the main object which controls the launched wallet backend
 * and its node.
 *
 * Example:
 *
 * ```javascript
 * var launcher = new cardanoLauncher.Launcher({
 *   networkName: "mainnet",
 *   stateDir: "/tmp/state-launcher",
 *   nodeConfig: {
 *     kind: "byron",
 *     configurationDir: "/home/user/cardano-node/configuration",
 *     network: {
 *       configFile: "configuration-mainnet.yaml",
 *       genesisFile: "mainnet-genesis.json",
 *       genesisHash: "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb",
 *       topologyFile: "mainnet-topology.json"
 *     }
 *   }
 * });
 * ```
 *
 * Initially, the backend is not started. Use [[Launcher.start]] for that.
 */

var Launcher = /*#__PURE__*/function () {
  /**
   * Sets up a Launcher which can start and control the wallet backend.
   *
   * @param config - controls how the wallet and node are started
   * @param logger - logging backend that launcher will use
   */
  function Launcher(config, logger) {
    var _this = this;

    if (logger === void 0) {
      logger = console;
    }

    /** Wallet API server port - set once it's known. */
    this.apiPort = 0;
    logger.debug('Launcher init');
    this.logger = logger;
    var start = makeServiceCommands(config, logger);
    this.walletService = setupService(start.wallet, prependName(logger, 'wallet'));
    this.nodeService = setupService(start.node, prependName(logger, 'node'));
    this.walletBackend = {
      getApi: function getApi() {
        return new V2Api(_this.apiPort);
      },
      events: new tsee.EventEmitter()
    };
    start.wallet.then(function (startService) {
      _this.apiPort = startService.apiPort;
    });
    this.walletService.events.on('statusChanged', function (status) {
      if (status === exports.ServiceStatus.Stopped) {
        _this.logger.debug('wallet exited');

        _this.stop();
      }
    });
    this.nodeService.events.on('statusChanged', function (status) {
      if (status === exports.ServiceStatus.Stopped) {
        _this.logger.debug('node exited');

        _this.stop();
      }
    });
    this.installSignalHandlers();
  }
  /**
   * Starts the wallet and node.
   *
   * Example:
   *
   * ```javascript
   * launcher.start().then(function(api) {
   *   console.log("*** cardano-wallet backend is ready, base URL is " + api.baseUrl);
   * });
   * ```
   *
   * @return a promise that will be fulfilled when the wallet API
   * server is ready to accept requests.
   */


  var _proto = Launcher.prototype;

  _proto.start = function start() {
    var _this2 = this;

    this.nodeService.start();
    this.walletService.start();
    this.waitForApi().then(function () {
      _this2.walletBackend.events.emit('ready', _this2.walletBackend.getApi());
    });
    return new Promise(function (resolve, reject) {
      _this2.walletBackend.events.on('ready', resolve);

      _this2.walletBackend.events.on('exit', reject);
    });
  }
  /**
   * Poll TCP port of wallet API server until it accepts connections.
   * @param port - TCP port number
   * @return a promise that is completed once the wallet API server accepts connections.
   */
  ;

  _proto.waitForApi = function waitForApi() {
    var _this3 = this;

    this.logger.debug('waitForApi');
    return new Promise(function (resolve) {
      var addr;
      var client;

      var poll = function poll() {
        if (_this3.apiPort) {
          if (!addr) {
            addr = {
              port: _this3.apiPort,
              host: '127.0.0.1'
            };

            _this3.logger.info("Waiting for tcp port " + addr.host + ":" + addr.port + " to accept connections...");
          }

          if (client) {
            client.destroy();
          }

          client = new net.Socket();
          client.connect(addr, function () {
            _this3.logger.info("... port is ready.");

            clearInterval(timer);
            resolve();
          });
          client.on('error', function (err) {
            _this3.logger.debug("waitForApi: not ready yet: " + err);
          });
        }
      };

      var timer = setInterval(poll, 250);
    });
  }
  /**
   * Stops the wallet backend. Attempts to cleanly shut down the
   * processes. However, if they have not exited before the timeout,
   * they will be killed.
   *
   * @param timeoutSeconds - how long to wait before killing the processes.
   * @return a [[Promise]] that is fulfilled at the timeout, or before.
   *
   * @event exit - `walletBackend.events` will emit this when the
   *   wallet and node have both exited.
   */
  ;

  _proto.stop = function stop(timeoutSeconds) {
    var _this4 = this;

    if (timeoutSeconds === void 0) {
      timeoutSeconds = 60;
    }

    this.logger.debug("Launcher.stop: stopping wallet and node");
    return Promise.all([this.walletService.stop(timeoutSeconds), this.nodeService.stop(timeoutSeconds)]).then(function (_ref) {
      var wallet = _ref[0],
          node = _ref[1];
      var status = {
        wallet: wallet,
        node: node
      };

      _this4.logger.debug("Launcher.stop: both services are stopped.", status);

      _this4.walletBackend.events.emit('exit', status);

      return status;
    });
  }
  /**
   * Stop services when this process gets killed.
   */
  ;

  _proto.installSignalHandlers = function installSignalHandlers() {
    var _this5 = this;

    var cleanup = function cleanup(signal) {
      _this5.logger.info("Received " + signal + " - stopping services...");

      _this5.walletService.stop(0);

      _this5.nodeService.stop(0);
    };

    ['SIGINT', 'SIGTERM', 'SIGHUP', 'SIGBREAK'].forEach(function (signal) {
      return process$1.on(signal, cleanup);
    });
  };

  return Launcher;
}();

var V2Api = function V2Api(port) {
  var hostname = '127.0.0.1';
  var path = '/v2/';
  this.baseUrl = "http://" + hostname + ":" + port + path;
  this.requestParams = {
    port: port,
    path: path,
    hostname: hostname
  };
};
/**
 * Format an [[ExitStatus]] as a multiline human-readable string.
 */


function exitStatusMessage(status) {
  return _.map(status, serviceExitStatusMessage).join('\n');
}

function makeServiceCommands(config, logger) {
  var baseDir = path.join(config.stateDir, config.nodeConfig.kind, config.networkName);
  logger.info("Creating base directory " + baseDir + " (if it doesn't already exist)");
  var node = mkdirp(baseDir).then(function () {
    return nodeExe(baseDir, config);
  });
  var wallet = node.then(function (nodeService) {
    return walletExe(baseDir, config);
  });
  return {
    wallet: wallet,
    node: node
  };
}

function nodeExe(baseDir, config) {
  switch (config.nodeConfig.kind) {
    case 'jormungandr':
      return startJormungandr(baseDir, config.nodeConfig);

    case 'byron':
      return startByronNode(baseDir, config.nodeConfig);

    case 'shelley':
      return startShelleyNode();
  }
}

/**
 * `cardano-launcher` command-line interface.
 *
 * This tool can be used for testing.
 *
 * See also: the entrypoint script `bin/cardano-launcher`.
 *
 * @packageDocumentation
 */

function combineStatus(statuses) {
  var code = _.reduce(statuses, function (res, status) {
    return res === null ? status.code : res;
  }, null);

  var signal = _.reduce(statuses, function (res, status) {
    return res === null ? status.signal : res;
  }, null); // let err = _.reduce(statuses, (res, status) => res === null ? status.err : res, null);


  return code === null ? signal === null ? 0 : 127 : code;
}
/**
 * Main function of the CLI.
 *
 * Is just a very basic interface for testing things.
 */


function cli(args) {
  var waitForExit = setInterval(function () {}, 3600000);
  args.shift(); // /usr/bin/node

  args.shift(); // cardano-launcher

  if (args.length < 4) {
    usage();
  }

  var backend = args.shift();
  var networkName = args.shift();
  var configurationDir = args.shift();
  var stateDir = args.shift();
  var nodeConfig;

  if (backend === 'byron') {
    if (!(networkName in networks)) {
      console.error("unknown network: " + networkName);
      process$1.exit(2);
    }

    var network = networks[networkName];
    nodeConfig = {
      kind: backend,
      configurationDir: configurationDir,
      network: network
    };
  } else if (backend === 'jormungandr') {
    if (!(networkName in networks$1)) {
      console.error("unknown network: " + networkName);
      process$1.exit(2);
    }

    var _network = networks$1[networkName];
    nodeConfig = {
      kind: backend,
      configurationDir: configurationDir,
      network: _network
    };
  } else {
    usage();
  }

  var launcher = new Launcher({
    stateDir: stateDir,
    nodeConfig: nodeConfig,
    networkName: networkName
  }, console);
  launcher.start(); // inform tests of subprocess pids

  launcher.nodeService.start().then(function (pid) {
    return sendMaybe({
      node: pid
    });
  });
  launcher.walletService.start().then(function (pid) {
    return sendMaybe({
      wallet: pid
    });
  });
  launcher.walletBackend.events.on('exit', function (status) {
    console.log(serviceExitStatusMessage(status.wallet));
    console.log(serviceExitStatusMessage(status.node));
    clearInterval(waitForExit);
    process$1.exit(combineStatus([status.wallet, status.node]));
  });
}

function usage() {
  console.log('usage: cardano-launcher BACKEND NETWORK CONFIG-DIR STATE-DIR');
  console.log('  BACKEND    - either jormungandr or byron');
  console.log('  NETWORK    - depends on backend, e.g. mainnet, itn_rewards_v1');
  console.log('  CONFIG-DIR - directory which contains config files for a backend');
  console.log('  STATE-DIR  - directory to put blockchains, databases, etc.');
  process$1.exit(1);
}

function sendMaybe(message) {
  if (process$1.send) {
    process$1.send(message);
  }
}

exports.Launcher = Launcher;
exports.cli = cli;
exports.exitStatusMessage = exitStatusMessage;
exports.serviceExitStatusMessage = serviceExitStatusMessage;
//# sourceMappingURL=cardano-launcher.cjs.development.js.map
