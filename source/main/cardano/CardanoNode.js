// @flow
import { readFileSync } from 'fs';
import { spawn } from 'child_process';
import type { ChildProcess } from 'child_process';
import type { WriteStream } from 'fs';
import type { TlsConfig } from '../../common/ipc-api/tls-config';
import { TLS_CONFIG_CHANNEL } from '../../common/ipc-api/tls-config';

type NodeArgs = Array<string>;

type Logger = {
  debug: (string) => void,
  info: (string) => void,
  error: (string) => void,
};

type IpcChannel = {
  send: (string, any) => void,
  on: (string, Function) => void,
};

type NodeIpcMessage = {
  Started?: Array<any>,
  ReplyPort?: number,
}

export class CardanoNode {
  /**
   * The ipc channel to use for broadcasting messages
   * @private
   */
  ipcChannel: IpcChannel;
  /**
   * Logger instance to print debug messages to
   * @private
   */
  log: Logger;
  /**
   * The managed cardano-node child process
   * @private
   */
  node: ChildProcess;

  /**
   * The TLS config used by the renderer processes to connect to
   * the cardano-node over HTTPS
   */
  tlsConfig: TlsConfig;

  constructor(ipcChannel: IpcChannel, log: Logger) {
    this.ipcChannel = ipcChannel;
    this.log = log;
    this.tlsConfig = {
      ca: null,
      key: null,
      cert: null,
      port: null,
    };
  }

  start(nodePath: string, tlsPath: string, args: NodeArgs, logFile: WriteStream) {
    const { log } = this;
    logFile.on('open', () => {

      // Spawning cardano-node
      const jsonArgs = JSON.stringify(args);
      log.info(`Spawning cardano-node from path: ${nodePath} with args: ${jsonArgs}.`);
      this.node = this._spawnNode(nodePath, args, logFile);
      log.info('CardanoNode spawned.');

      this.node.on('message', (msg: NodeIpcMessage) => {
        log.info(`CardanoNode: received message: ${JSON.stringify(msg)}`);
        if (msg.Started) {
          log.info('CardanoNode: started, TLS certs updated');
          Object.assign(this.tlsConfig, {
            ca: readFileSync(tlsPath + '/client/ca.crt'),
            key: readFileSync(tlsPath + '/client/client.key'),
            cert: readFileSync(tlsPath + '/client/client.pem'),
          });
        } else if (msg.ReplyPort) {
          this.tlsConfig.port = msg.ReplyPort;
          this.broadcastTlsConfig();
        }
      });

      this.node.on('close', (code, signal) => {
        log.info(`CardanoNode: all stdio to child has been closed with: ${code}, ${signal}`);
      });

      this.node.on('disconnect', () => {
        log.info('CardanoNode: all IPC handles closed');
      });

      this.node.on('error', (err) => {
        log.info(`CardanoNode: error: ${err}`);
      });

      this.node.on('exit', (code, signal) => {
        // TODO: give a better UI when it fails and auto-retry a few times
        log.info(`CardanoNode: child exited with ${code}, ${signal}`);
      });

      this.node.send({ QueryPort: [] });
    });
  }

  stop() {
    const { log, node } = this;
    if (node) {
      log.info('CardanoNode: disconnecting IPC channel');
      node.disconnect();
    }
  }

  broadcastTlsConfig() {
    this.ipcChannel.send(TLS_CONFIG_CHANNEL, this.tlsConfig);
  }

  // =============== PRIVATE ===================

  /**
   * Spawns cardano-node as child_process in ipc mode
   * @param nodePath {string}
   * @param args {NodeArgs}
   * @param logFile {WriteStream}
   * @returns {ChildProcess}
   * @private
   */
  _spawnNode(nodePath: string, args: NodeArgs, logFile: WriteStream) {
    return spawn(nodePath, args, { stdio: ['inherit', logFile, logFile, 'ipc'] });
  }

}
