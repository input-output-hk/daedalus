import { spawn } from 'child_process';
import * as net from 'net';
import * as path from 'path';
import type { NodeConfig } from '../config';
import { logger } from '../utils/logging';

export type WatchdogOptions = {
  watchdogBin: string;
  nodeBin: string;
  walletBin: string;
  stateDir: string;
  nodeConfig: NodeConfig;
  tlsPath: string;
  configPath: string;
  syncTolerance: string;
  isStaging: boolean;
  cluster: string;
  metadataUrl?: string;
  rtsFlags: Array<string>;
  nodeLogFile: string;
  walletLogFile: string;
};

export type WatchdogHandle = {
  pid: number;
  wpid: number;
  connected: boolean;
  walletPort: number;
  nodeStartedAt: number | null;
  walletStartedAt: number | null;
  walletRestartCount: number;
  stop(timeoutSeconds?: number): Promise<void>;
  kill(): void;
  send(msg: object): void;
};

type WatchdogEvent =
  | { event: 'node_started'; pid: number; started_at_unix_ms: number }
  | { event: 'wallet_started'; pid: number; started_at_unix_ms: number }
  | { event: 'wallet_ready'; port: number }
  | { event: 'wallet_exited'; code: number | null; signal: string | null }
  | { event: 'wallet_restarting'; attempt: number }
  | { event: 'node_exited'; code: number | null; signal: string | null }
  | { event: 'stopped' }
  | { event: 'error'; message: string };

function getFreePort(): Promise<number> {
  return new Promise((resolve, reject) => {
    const srv = net.createServer();
    srv.listen(0, '127.0.0.1', () => {
      const port = (srv.address() as net.AddressInfo).port;
      srv.close(() => resolve(port));
    });
    srv.on('error', reject);
  });
}

function buildNodeArgs(
  opts: WatchdogOptions,
  nodePort: number
): string[] {
  const { stateDir, nodeConfig, rtsFlags } = opts;
  const { configFile, topologyFile } = nodeConfig.network;
  const rtsOpts = [...(rtsFlags ?? []), '-N'];

  const args = [
    'run',
    '--socket-path', 'cardano-node.socket',
    '--topology', topologyFile,
    '--database-path', 'chain',
    '--port', String(nodePort),
    '--config', configFile,
  ];

  if (nodeConfig.signingKey) {
    args.push('--signing-key', nodeConfig.signingKey);
  }
  if (nodeConfig.delegationCertificate) {
    args.push('--delegation-certificate', nodeConfig.delegationCertificate);
  }

  args.push('+RTS', ...rtsOpts, '-RTS');
  return args;
}

function buildWalletArgs(
  opts: WatchdogOptions,
  walletPort: number
): string[] {
  const { stateDir, tlsPath, syncTolerance, metadataUrl, isStaging, nodeConfig } = opts;
  const socketPath = path.join(stateDir, 'cardano-node.socket');
  const walletDb = path.join(stateDir, 'wallets');
  const syncToleranceSecs = parseInt(syncTolerance.replace('s', ''), 10);
  const configDir = path.dirname(nodeConfig.network.configFile);

  const args = [
    'serve',
    '+RTS', '-N', '-RTS',
    '--port', String(walletPort),
    '--database', walletDb,
    '--tls-ca-cert', path.join(tlsPath, 'server/ca.crt'),
    '--tls-sv-cert', path.join(tlsPath, 'server/server.crt'),
    '--tls-sv-key', path.join(tlsPath, 'server/server.key'),
    '--node-socket', socketPath,
  ];

  if (isStaging) {
    args.push('--mainnet');
  } else {
    args.push('--testnet', path.join(configDir, 'genesis-byron.json'));
  }

  if (!isNaN(syncToleranceSecs)) {
    args.push('--sync-tolerance', `${syncToleranceSecs}s`);
  }

  const tokenMetadataServer = metadataUrl ?? 'https://tokens.cardano.org';
  args.push('--token-metadata-server', tokenMetadataServer);

  return args;
}

export async function startWatchdog(
  opts: WatchdogOptions,
  onNodeExited: (code: number | null, signal: string | null) => void,
  onWalletExited: (code: number | null, signal: string | null) => void,
  onWalletRestarted?: () => void
): Promise<WatchdogHandle> {
  const [nodePort, walletPort] = await Promise.all([getFreePort(), getFreePort()]);

  const watchdogConfig = {
    node: {
      exe: opts.nodeBin,
      args: buildNodeArgs(opts, nodePort),
      state_dir: opts.stateDir,
      socket_path: path.join(opts.stateDir, 'cardano-node.socket'),
    },
    wallet: {
      exe: opts.walletBin,
      args: buildWalletArgs(opts, walletPort),
      state_dir: opts.stateDir,
      api_port: walletPort,
      restart_delay_ms: 1000,
    },
    node_log_file: opts.nodeLogFile,
    wallet_log_file: opts.walletLogFile,
  };

  const proc = spawn(opts.watchdogBin, [], {
    stdio: ['pipe', 'pipe', 'pipe'],
    detached: false,
  });

  // Log watchdog stderr
  proc.stderr?.on('data', (chunk: Buffer) => {
    logger.info('[watchdog]', { msg: chunk.toString().trim() });
  });

  // Write config as first stdin line
  proc.stdin?.write(JSON.stringify(watchdogConfig) + '\n');

  const handle: WatchdogHandle = {
    pid: 0,
    wpid: 0,
    connected: false,
    walletPort,
    nodeStartedAt: null,
    walletStartedAt: null,
    walletRestartCount: 0,
    stop(timeoutSeconds = 30) {
      return new Promise((resolve) => {
        proc.stdin?.write(JSON.stringify({ cmd: 'stop' }) + '\n');
        const t = setTimeout(() => {
          proc.kill('SIGKILL');
          resolve();
        }, (timeoutSeconds * 1000));
        proc.once('exit', () => { clearTimeout(t); resolve(); });
      });
    },
    kill() {
      proc.kill('SIGKILL');
    },
    send(_msg: object) {
      // Fault injection not yet supported via watchdog IPC
    },
  };

  return new Promise((resolve, reject) => {
    let resolved = false;

    const rl = require('readline').createInterface({ input: proc.stdout });
    rl.on('line', (line: string) => {
      let ev: WatchdogEvent;
      try {
        ev = JSON.parse(line);
      } catch {
        return;
      }

      logger.info('watchdog event', { ev });

      switch (ev.event) {
        case 'node_started':
          handle.pid = ev.pid;
          handle.nodeStartedAt = ev.started_at_unix_ms;
          break;
        case 'wallet_started':
          handle.wpid = ev.pid;
          handle.walletStartedAt = ev.started_at_unix_ms;
          break;
        case 'wallet_ready':
          handle.connected = true;
          if (!resolved) {
            resolved = true;
            resolve(handle);
          } else {
            onWalletRestarted?.();
          }
          break;
        case 'wallet_exited':
          handle.connected = false;
          onWalletExited(ev.code, ev.signal);
          break;
        case 'wallet_restarting':
          handle.walletRestartCount = ev.attempt;
          logger.info(`watchdog restarting wallet (attempt ${ev.attempt})`);
          break;
        case 'node_exited':
          onNodeExited(ev.code, ev.signal);
          break;
        case 'error':
          logger.error('watchdog error', { message: ev.message });
          if (!resolved) {
            resolved = true;
            reject(new Error(ev.message));
          }
          break;
      }
    });

    proc.on('exit', (code, signal) => {
      handle.connected = false;
      if (!resolved) {
        resolved = true;
        reject(new Error(`cardano-watchdog exited unexpectedly (code=${code}, signal=${signal})`));
      }
    });

    proc.on('error', (err) => {
      if (!resolved) {
        resolved = true;
        reject(err);
      }
    });
  });
}
