// Copyright © 2020 IOHK
// License: Apache-2.0

/**
 * Functions for starting and stopping an individual backend service.
 *
 * The important function is [[setupService]] which creates a [[Service]].
 *
 * @packageDocumentation
 */

import { spawn, ChildProcess, StdioOptions } from 'child_process';
import { WriteStream } from 'fs';
import { Writable } from 'stream';
import _ from 'lodash';
import { EventEmitter } from 'tsee';

import { Logger } from './logging';

export interface ServiceExitStatus {
  /** Program name. */
  exe: string;
  /** Process exit status code, if process exited itself. */
  code: number | null;
  /** Signal name, if process was killed. */
  signal: string | null;
  /** Error object, if process could not be started, or could not be killed. */
  err: Error | null;
}

/**
 * Produce an exit message from an exit status.
 * @param res - exit status of service.
 * @return a human readable exit message.
 */
export function serviceExitStatusMessage(res: ServiceExitStatus): string {
  const reason =
    typeof res.code === 'number'
      ? `status ${res.code}`
      : res.signal
      ? `signal ${res.signal}`
      : `error ${res.err}`;

  return `${res.exe} exited with ${reason}`;
}

/**
 * States for a launched process.  The processes are not guaranteed to
 * use all of these states. For example, a process may go directly
 * from `Started` to `Stopped`.
 */
export enum ServiceStatus {
  /** Initial state. */
  NotStarted,
  /** Waiting for [[StartService]] info. */
  Starting,
  /** Subprocess has been started and has a PID. */
  Started,
  /** Caller has requested to stop the process. Now waiting for it to exit, or for the timeout to elapse. */
  Stopping,
  /** Subprocess has exited or been killed. */
  Stopped,
}

/**
 * A launched process.
 */
export interface Service {
  /**
   * @return - a promise that will be fulfilled when the process has
   *   started. The returned PID is not guaranteed to be running. It
   *   may already have exited.
   */
  start(): Promise<Pid>;

  /**
   * Stops the process.
   *
   * If the process doesn't exit within `timeoutSeconds`, it will be
   * killed. The default timeout is [[defaultTimeoutSeconds]].
   *
   * @param timeoutSeconds - How long to wait for the service to stop
   *   itself before killing it. If `0`, any running timeout kill be
   *   cancelled and the process killed immediately.
   * @return a promise that will be fulfilled when the process has stopped.
   */
  stop(timeoutSeconds?: number): Promise<ServiceExitStatus>;

  /**
   * Waits for the process to finish somehow -- whether it exits by
   * itself, or exits due to `stop()` being called.
   *
   * @return a promise that will be fulfilled when the process has exited.
   */
  waitForExit(): Promise<ServiceExitStatus>;

  /**
   * @return the status of this process.
   */
  getStatus(): ServiceStatus;

  /**
   * @return the ChildProcess running the service, or null if
   *   the service has not been started yet.
   */
  getProcess(): ChildProcess | null;

  /** @return the 'StartService' configuration, or null if the service
   *   has not been configured yet.
   */
  getConfig(): StartService | null;

  /**
   * An [`EventEmitter`](https://nodejs.org/api/events.html#class-eventemitter)
   * that can be used to register handlers when
   * the process changes status.
   *
   * ```typescript
   * launcher.walletService.events.on('statusChanged', (status: ServiceStatus) => { ... });
   * ```
   */
  events: ServiceEvents;
}

/** Process ID */
export type Pid = number;

/**
 * The type of events for [[Service]].
 */
export class ServiceEvents extends EventEmitter<{
  statusChanged: (status: ServiceStatus) => void;
}> {
  /**
   * [[Launcher.walletService.events]] and
   * [[Launcher.nodeService.events]] will emit this when their
   * processes start or stop.
   * @event
   */
  statusChanged(status: ServiceStatus): void {
    this.emit('statusChanged', status);
  }
}

/**
 * How to stop a service.
 */
export enum ShutdownMethod {
  /** Terminate the process with a signal (not clean on Windows) */
  Signal,
  /** Terminate the process by closing its stdin. */
  CloseStdin,
  /** Terminate the process by closing an inherited file descriptor. */
  CloseFD,
}

/**
 * The file number that [[Service]] will use for child process
 * notification when [[ShutdownMethod.CloseFD]] is in use.
 */
export const cleanShutdownFD = 3;

/**
 * The value for the `timeoutSeconds` argument of [[Service.stop]].
 */
export const defaultTimeoutSeconds = 300;

/**
 * Initialise a [[Service]] which can control the lifetime of a
 * backend process.
 *
 * This does not start the process. Use [[Service.start]] for that.
 *
 * @param cfgPromise - a promise which will return the command to run.
 * @param logger - logging object.
 * @param childProcessLogWriteStream - WriteStream for writing the child process data events from stdout and stderr.
 * @return A handle on the [[Service]].
 */
export function setupService(
  cfgPromise: Promise<StartService>,
  logger: Logger = console,
  childProcessLogWriteStream?: WriteStream
): Service {
  const events = new ServiceEvents();
  // What the current state is.
  let status = ServiceStatus.NotStarted;
  // Fulfilled promise of service command-line.
  // This will always be defined if status > Starting.
  let cfg: StartService;
  // NodeJS child process object, or null if not running.
  let proc: ChildProcess | null = null;
  // Pipe file descriptor for clean shutdown, or null if not yet running.
  let shutdownFD: number | null;
  // When the service started (milliseconds since epoch)
  let startTimeMs = 0;
  // How the child process exited, or null if it hasn't yet exited.
  let exitStatus: ServiceExitStatus | null;
  // For cancelling the kill timeout.
  let killTimer: NodeJS.Timeout | null = null;
  let startPromise: Promise<Pid>;

  const setStatus = (newStatus: ServiceStatus): void => {
    logger.debug(
      `setStatus ${ServiceStatus[status]} -> ${ServiceStatus[newStatus]}`
    );
    status = newStatus;
    if (status === ServiceStatus.Started) {
      startTimeMs = Date.now();
    }
    events.statusChanged(status);
  };

  const onStopped = (
    code: number | null = null,
    signal: string | null = null,
    err: Error | null = null
  ): void => {
    exitStatus = { exe: cfg.command, code, signal, err };
    logger.debug(`Service onStopped`, exitStatus);
    if (killTimer) {
      clearTimeout(killTimer);
      killTimer = null;
    }
    proc = null;
    setStatus(ServiceStatus.Stopped);
  };

  const doStart = async (): Promise<Pid> => {
    const envStr = _.map(
      cfg.extraEnv,
      (value, name) => `${name}=${value} `
    ).join('');
    const commandStr = `${envStr}${cfg.command} ${cfg.args.join(' ')}`;
    logger.info(`Service.start: trying to start ${commandStr}`, cfg);
    const stdOuts = childProcessLogWriteStream ? 'pipe' : 'inherit';
    const stdio = [
      cfg.shutdownMethod === ShutdownMethod.CloseStdin ? 'pipe' : 'ignore',
      stdOuts,
      stdOuts,
    ].concat(
      cfg.shutdownMethod === ShutdownMethod.CloseFD ? ['pipe'] : []
    ) as StdioOptions;
    const cwd = cfg.cwd ? { cwd: cfg.cwd } : {};
    const env = cfg.extraEnv
      ? Object.assign({}, process.env, cfg.extraEnv)
      : process.env;
    const options = Object.assign({ stdio }, cwd, { env });
    try {
      proc = spawn(cfg.command, cfg.args, options);
    } catch (err) {
      logger.error(`Service.start: child_process.spawn() failed: ${err}`);
      logger.error(
        `Service.start: child_process.spawn(${cfg.command}, ${cfg.args.join(
          ' '
        )}, ...)`,
        options
      );
      throw err;
    }
    if (cfg.shutdownMethod === ShutdownMethod.CloseStdin) {
      // corresponds to first element of `stdio` above
      shutdownFD = 0;
    } else if (cfg.shutdownMethod === ShutdownMethod.CloseFD) {
      // corresponds to last element of `stdio` above
      shutdownFD = cleanShutdownFD;
    }
    setStatus(ServiceStatus.Started);
    proc.on('exit', (code, signal) => {
      onStopped(code, signal);
    });
    proc.on('error', (err) => {
      logger.error(`Service.start: child_process failed: ${err}`);
      onStopped(null, null, err);
    });
    if (proc.stdout && proc.stderr && childProcessLogWriteStream) {
      proc.stdout.on('data', (data) => {
        childProcessLogWriteStream.write(data);
      });
      proc.stderr.on('data', (data) => {
        childProcessLogWriteStream.write(data);
      });
    }
    return proc.pid as number;
  };

  const doStop = (timeoutSeconds: number): void => {
    logger.info(`Service.stop: trying to stop ${cfg.command}`, cfg);
    setStatus(ServiceStatus.Stopping);
    if (proc) {
      if (cfg.shutdownMethod === ShutdownMethod.Signal) {
        proc.kill('SIGTERM');
      } else if (shutdownFD !== null && proc.stdio[shutdownFD]) {
        const stream = proc.stdio[shutdownFD] as Writable;
        const closeFD = (): void => {
          stream.end();
        };

        // Allow the service one second after startup to begin reading from its
        // shutdownFD, before closing the shutdown FD.
        const shutdownFDGracePeriodMs = 1000;
        const grace = startTimeMs - Date.now() + shutdownFDGracePeriodMs;
        if (grace > 0) {
          setTimeout(closeFD, grace);
        } else {
          closeFD();
        }
      }
    }
    killTimer = setTimeout(() => {
      if (proc) {
        logger.info(
          `Service.stop: timed out after ${timeoutSeconds} seconds. Killing process ${proc.pid}.`
        );
        proc.kill('SIGKILL');
      }
    }, timeoutSeconds * 1000);
  };

  const waitForStop = (): Promise<ServiceExitStatus> =>
    new Promise((resolve) => {
      logger.debug(`Service.stop: waiting for ServiceStatus.Stopped`);
      events.on('statusChanged', (status) => {
        if (status === ServiceStatus.Stopped && exitStatus) {
          resolve(exitStatus);
        }
      });
    });

  const waitForExit = (): Promise<ServiceExitStatus> => {
    const defaultExitStatus = {
      exe: cfg ? cfg.command : '',
      code: null,
      signal: null,
      err: null,
    };
    switch (status) {
      case ServiceStatus.NotStarted:
      case ServiceStatus.Starting:
        return new Promise((resolve) => {
          status = ServiceStatus.Stopped;
          exitStatus = defaultExitStatus;
          resolve(exitStatus);
        });
      case ServiceStatus.Started:
        return waitForStop();
      case ServiceStatus.Stopping:
        return waitForStop();
      case ServiceStatus.Stopped:
        return new Promise((resolve) =>
          resolve(exitStatus || defaultExitStatus)
        );
    }
  };

  return {
    start: async (): Promise<Pid> => {
      switch (status) {
        case ServiceStatus.NotStarted:
          setStatus(ServiceStatus.Starting);
          startPromise = cfgPromise.then((theCfg) => {
            cfg = theCfg;
            return doStart();
          });
          return startPromise;
        case ServiceStatus.Starting:
          logger.info(`Service.start: already starting`);
          return startPromise;
        case ServiceStatus.Started:
          logger.info(`Service.start: already started`);
          return proc?.pid || -1;
        case ServiceStatus.Stopping:
          logger.info(`Service.start: cannot start - already stopping`);
          return -1;
        case ServiceStatus.Stopped:
          logger.info(`Service.start: cannot start - already stopped`);
          return -1;
      }
    },
    stop: async (
      timeoutSeconds = defaultTimeoutSeconds
    ): Promise<ServiceExitStatus> => {
      switch (status) {
        case ServiceStatus.NotStarted:
        case ServiceStatus.Starting:
          logger.info(`Service.stop: cannot stop - never started`);
          break;
        case ServiceStatus.Started:
          doStop(timeoutSeconds);
          break;
        case ServiceStatus.Stopping:
          if (timeoutSeconds === 0 && proc) {
            logger.info(
              `Service.stop: was already stopping, but will now kill process ${proc.pid} immediately`
            );
            proc.kill('SIGKILL');
          } else {
            logger.info(`Service.stop: already stopping`);
          }
          break;
        case ServiceStatus.Stopped:
          logger.info(`Service.stop: already stopped`);
          break;
      }
      return waitForExit();
    },
    waitForExit,
    getStatus: (): ServiceStatus => status,
    getProcess: (): ChildProcess | null => proc,
    getConfig: (): StartService | null => cfg,
    events,
  };
}

/**
 * Describes the command to run for the service.
 */
export interface StartService {
  /** Program name. Will be searched for in `PATH`. */
  command: string;
  /** Command-line arguments. */
  args: string[];
  /** Directory to start program in. Helpful if it outputs files. */
  cwd?: string;
  /** Additional environment variables to set, on top of the current process environment. */
  extraEnv?: { [propName: string]: string };
  /**
   * Whether this service supports the clean shutdown method documented in
   * `docs/windows-clean-shutdown.md`.
   */
  shutdownMethod: ShutdownMethod;
}
