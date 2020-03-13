/**
 * Functions for starting and stopping an individual backend service.
 *
 * The important function is [[setupService]] which creates a [[Service]].
 *
 * @packageDocumentation
 */
/// <reference types="node" />
import { ChildProcess } from 'child_process';
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
export declare function serviceExitStatusMessage(res: ServiceExitStatus): string;
/**
 * States for a launched process.  The processes are not guaranteed to
 * use all of these states. For example, a process may go directly
 * from `Started` to `Stopped`.
 */
export declare enum ServiceStatus {
    /** Initial state. */
    NotStarted = 0,
    /** Waiting for [[StartService]] info. */
    Starting = 1,
    /** Subprocess has been started and has a PID. */
    Started = 2,
    /** Caller has requested to stop the process. Now waiting for it to exit, or for the timeout to elapse. */
    Stopping = 3,
    /** Subprocess has exited or been killed. */
    Stopped = 4
}
/**
 * A launched process.
 */
export interface Service {
    /**
     * @return a promise that will be fulfilled when the process has
     *   started. The returned PID is not guaranteed to be running. It may
     *   already have exited.
     */
    start(): Promise<Pid>;
    /**
     * Stops the process.
     * @param timeoutSeconds - how long to wait for the service to stop itself before killing it.
     *   If `0`, any running timeout kill be cancelled and the process killed immediately.
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
     * @return the ChildProcess running the service, or null if the service has not been started yet.
     */
    getProcess(): ChildProcess | null;
    /**
     * An [[EventEmitter]] that can be used to register handlers when
     * the process changes status.
     *
     * ```typescript
     * launcher.walletService.events.on('statusChanged', status => { ... });
     * ```
     */
    events: ServiceEvents;
}
/** Process ID */
export declare type Pid = number;
/**
 * The type of events for [[Service]].
 */
declare type ServiceEvents = EventEmitter<{
    /**
     * [[Launcher.walletService.events]] and
     * [[Launcher.nodeService.events]] will emit this when their
     * processes start or stop.
     * @event
     */
    statusChanged: (status: ServiceStatus) => void;
}>;
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
export declare function setupService(cfgPromise: Promise<StartService>, logger?: Logger): Service;
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
    extraEnv?: {
        [propName: string]: string;
    };
    /**
     * Whether this service supports the clean shutdown method documented in
     * `docs/windows-clean-shutdown.md`.
     */
    supportsCleanShutdown: boolean;
}
export {};
