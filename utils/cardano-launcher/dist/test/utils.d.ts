import { Service, ServiceStatus, Api } from '../src';
import { StartService } from '../src/service';
import { Logger } from '../src/logging';
/*******************************************************************************
 * Utils
 ******************************************************************************/
/** Construct a promise to a service command. */
export declare function testService(command: string, args: string[], supportsCleanShutdown?: boolean): Promise<StartService>;
/**
 * Expect the given process ID to not exist.
 */
export declare const expectProcessToBeGone: (pid: number, signal?: number) => void;
/**
 * @return mutable array which will contain events as they occur.
 */
export declare const collectEvents: (service: Service) => ServiceStatus[];
export interface MockLog {
    severity: 'debug' | 'info' | 'error';
    msg: string;
    param: object | undefined;
}
export interface MockLogger extends Logger {
    getLogs(): MockLog[];
}
export declare function mockLogger(echo?: boolean): MockLogger;
export declare function delay(ms: number): Promise<unknown>;
/**
 * Sets up the parameters for `http.request` for this Api.
 *
 * @param path - the api route (without leading slash)
 * @param options - extra options to be added to the request.
 * @return an options object suitable for `http.request`
 */
export declare function makeRequest(api: Api, path: string, options?: object): object;
