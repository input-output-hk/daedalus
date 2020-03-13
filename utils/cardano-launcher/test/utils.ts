import { Service, ServiceStatus, Api } from '../src';
import { StartService } from '../src/service';
import { Logger, LogFunc } from '../src/logging';

/*******************************************************************************
 * Utils
 ******************************************************************************/

/** Construct a promise to a service command. */
export function testService(
  command: string,
  args: string[],
  supportsCleanShutdown = true
): Promise<StartService> {
  return new Promise(resolve =>
    resolve({ command, args, supportsCleanShutdown })
  );
}

/**
 * Expect the given process ID to not exist.
 */
export const expectProcessToBeGone = (
  pid: number,
  signal: number = 0
): void => {
  expect(() => process.kill(pid, signal)).toThrow();
};

/**
 * @return mutable array which will contain events as they occur.
 */
export const collectEvents = (service: Service): ServiceStatus[] => {
  let events: ServiceStatus[] = [];
  service.events.on('statusChanged', status => events.push(status));
  return events;
};

export interface MockLog {
  severity: 'debug' | 'info' | 'error';
  msg: string;
  param: object | undefined;
}

export interface MockLogger extends Logger {
  getLogs(): MockLog[];
}

export function mockLogger(echo: boolean = false): MockLogger {
  let logs: MockLog[] = [];

  const mockLog = (severity: 'debug' | 'info' | 'error'): LogFunc => {
    return (msg: string, param?: object) => {
      if (echo) {
        if (param) {
          console[severity](msg, param);
        } else {
          console[severity](msg);
        }
      }
      logs.push({ severity, msg, param: param || undefined });
    };
  };

  return {
    debug: mockLog('debug'),
    info: mockLog('info'),
    error: mockLog('error'),
    getLogs: () => logs,
  };
}

export function delay(ms: number) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

/**
 * Sets up the parameters for `http.request` for this Api.
 *
 * @param path - the api route (without leading slash)
 * @param options - extra options to be added to the request.
 * @return an options object suitable for `http.request`
 */
export function makeRequest(api: Api, path: string, options?: object): object {
  return Object.assign(
    {},
    api.requestParams,
    {
      path: api.requestParams.path + path,
    },
    options
  );
}
