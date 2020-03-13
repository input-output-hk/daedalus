import { setupService, ServiceStatus, ServiceExitStatus } from '../src/service';
import {
  testService,
  collectEvents,
  expectProcessToBeGone,
  mockLogger,
} from './utils';

// increase time available for some tests to run
const longTestTimeoutMs = 15000;

// Note: These tests use simple coreutils commands as mock services.
// For example, `cat` will read its standard input and exit on EOF.
// On Windows, they are installed as part of Git For Windows.

describe('setupService', () => {
  it('starting simple command', async () => {
    let service = setupService(testService('echo', ['test echo']));
    let events: ServiceStatus[] = [];
    service.events.on('statusChanged', status => events.push(status));
    service.start();

    return new Promise(done => {
      service.events.on('statusChanged', status => {
        if (status === ServiceStatus.Stopped) {
          expect(events).toEqual([
            ServiceStatus.Starting,
            ServiceStatus.Started,
            ServiceStatus.Stopped,
          ]);
        }
        done();
      });
    });
  });

  it('stopping a command', async () => {
    let service = setupService(testService('cat', []));
    let events: ServiceStatus[] = [];
    service.events.on('statusChanged', status => events.push(status));
    let pid = await service.start();
    let result = await service.stop(2);
    // process should not exist
    expect(() => process.kill(pid, 0)).toThrow();
    // end of file for cat
    expect(result).toEqual({ exe: 'cat', code: 0, signal: null, err: null });
  });

  it('stopping a command (timeout)', async () => {
    let service = setupService(testService('sleep', ['4']));
    let pid = await service.start();
    let result = await service.stop(2);
    // process should not exist
    expect(() => process.kill(pid, 0)).toThrow();
    // exited with signal
    expect(result).toEqual({
      exe: 'sleep',
      code: null,
      signal: 'SIGKILL',
      err: null,
    });
  });

  it(
    'command was killed',
    () => {
      const service = setupService(testService('sleep', ['10'], false));
      const events: ServiceStatus[] = [];
      service.events.on('statusChanged', status => events.push(status));
      const pidP = service.start();
      return new Promise(done => {
        setTimeout(
          () =>
            pidP.then(pid => {
              console.log('Killing the process ' + pid);
              process.kill(pid);
            }),
          1000
        );
        service.events.on('statusChanged', status => {
          if (status === ServiceStatus.Stopped) {
            expect(events).toEqual([
              ServiceStatus.Starting,
              ServiceStatus.Started,
              ServiceStatus.Stopped,
            ]);
            service.stop().then((status: ServiceExitStatus) => {
              if (process.platform === 'win32') {
                expect(status.code).toBe(1);
              } else {
                expect(status.code).toBeNull();
                expect(status.signal).toBe('SIGTERM');
              }
              expect(status.exe).toBe('sleep');
              done();
            });
          }
        });
      });
    },
    longTestTimeoutMs
  );

  it('start is idempotent', async () => {
    let service = setupService(testService('cat', []));
    let events = collectEvents(service);
    let pid1 = await service.start();
    let pid2 = await service.start();
    await service.stop(2);
    // should have only started once
    expect(pid1).toBe(pid2);
    // process should not exist
    expectProcessToBeGone(pid1);
    // events fire only once
    expect(events).toEqual([
      ServiceStatus.Starting,
      ServiceStatus.Started,
      ServiceStatus.Stopping,
      ServiceStatus.Stopped,
    ]);
  });

  it('stop is idempotent', async () => {
    let service = setupService(testService('cat', []));
    let events = collectEvents(service);
    let pid = await service.start();
    let result1 = await service.stop(2);
    let result2 = await service.stop(2);
    // same result
    expect(result1).toEqual(result2);
    // process should not exist
    expectProcessToBeGone(pid);
    // cat command exits normally
    expect(result1).toEqual({ exe: 'cat', code: 0, signal: null, err: null });
    // events fire only once
    expect(events).toEqual([
      ServiceStatus.Starting,
      ServiceStatus.Started,
      ServiceStatus.Stopping,
      ServiceStatus.Stopped,
    ]);
  });

  it('stopping an already stopped command', done => {
    let service = setupService(testService('echo', ['hello from tests']));
    let events = collectEvents(service);
    let pidP = service.start();
    setTimeout(() => {
      // should have exited after 1 second
      pidP.then(pid => {
        expectProcessToBeGone(pid);
        // stop what's already stopped
        service.stop(2).then(result => {
          // check collected status
          expect(result).toEqual({
            exe: 'echo',
            code: 0,
            signal: null,
            err: null,
          });
          // sequence of events doesn't include Stopping
          expect(events).toEqual([
            ServiceStatus.Starting,
            ServiceStatus.Started,
            ServiceStatus.Stopped,
          ]);
          done();
        });
      });
    }, 1000);
  });

  it('starting a bogus command', async () => {
    let logger = mockLogger(true);
    let service = setupService(testService('xyzzy', []), logger);
    let events = collectEvents(service);
    await service.start();
    let result = await service.waitForExit();
    expect(result.err ? result.err.toString() : null).toBe(
      'Error: spawn xyzzy ENOENT'
    );
    expect(result.code).toBeNull();
    expect(result.signal).toBeNull();
    expect(events).toEqual([
      ServiceStatus.Starting,
      ServiceStatus.Started,
      ServiceStatus.Stopped,
    ]);
    expect(logger.getLogs().filter(l => l.severity === 'error').length).toBe(1);
  });
});
