import { killProcessTree } from './killProcessTree';

jest.mock('../utils/logging', () => ({
  logger: {
    info: jest.fn(),
    warn: jest.fn(),
  },
}));

// Mutable environment mock reassigned per test (established runner-spec pattern —
// NOT jest.isolateModules).
jest.mock('../environment', () => ({
  environment: {
    isWindows: false,
  },
}));

jest.mock('child_process', () => ({
  exec: jest.fn(),
  execSync: jest.fn(),
}));

const createFakeChild = (pid: number | undefined) =>
  ({
    pid,
    kill: jest.fn(),
  } as any);

describe('killProcessTree', () => {
  let processKillSpy: jest.SpyInstance;

  beforeEach(() => {
    jest.clearAllMocks();
    processKillSpy = jest.spyOn(process, 'kill').mockImplementation(() => true);
  });

  afterEach(() => {
    const { environment } = require('../environment');
    environment.isWindows = false;
    processKillSpy.mockRestore();
  });

  it('POSIX: kills the process GROUP via process.kill(-pid, signal)', () => {
    const child = createFakeChild(4242);

    killProcessTree(child, 'SIGTERM');

    expect(processKillSpy).toHaveBeenCalledWith(-4242, 'SIGTERM');
    expect(child.kill).not.toHaveBeenCalled();
  });

  it('POSIX: defaults the signal to SIGTERM and forwards SIGKILL when given', () => {
    const child = createFakeChild(77);

    killProcessTree(child);
    expect(processKillSpy).toHaveBeenCalledWith(-77, 'SIGTERM');

    killProcessTree(child, 'SIGKILL');
    expect(processKillSpy).toHaveBeenCalledWith(-77, 'SIGKILL');
  });

  it('Windows async default (interactive kills): non-blocking exec taskkill /pid <pid> /t /f', () => {
    const { environment } = require('../environment');
    const { exec, execSync } = require('child_process');
    environment.isWindows = true;

    const child = createFakeChild(1337);
    killProcessTree(child, 'SIGTERM');

    expect(exec).toHaveBeenCalledWith(
      'taskkill /pid 1337 /t /f',
      expect.objectContaining({ windowsHide: true, timeout: 5000 }),
      expect.any(Function)
    );
    expect(execSync).not.toHaveBeenCalled();
    expect(processKillSpy).not.toHaveBeenCalled();
    expect(child.kill).not.toHaveBeenCalled();
  });

  it('Windows sync: true (shutdown reap ONLY): execSync taskkill with ignored stdio and a bounded timeout', () => {
    const { environment } = require('../environment');
    const { exec, execSync } = require('child_process');
    environment.isWindows = true;

    const child = createFakeChild(2026);
    killProcessTree(child, 'SIGKILL', { sync: true });

    expect(execSync).toHaveBeenCalledWith(
      'taskkill /pid 2026 /t /f',
      expect.objectContaining({
        stdio: 'ignore',
        timeout: 5000,
        windowsHide: true,
      })
    );
    expect(exec).not.toHaveBeenCalled();
    expect(processKillSpy).not.toHaveBeenCalled();
  });

  it('no-ops on a null child and on a child with no pid', () => {
    const { exec, execSync } = require('child_process');

    killProcessTree(null, 'SIGTERM');
    const pidlessChild = createFakeChild(undefined);
    killProcessTree(pidlessChild, 'SIGTERM');

    expect(processKillSpy).not.toHaveBeenCalled();
    expect(exec).not.toHaveBeenCalled();
    expect(execSync).not.toHaveBeenCalled();
    expect(pidlessChild.kill).not.toHaveBeenCalled();
  });

  it('falls back to a direct child.kill(signal) when the POSIX group kill throws', () => {
    const { logger } = require('../utils/logging');
    processKillSpy.mockImplementation(() => {
      throw new Error('ESRCH');
    });

    const child = createFakeChild(555);
    killProcessTree(child, 'SIGKILL');

    expect(child.kill).toHaveBeenCalledWith('SIGKILL');
    expect(logger.warn).toHaveBeenCalledWith(
      'killProcessTree: group kill failed; falling back to direct child.kill',
      expect.objectContaining({ pid: 555, signal: 'SIGKILL' })
    );
  });

  it('falls back to a direct child.kill(signal) when the Windows sync taskkill throws', () => {
    const { environment } = require('../environment');
    const { execSync } = require('child_process');
    environment.isWindows = true;
    execSync.mockImplementation(() => {
      throw new Error('taskkill exited 1');
    });

    const child = createFakeChild(31337);
    killProcessTree(child, 'SIGKILL', { sync: true });

    expect(child.kill).toHaveBeenCalledWith('SIGKILL');
  });

  it('Windows async: the exec callback falls back to child.kill(signal) on failure', () => {
    const { environment } = require('../environment');
    const { exec } = require('child_process');
    environment.isWindows = true;

    const child = createFakeChild(808);
    killProcessTree(child, 'SIGTERM');

    const callback = exec.mock.calls[0][2];
    callback(new Error('taskkill failed'));

    expect(child.kill).toHaveBeenCalledWith('SIGTERM');
  });

  it('swallows and warns when the direct child.kill fallback itself throws', () => {
    const { logger } = require('../utils/logging');
    processKillSpy.mockImplementation(() => {
      throw new Error('group kill failed');
    });

    const child = createFakeChild(666);
    child.kill.mockImplementation(() => {
      throw new Error('kill EPERM');
    });

    expect(() => killProcessTree(child, 'SIGTERM')).not.toThrow();
    expect(logger.warn).toHaveBeenCalledWith(
      'killProcessTree: direct child.kill fallback failed',
      expect.objectContaining({ pid: 666, signal: 'SIGTERM' })
    );
  });
});
