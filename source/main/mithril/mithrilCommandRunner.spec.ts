import { EventEmitter } from 'events';
import {
  openLogStream,
  attachLogStream,
  runCommand,
  runBinary,
  normalizeSpawnEnv,
} from './mithrilCommandRunner';

jest.mock('../utils/logging', () => ({
  logger: {
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

jest.mock('../config', () => ({
  stateDirectoryPath: '/tmp/daedalus-state',
}));

jest.mock('../utils/ensureDirectoryExists', () => jest.fn());

jest.mock('fs-extra', () => ({
  createWriteStream: jest.fn(() => ({
    write: jest.fn(),
    end: jest.fn(),
  })),
}));

jest.mock('./mithrilNetworkConfig', () => ({
  buildMithrilEnv: jest
    .fn()
    .mockResolvedValue({ AGGREGATOR_ENDPOINT: 'http://test' }),
}));

jest.mock('../environment', () => ({
  environment: {
    isWindows: false,
  },
}));

jest.mock('child_process', () => ({
  spawn: jest.fn(),
}));

const createChildProcess = () => {
  const childEmitter = new EventEmitter() as any;
  childEmitter.stdout = new EventEmitter();
  childEmitter.stderr = new EventEmitter();
  childEmitter.kill = jest.fn();
  return childEmitter;
};

describe('openLogStream', () => {
  it('creates a write stream at the expected log path', () => {
    const fs = require('fs-extra');
    openLogStream();
    expect(fs.createWriteStream).toHaveBeenCalledWith(
      '/tmp/daedalus-state/Logs/mithril-bootstrap.log',
      {
        flags: 'a',
      }
    );
  });

  it('creates a write stream for a custom log file name', () => {
    const fs = require('fs-extra');
    openLogStream('mithril-partial-sync.log');
    expect(fs.createWriteStream).toHaveBeenCalledWith(
      '/tmp/daedalus-state/Logs/mithril-partial-sync.log',
      {
        flags: 'a',
      }
    );
  });
});

describe('attachLogStream', () => {
  it('pipes child stdout and stderr to the log stream', () => {
    const logStream = { write: jest.fn(), end: jest.fn() } as any;

    const stdoutEmitter = new EventEmitter();
    const stderrEmitter = new EventEmitter();
    const child = { stdout: stdoutEmitter, stderr: stderrEmitter } as any;

    attachLogStream(child, logStream);

    stdoutEmitter.emit('data', Buffer.from('hello'));
    stderrEmitter.emit('data', Buffer.from('world'));

    expect(logStream.write).toHaveBeenCalledTimes(2);
  });
});

describe('runCommand', () => {
  const originalInstallDir = process.env.DAEDALUS_INSTALL_DIRECTORY;

  afterEach(() => {
    const { environment } = require('../environment');
    const { spawn } = require('child_process');

    environment.isWindows = false;
    spawn.mockReset();

    if (typeof originalInstallDir === 'undefined') {
      delete process.env.DAEDALUS_INSTALL_DIRECTORY;
    } else {
      process.env.DAEDALUS_INSTALL_DIRECTORY = originalInstallDir;
    }
  });

  it('resolves with stdout, stderr, and exitCode on success', async () => {
    const { spawn } = require('child_process');

    const childEmitter = createChildProcess();

    // Emit data and close via setTimeout so all listeners are attached first
    spawn.mockImplementation(() => {
      setTimeout(() => {
        childEmitter.stdout.emit('data', Buffer.from('output-data'));
        childEmitter.emit('close', 0);
      }, 0);
      return childEmitter;
    });

    const result = await runCommand(
      ['cardano-db', 'snapshot', 'list'],
      '/tmp/workdir',
      {}
    );

    expect(spawn).toHaveBeenCalledWith(
      'mithril-client',
      ['--origin-tag', 'DAEDALUS', 'cardano-db', 'snapshot', 'list'],
      expect.objectContaining({
        cwd: '/tmp/workdir',
      })
    );
    expect(result.stdout).toBe('output-data');
    expect(result.exitCode).toBe(0);
  });

  it('uses the installed binary path when DAEDALUS_INSTALL_DIRECTORY is set', async () => {
    const { spawn } = require('child_process');

    process.env.DAEDALUS_INSTALL_DIRECTORY = '/opt/daedalus';

    const childEmitter = createChildProcess();
    spawn.mockImplementation(() => {
      setTimeout(() => {
        childEmitter.emit('close', 0);
      }, 0);
      return childEmitter;
    });

    await runCommand(['snapshot', 'list'], '/tmp/workdir', {});

    expect(spawn).toHaveBeenCalledWith(
      '/opt/daedalus/mithril-client',
      ['--origin-tag', 'DAEDALUS', 'snapshot', 'list'],
      expect.objectContaining({
        cwd: '/tmp/workdir',
      })
    );
  });

  it('uses the Windows binary name when running on Windows', async () => {
    const { spawn } = require('child_process');
    const { environment } = require('../environment');

    environment.isWindows = true;

    const childEmitter = createChildProcess();
    spawn.mockImplementation(() => {
      setTimeout(() => {
        childEmitter.emit('close', 0);
      }, 0);
      return childEmitter;
    });

    await runCommand(['snapshot', 'list'], 'C:\\workdir', {});

    expect(spawn).toHaveBeenCalledWith(
      'mithril-client.exe',
      ['--origin-tag', 'DAEDALUS', 'snapshot', 'list'],
      expect.objectContaining({
        cwd: 'C:\\workdir',
      })
    );
  });

  it('uses the installed Windows binary path when DAEDALUS_INSTALL_DIRECTORY is set', async () => {
    const { spawn } = require('child_process');
    const { environment } = require('../environment');

    environment.isWindows = true;
    process.env.DAEDALUS_INSTALL_DIRECTORY = 'C:\\Program Files\\Daedalus';

    const childEmitter = createChildProcess();
    spawn.mockImplementation(() => {
      setTimeout(() => {
        childEmitter.emit('close', 0);
      }, 0);
      return childEmitter;
    });

    await runCommand(['snapshot', 'list'], 'C:\\workdir', {});

    expect(spawn).toHaveBeenCalledWith(
      'C:\\Program Files\\Daedalus/mithril-client.exe',
      ['--origin-tag', 'DAEDALUS', 'snapshot', 'list'],
      expect.objectContaining({
        cwd: 'C:\\workdir',
      })
    );
  });

  it('prepends the Daedalus origin tag before command args', async () => {
    const { spawn } = require('child_process');

    const childEmitter = createChildProcess();
    spawn.mockImplementation(() => {
      setTimeout(() => {
        childEmitter.emit('close', 0);
      }, 0);
      return childEmitter;
    });

    await runCommand(['cardano-db', 'snapshot', 'list'], '/tmp/workdir');

    expect(spawn).toHaveBeenCalledWith(
      'mithril-client',
      ['--origin-tag', 'DAEDALUS', 'cardano-db', 'snapshot', 'list'],
      expect.objectContaining({
        cwd: '/tmp/workdir',
      })
    );
  });

  it('rejects on child process error', async () => {
    const { spawn } = require('child_process');

    const childEmitter = createChildProcess();

    spawn.mockImplementation(() => {
      setTimeout(() => {
        childEmitter.emit('error', new Error('spawn ENOENT'));
      }, 0);
      return childEmitter;
    });

    await expect(
      runCommand(['bad-command'], '/tmp/workdir', {})
    ).rejects.toThrow('spawn ENOENT');
  });

  it('calls onProcess callback with child on start and null on close', async () => {
    const { spawn } = require('child_process');

    const childEmitter = createChildProcess();

    spawn.mockImplementation(() => {
      setTimeout(() => {
        childEmitter.emit('close', 0);
      }, 0);
      return childEmitter;
    });

    const onProcess = jest.fn();
    await runCommand(['list'], '/tmp/workdir', {}, { onProcess });

    expect(onProcess).toHaveBeenCalledWith(childEmitter);
    expect(onProcess).toHaveBeenCalledWith(null);
  });

  it('logs the spawned pid, registers an exit listener that logs code/signal, and resolves un-truncated stdout on close', async () => {
    const { spawn } = require('child_process');
    const { logger } = require('../utils/logging');

    const childEmitter = createChildProcess();
    childEmitter.pid = 111;
    childEmitter.killed = false;

    // A payload larger than any UI-facing truncation to prove `close` resolves the full stdout.
    const bigStdout = 'x'.repeat(100_000);

    spawn.mockImplementation(() => {
      setTimeout(() => {
        childEmitter.stdout.emit('data', Buffer.from(bigStdout));
        childEmitter.emit('exit', 0, null);
        childEmitter.emit('close', 0);
      }, 0);
      return childEmitter;
    });

    const result = await runCommand(['snapshot', 'list'], '/tmp/workdir', {});

    // An 'exit' listener is registered purely for logging (there was none previously).
    expect(childEmitter.listenerCount('exit')).toBeGreaterThan(0);

    expect(logger.info).toHaveBeenCalledWith('[mithril] child spawned', {
      pid: 111,
    });
    expect(logger.info).toHaveBeenCalledWith(
      '[mithril] child exited',
      expect.objectContaining({ pid: 111, code: 0, signal: null })
    );
    expect(logger.info).toHaveBeenCalledWith('[mithril] child closed', {
      pid: 111,
      exitCode: 0,
    });

    // `close` still resolves the un-truncated stdout/stderr/exitCode contract.
    expect(result.stdout).toBe(bigStdout);
    expect(result.stderr).toBe('');
    expect(result.exitCode).toBe(0);
  });

  it('spawns detached on POSIX so the child leads its own process group', async () => {
    const { spawn } = require('child_process');

    const childEmitter = createChildProcess();
    spawn.mockImplementation(() => {
      setTimeout(() => {
        childEmitter.emit('close', 0);
      }, 0);
      return childEmitter;
    });

    await runCommand(['snapshot', 'list'], '/tmp/workdir', {});

    expect(spawn).toHaveBeenCalledWith(
      'mithril-client',
      expect.any(Array),
      expect.objectContaining({ detached: true })
    );
  });

  it('does NOT spawn detached on Windows', async () => {
    const { spawn } = require('child_process');
    const { environment } = require('../environment');

    environment.isWindows = true;

    const childEmitter = createChildProcess();
    spawn.mockImplementation(() => {
      setTimeout(() => {
        childEmitter.emit('close', 0);
      }, 0);
      return childEmitter;
    });

    await runCommand(['snapshot', 'list'], 'C:\\workdir', {});

    expect(spawn).toHaveBeenCalledWith(
      'mithril-client.exe',
      expect.any(Array),
      expect.objectContaining({ detached: false })
    );
  });
});

describe('runBinary', () => {
  const originalInstallDir = process.env.DAEDALUS_INSTALL_DIRECTORY;

  afterEach(() => {
    const { environment } = require('../environment');
    const { spawn } = require('child_process');

    environment.isWindows = false;
    spawn.mockReset();

    if (typeof originalInstallDir === 'undefined') {
      delete process.env.DAEDALUS_INSTALL_DIRECTORY;
    } else {
      process.env.DAEDALUS_INSTALL_DIRECTORY = originalInstallDir;
    }
  });

  it('spawns the named binary with given args and no origin-tag prefix', async () => {
    const { spawn } = require('child_process');

    const childEmitter = createChildProcess();
    spawn.mockImplementation(() => {
      setTimeout(() => {
        childEmitter.emit('close', 0);
      }, 0);
      return childEmitter;
    });

    await runBinary(
      'snapshot-converter',
      [
        '--input-mem',
        '/db/ledger/12345',
        '--output-lsm-snapshot',
        '/db/tmp/snapshots/12345_lsm',
      ],
      '/tmp/workdir'
    );

    expect(spawn).toHaveBeenCalledWith(
      'snapshot-converter',
      [
        '--input-mem',
        '/db/ledger/12345',
        '--output-lsm-snapshot',
        '/db/tmp/snapshots/12345_lsm',
      ],
      expect.objectContaining({ cwd: '/tmp/workdir' })
    );
  });

  it('uses the installed binary path when DAEDALUS_INSTALL_DIRECTORY is set', async () => {
    const { spawn } = require('child_process');

    process.env.DAEDALUS_INSTALL_DIRECTORY = '/opt/daedalus';

    const childEmitter = createChildProcess();
    spawn.mockImplementation(() => {
      setTimeout(() => {
        childEmitter.emit('close', 0);
      }, 0);
      return childEmitter;
    });

    await runBinary(
      'snapshot-converter',
      ['--input-mem', '/db'],
      '/tmp/workdir'
    );

    expect(spawn).toHaveBeenCalledWith(
      '/opt/daedalus/snapshot-converter',
      expect.any(Array),
      expect.any(Object)
    );
  });

  it('appends .exe on Windows', async () => {
    const { spawn } = require('child_process');
    const { environment } = require('../environment');

    environment.isWindows = true;

    const childEmitter = createChildProcess();
    spawn.mockImplementation(() => {
      setTimeout(() => {
        childEmitter.emit('close', 0);
      }, 0);
      return childEmitter;
    });

    await runBinary('snapshot-converter', [], 'C:\\workdir');

    expect(spawn).toHaveBeenCalledWith(
      'snapshot-converter.exe',
      expect.any(Array),
      expect.objectContaining({ cwd: 'C:\\workdir' })
    );
  });

  it('resolves with stdout, stderr, and exitCode', async () => {
    const { spawn } = require('child_process');

    const childEmitter = createChildProcess();
    spawn.mockImplementation(() => {
      setTimeout(() => {
        childEmitter.stdout.emit('data', Buffer.from('converted'));
        childEmitter.emit('close', 0);
      }, 0);
      return childEmitter;
    });

    const result = await runBinary('snapshot-converter', [], '/tmp/workdir');

    expect(result.stdout).toBe('converted');
    expect(result.exitCode).toBe(0);
  });

  it('logs the spawned pid, registers an exit listener that logs code/signal, and resolves un-truncated stdout on close', async () => {
    const { spawn } = require('child_process');
    const { logger } = require('../utils/logging');

    const childEmitter = createChildProcess();
    childEmitter.pid = 222;
    childEmitter.killed = false;

    const bigStdout = 'y'.repeat(100_000);

    spawn.mockImplementation(() => {
      setTimeout(() => {
        childEmitter.stdout.emit('data', Buffer.from(bigStdout));
        childEmitter.emit('exit', 0, null);
        childEmitter.emit('close', 0);
      }, 0);
      return childEmitter;
    });

    const result = await runBinary('snapshot-converter', [], '/tmp/workdir');

    expect(childEmitter.listenerCount('exit')).toBeGreaterThan(0);

    expect(logger.info).toHaveBeenCalledWith('[mithril] child spawned', {
      pid: 222,
    });
    expect(logger.info).toHaveBeenCalledWith(
      '[mithril] child exited',
      expect.objectContaining({ pid: 222, code: 0, signal: null })
    );
    expect(logger.info).toHaveBeenCalledWith('[mithril] child closed', {
      pid: 222,
      exitCode: 0,
    });

    expect(result.stdout).toBe(bigStdout);
    expect(result.stderr).toBe('');
    expect(result.exitCode).toBe(0);
  });

  it('spawns detached on POSIX so the child leads its own process group', async () => {
    const { spawn } = require('child_process');

    const childEmitter = createChildProcess();
    spawn.mockImplementation(() => {
      setTimeout(() => {
        childEmitter.emit('close', 0);
      }, 0);
      return childEmitter;
    });

    await runBinary('snapshot-converter', [], '/tmp/workdir');

    expect(spawn).toHaveBeenCalledWith(
      'snapshot-converter',
      expect.any(Array),
      expect.objectContaining({ detached: true })
    );
  });

  it('does NOT spawn detached on Windows', async () => {
    const { spawn } = require('child_process');
    const { environment } = require('../environment');

    environment.isWindows = true;

    const childEmitter = createChildProcess();
    spawn.mockImplementation(() => {
      setTimeout(() => {
        childEmitter.emit('close', 0);
      }, 0);
      return childEmitter;
    });

    await runBinary('snapshot-converter', [], 'C:\\workdir');

    expect(spawn).toHaveBeenCalledWith(
      'snapshot-converter.exe',
      expect.any(Array),
      expect.objectContaining({ detached: false })
    );
  });
});

describe('normalizeSpawnEnv', () => {
  const originalInstallDir = process.env.DAEDALUS_INSTALL_DIRECTORY;

  afterEach(() => {
    const { environment } = require('../environment');
    environment.isWindows = false;

    if (typeof originalInstallDir === 'undefined') {
      delete process.env.DAEDALUS_INSTALL_DIRECTORY;
    } else {
      process.env.DAEDALUS_INSTALL_DIRECTORY = originalInstallDir;
    }
  });

  it('normalizes duplicate Windows path keys and prepends install dir', () => {
    const { environment } = require('../environment');
    environment.isWindows = true;
    process.env.DAEDALUS_INSTALL_DIRECTORY = 'C:\\Program Files\\Daedalus';

    const env = normalizeSpawnEnv({
      Path: 'C:\\Windows\\System32',
      PATH: 'C:\\Windows\\System32;C:\\Temp',
      SystemRoot: 'C:\\Windows',
    } as unknown as NodeJS.ProcessEnv);

    expect(env.Path).toBe(
      'C:\\Program Files\\Daedalus;C:\\Windows\\System32;C:\\Temp'
    );
    expect(env.PATH).toBeUndefined();
    expect(env.SystemRoot).toBe('C:\\Windows');
  });
});
