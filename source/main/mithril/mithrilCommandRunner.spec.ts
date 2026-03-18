import { EventEmitter } from 'events';
import {
  openLogStream,
  attachLogStream,
  runCommand,
} from './mithrilCommandRunner';

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

jest.mock('child_process', () => ({
  spawn: jest.fn(),
}));

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
  it('resolves with stdout, stderr, and exitCode on success', async () => {
    const { spawn } = require('child_process');

    const childEmitter = new EventEmitter() as any;
    childEmitter.stdout = new EventEmitter();
    childEmitter.stderr = new EventEmitter();
    childEmitter.kill = jest.fn();

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
    expect(result.stdout).toBe('output-data');
    expect(result.exitCode).toBe(0);
  });

  it('rejects on child process error', async () => {
    const { spawn } = require('child_process');

    const childEmitter = new EventEmitter() as any;
    childEmitter.stdout = new EventEmitter();
    childEmitter.stderr = new EventEmitter();

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

    const childEmitter = new EventEmitter() as any;
    childEmitter.stdout = new EventEmitter();
    childEmitter.stderr = new EventEmitter();

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
});
