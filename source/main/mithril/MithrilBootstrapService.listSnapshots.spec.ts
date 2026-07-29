import type { ChildProcess } from 'child_process';
import { MithrilBootstrapService } from './MithrilBootstrapService';
import { runCommand } from './mithrilCommandRunner';
import { killProcessTree } from './killProcessTree';

jest.mock('./mithrilCommandRunner', () => ({
  runCommand: jest.fn(),
  runBinary: jest.fn(),
}));

jest.mock('./killProcessTree', () => ({
  killProcessTree: jest.fn(),
}));

jest.mock('./mithrilSnapshotConverter', () => ({
  convertSnapshotDbToLsm: jest.fn(),
}));

jest.mock('../config', () => ({
  stateDirectoryPath: '/tmp/state',
  launcherConfig: {
    nodeConfig: { network: { configFile: '/config.yaml' } },
  },
}));

jest.mock('../environment', () => ({
  environment: { network: 'mainnet', nodeVersion: '1.35.0', isWindows: false },
}));

jest.mock('../utils/logging', () => ({
  logger: { warn: jest.fn(), info: jest.fn(), error: jest.fn() },
}));

jest.mock('../utils/chainStorageManager', () => ({
  ChainStorageManager: jest.fn().mockImplementation(() => ({})),
}));

const FAKE_PROCESS = { pid: 1234 } as unknown as ChildProcess;

const mockRunCommand = runCommand as jest.MockedFunction<typeof runCommand>;
const mockKillProcessTree = killProcessTree as jest.MockedFunction<
  typeof killProcessTree
>;

function makeService(): MithrilBootstrapService {
  const service = new MithrilBootstrapService('/tmp/state/chain');
  return service;
}

describe('MithrilBootstrapService.listSnapshots', () => {
  beforeEach(() => jest.resetAllMocks());

  it('returns parsed snapshots when mithril-client succeeds', async () => {
    const snapshotJson = JSON.stringify([
      {
        digest: 'abc123',
        size: 100,
        created_at: '2024-01-01T00:00:00Z',
        compression_algorithm: 'zstandard',
        beacon: { network: 'mainnet', epoch: 1, immutable_file_number: 1 },
        certificate_hash: 'cert1',
        locations: [],
        cardano_node_version: '9.0.0',
      },
    ]);

    mockRunCommand.mockImplementation((_args, _workDir, _opts, callbacks) => {
      callbacks?.onProcess?.(FAKE_PROCESS);
      return Promise.resolve({ stdout: snapshotJson, stderr: '', exitCode: 0 });
    });

    const result = await makeService().listSnapshots();

    expect(result).toHaveLength(1);
    expect(result[0].digest).toBe('abc123');
    expect(mockKillProcessTree).not.toHaveBeenCalled();
  });

  it('returns [] when mithril-client exits non-zero (e.g. aggregator DNS failure)', async () => {
    mockRunCommand.mockImplementation((_args, _workDir, _opts, callbacks) => {
      callbacks?.onProcess?.(FAKE_PROCESS);
      return Promise.resolve({
        stdout: '',
        stderr: 'dns error: failed to lookup address information',
        exitCode: 1,
      });
    });

    const result = await makeService().listSnapshots();

    expect(result).toEqual([]);
    expect(mockKillProcessTree).not.toHaveBeenCalled();
  });

  it('kills the in-flight process when abortSnapshotList() is called', async () => {
    let resolveCommand!: (value: any) => void;

    mockRunCommand.mockImplementation((_args, _workDir, _opts, callbacks) => {
      callbacks?.onProcess?.(FAKE_PROCESS);
      return new Promise((resolve) => {
        resolveCommand = resolve;
      });
    });

    const service = makeService();
    const promise = service.listSnapshots();

    service.abortSnapshotList();

    resolveCommand({ stdout: '', stderr: '', exitCode: 1 });

    const result = await promise;

    expect(result).toEqual([]);
    expect(mockKillProcessTree).toHaveBeenCalledWith(FAKE_PROCESS);
  });
});
