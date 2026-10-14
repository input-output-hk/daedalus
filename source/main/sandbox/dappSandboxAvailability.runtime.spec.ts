/** @jest-environment node */

import { EventEmitter } from 'events';
import type { ProcessMetric } from 'electron';
import type * as Fs from 'fs';
import type * as DappSandboxAvailability from './dappSandboxAvailability';

const darwinRoot = '/Applications/Daedalus Preview.app';
const darwinExecutable = `${darwinRoot}/Contents/MacOS/Frontend`;
const darwinResources = `${darwinRoot}/Contents/Resources`;
const validMetric = {
  creationTime: 100.25,
  pid: 20,
  sandboxed: true,
  type: 'Tab',
} as ProcessMetric;

const processDescriptors = {
  platform: Object.getOwnPropertyDescriptor(process, 'platform'),
  arch: Object.getOwnPropertyDescriptor(process, 'arch'),
  execPath: Object.getOwnPropertyDescriptor(process, 'execPath'),
  resourcesPath: Object.getOwnPropertyDescriptor(process, 'resourcesPath'),
};
const configFile = process.env.DAEDALUS_CONFIG_FILE;

type RuntimeOptions = {
  metrics?: readonly (readonly ProcessMetric[])[];
  bypass?: boolean;
  cleanupFailure?: boolean;
  packaged?: boolean;
  symlinkPackage?: boolean;
};

const setProcessValue = (name: string, value: string): void => {
  Object.defineProperty(process, name, { configurable: true, value });
};

const loadRuntime = (options: RuntimeOptions = {}) => {
  jest.resetModules();
  setProcessValue('platform', 'darwin');
  setProcessValue('arch', 'arm64');
  setProcessValue('execPath', darwinExecutable);
  setProcessValue('resourcesPath', darwinResources);
  process.env.DAEDALUS_CONFIG_FILE = `${darwinResources}/daedalus-config.json`;

  const canarySession = {
    isPersistent: jest.fn(() => false),
    getStoragePath: jest.fn(() => null),
    clearStorageData: jest.fn(() => Promise.resolve()),
    clearCache: jest.fn(() =>
      options.cleanupFailure
        ? Promise.reject(new Error('cleanup failed'))
        : Promise.resolve()
    ),
    clearAuthCache: jest.fn(() => Promise.resolve()),
    clearHostResolverCache: jest.fn(() => Promise.resolve()),
    closeAllConnections: jest.fn(() => Promise.resolve()),
  };
  const webContents = Object.assign(new EventEmitter(), {
    setWindowOpenHandler: jest.fn(),
    getOSProcessId: jest.fn(() => 20),
    isDestroyed: jest.fn(() => false),
  });
  const window = Object.assign(new EventEmitter(), {
    webContents,
    loadURL: jest.fn(() => Promise.resolve()),
    isDestroyed: jest.fn(() => false),
    destroy: jest.fn(),
  });
  const BrowserWindow = jest.fn(() => window);
  const snapshots = options.metrics || [[validMetric], [validMetric]];
  let metricCall = 0;
  const app = {
    commandLine: { hasSwitch: jest.fn(() => options.bypass === true) },
    getAppMetrics: jest.fn(
      () => snapshots[Math.min(metricCall++, snapshots.length - 1)]
    ),
    getAppPath: jest.fn(() => `${darwinResources}/app`),
    getName: jest.fn(() => 'Daedalus Preview'),
    isPackaged: options.packaged === true,
  };
  jest.doMock('electron', () => ({
    app,
    BrowserWindow,
    session: { fromPartition: jest.fn(() => canarySession) },
  }));

  const actualFs = jest.requireActual<typeof Fs>('fs');
  jest.doMock('fs', () => ({
    ...actualFs,
    lstatSync: jest.fn((entryPath: string) => ({
      isDirectory: () => !entryPath.endsWith('Frontend'),
      isFile: () => entryPath.endsWith('Frontend'),
      isSymbolicLink: () => options.symlinkPackage === true,
    })),
    realpathSync: jest.fn((entryPath: string) => entryPath),
  }));

  let runtime!: typeof DappSandboxAvailability;
  jest.isolateModules(() => {
    runtime = require('./dappSandboxAvailability');
  });
  return { app, BrowserWindow, runtime };
};

const restoreProcessValue = (name: keyof typeof processDescriptors): void => {
  const descriptor = processDescriptors[name];
  if (descriptor) Object.defineProperty(process, name, descriptor);
  else delete (process as unknown as Record<string, unknown>)[name];
};

afterEach(() => {
  restoreProcessValue('platform');
  restoreProcessValue('arch');
  restoreProcessValue('execPath');
  restoreProcessValue('resourcesPath');
  if (configFile === undefined) delete process.env.DAEDALUS_CONFIG_FILE;
  else process.env.DAEDALUS_CONFIG_FILE = configFile;
  jest.resetModules();
  jest.restoreAllMocks();
  jest.dontMock('electron');
  jest.dontMock('fs');
});

describe('Darwin dApp sandbox runtime availability', () => {
  test('stable native evidence reaches available', async () => {
    const { runtime } = loadRuntime();
    await expect(
      runtime.startDappSandboxAvailabilityCheck({
        cluster: 'preview',
        isDevelopment: true,
      })
    ).resolves.toEqual({ status: 'available' });
    await expect(
      runtime.requireDappSandboxAvailable()
    ).resolves.toBeUndefined();
  });

  test.each([
    [
      'changed creation time',
      [[validMetric], [{ ...validMetric, creationTime: 101 }]],
    ],
    ['missing final metric', [[validMetric], []]],
  ])('%s fails closed and remains cached', async (_name, metrics) => {
    const { app, BrowserWindow, runtime } = loadRuntime({
      metrics: metrics as ProcessMetric[][],
    });
    const options = { cluster: 'preview', isDevelopment: true };
    const first = await runtime.startDappSandboxAvailabilityCheck(options);
    expect(first).toEqual({ status: 'unavailable', reason: 'canary-failed' });
    app.getAppMetrics.mockReturnValue([validMetric]);
    await expect(
      runtime.startDappSandboxAvailabilityCheck(options)
    ).resolves.toBe(first);
    expect(BrowserWindow).toHaveBeenCalledTimes(1);
  });

  test.each([
    {
      name: 'package path',
      installRoot: `${darwinRoot}/Contents`,
      symlinkPackage: false,
    },
    {
      name: 'package symlink',
      installRoot: darwinRoot,
      symlinkPackage: true,
    },
  ])('$name evidence refuses before canary creation', async (fixture) => {
    const { BrowserWindow, runtime } = loadRuntime({
      packaged: true,
      symlinkPackage: fixture.symlinkPackage,
    });
    await expect(
      runtime.startDappSandboxAvailabilityCheck({
        cluster: 'preview',
        installRoot: fixture.installRoot,
        isDevelopment: false,
      })
    ).resolves.toEqual({
      status: 'unavailable',
      reason: 'unsupported-package',
    });
    expect(BrowserWindow).not.toHaveBeenCalled();
  });

  test('sandbox bypass refuses without creating a canary', async () => {
    const { BrowserWindow, runtime } = loadRuntime({ bypass: true });
    await expect(
      runtime.startDappSandboxAvailabilityCheck({
        cluster: 'preview',
        isDevelopment: true,
      })
    ).resolves.toEqual({ status: 'unavailable', reason: 'sandbox-bypass' });
    expect(BrowserWindow).not.toHaveBeenCalled();
  });

  test('cleanup rejection overrides successful evidence', async () => {
    const { runtime } = loadRuntime({ cleanupFailure: true });
    await expect(
      runtime.startDappSandboxAvailabilityCheck({
        cluster: 'preview',
        isDevelopment: true,
      })
    ).resolves.toEqual({ status: 'unavailable', reason: 'cleanup-failed' });
  });
});

type LinuxSample = {
  status: Record<string, string>;
  startTime: string;
  namespaces: Record<'pid' | 'user' | 'mnt' | 'net', string>;
  uidMap: string;
  gidMap: string;
  device: bigint;
  inode: bigint;
  filesystemType: bigint;
  directory: boolean;
  entries: string[];
  rootLink: string;
  cwdDevice: bigint;
  cwdInode: bigint;
  filesystemMissing?: boolean;
};

type LinuxOperation =
  | 'root-open'
  | 'cwd-open'
  | 'root-stat'
  | 'cwd-stat'
  | 'statfs'
  | 'enumerate'
  | 'root-close'
  | 'cwd-close'
  | 'root-link';

type LinuxRuntimeOptions = {
  startup?: boolean;
  configure?: (sample: LinuxSample, main: boolean, final: boolean) => void;
  syscallError?: {
    operation: LinuxOperation;
    code: string;
    main?: boolean;
    final?: boolean;
  };
  procError?: string;
  procErrorMain?: boolean;
  rootLinkChanged?: boolean;
  bracketChanged?: boolean;
  initialPid?: number;
  pidThrows?: boolean;
  finalPid?: number;
  finalDestroyed?: boolean;
  missingStatfs?: boolean;
};

const loadLinuxRuntime = (options: LinuxRuntimeOptions = {}) => {
  const loaded = loadRuntime();
  setProcessValue('platform', 'linux');
  setProcessValue('arch', 'x64');
  const actualFs = jest.requireActual<typeof Fs>('fs');
  const linuxConstants = {
    ...actualFs.constants,
    O_DIRECTORY: 0x10000,
  };
  const statusReads = new Map<number, number>();
  const rootLinkReads = new Map<number, number>();
  const samples = new Map<number, LinuxSample>();
  const descriptors = new Map<number, { pid: number; cwd: boolean }>();
  const opened: number[] = [];
  const closed: number[] = [];
  let nextDescriptor = 100;
  const finalSample = (pid: number) => {
    const sampleIndex = Math.floor(((statusReads.get(pid) || 1) - 1) / 2);
    return sampleIndex >= (pid === process.pid && options.startup ? 2 : 1);
  };
  const sampleFor = (pid: number): LinuxSample => {
    const main = pid === process.pid;
    const sample: LinuxSample = {
      status: {
        Pid: String(pid),
        NoNewPrivs: main ? '0' : '1',
        Seccomp: main ? '0' : '2',
        Seccomp_filters: main ? '0' : '1',
        CapEff: '0000000000000000',
        CapPrm: '0000000000000000',
        CapInh: '0000000000000000',
      },
      startTime: main ? '100' : '200',
      namespaces: {
        pid: main ? 'pid:[1]' : 'pid:[2]',
        user: main ? 'user:[1]' : 'user:[2]',
        mnt: 'mnt:[1]',
        net: main ? 'net:[1]' : 'net:[2]',
      },
      uidMap: main ? '0 0 4294967295\n' : '0 1000 1\n',
      gidMap: main ? '0 0 4294967295\n' : '0 1000 1\n',
      device: main ? BigInt(1) : BigInt(2),
      inode: main ? BigInt(10) : BigInt(20),
      filesystemType: main ? BigInt(61267) : BigInt(40864),
      directory: true,
      entries: [],
      rootLink: main ? '/' : '/proc/30/task/30/fdinfo (deleted)',
      cwdDevice: main ? BigInt(1) : BigInt(2),
      cwdInode: main ? BigInt(10) : BigInt(20),
    };
    options.configure?.(sample, main, finalSample(pid));
    samples.set(pid, sample);
    return sample;
  };
  const currentSample = (pid: number) => samples.get(pid) || sampleFor(pid);
  const fail = (operation: LinuxOperation, pid: number) => {
    const fault = options.syscallError;
    if (
      fault?.operation === operation &&
      (pid === process.pid) === (fault.main === true) &&
      (fault.final === undefined || fault.final === finalSample(pid))
    )
      throw Object.assign(new Error('mock syscall failure'), {
        code: fault.code,
      });
  };
  const descriptorFor = (fd: number) => {
    const descriptor = descriptors.get(fd);
    if (!descriptor) throw new Error('unknown descriptor');
    return descriptor;
  };
  const fsMock = {
    ...actualFs,
    constants: linuxConstants,
    readFileSync: jest.fn((entryPath: string) => {
      const match = /^\/proc\/(\d+)\/(.+)$/.exec(entryPath);
      if (!match) throw new Error(`unexpected proc file: ${entryPath}`);
      const pid = Number(match[1]);
      const file = match[2];
      if (
        file === options.procError &&
        (pid === process.pid) === (options.procErrorMain === true)
      )
        throw new Error('mandatory read failure');
      if (file === 'status') {
        const reads = (statusReads.get(pid) || 0) + 1;
        statusReads.set(pid, reads);
        const sample = reads % 2 === 1 ? sampleFor(pid) : currentSample(pid);
        const status = { ...sample.status };
        if (options.bracketChanged && reads % 2 === 0 && pid !== process.pid)
          status.Seccomp = '0';
        return Object.entries(status)
          .map(([key, value]) => `${key}:\t${value}`)
          .join('\n');
      }
      const sample = currentSample(pid);
      if (file === 'stat')
        return `${pid} (mock renderer) ${Array(19).fill('0').join(' ')} ${sample.startTime} 0`;
      if (file === 'cmdline')
        return Buffer.from(
          pid === process.pid ? 'electron\0' : 'electron\0--type=renderer\0'
        );
      if (file === 'uid_map') return sample.uidMap;
      if (file === 'gid_map') return sample.gidMap;
      throw new Error(`unexpected proc file: ${file}`);
    }),
    readlinkSync: jest.fn((entryPath: string) => {
      const match = /^\/proc\/(\d+)\/(.+)$/.exec(entryPath);
      if (!match) throw new Error('unexpected proc link');
      const pid = Number(match[1]);
      const sample = currentSample(pid);
      if (
        match[2] === options.procError &&
        (pid === process.pid) === (options.procErrorMain === true)
      )
        throw new Error('mandatory link failure');
      if (match[2] === 'root') {
        fail('root-link', pid);
        const reads = (rootLinkReads.get(pid) || 0) + 1;
        rootLinkReads.set(pid, reads);
        if (options.rootLinkChanged && pid !== process.pid && reads % 2 === 0)
          return '/proc/31/fdinfo (deleted)';
        return sample.rootLink;
      }
      if (match[2].startsWith('ns/'))
        return sample.namespaces[
          match[2].slice(3) as keyof LinuxSample['namespaces']
        ];
      throw new Error('unexpected proc link');
    }),
    openSync: jest.fn((entryPath: string, flags: number) => {
      const match = /^\/proc\/(\d+)\/(root|cwd)$/.exec(entryPath);
      if (!match) throw new Error('unexpected directory open');
      expect(flags & 0x200000).toBe(0x200000);
      expect(flags & linuxConstants.O_DIRECTORY).toBe(
        linuxConstants.O_DIRECTORY
      );
      const pid = Number(match[1]);
      const cwd = match[2] === 'cwd';
      fail(cwd ? 'cwd-open' : 'root-open', pid);
      if (currentSample(pid).filesystemMissing)
        throw Object.assign(new Error('missing filesystem proof'), {
          code: 'ENOENT',
        });
      const fd = nextDescriptor++;
      descriptors.set(fd, { pid, cwd });
      opened.push(fd);
      return fd;
    }),
    fstatSync: jest.fn((fd: number, statOptions: { bigint: boolean }) => {
      expect(statOptions).toEqual({ bigint: true });
      const { pid, cwd } = descriptorFor(fd);
      fail(cwd ? 'cwd-stat' : 'root-stat', pid);
      const sample = currentSample(pid);
      return {
        dev: cwd ? sample.cwdDevice : sample.device,
        ino: cwd ? sample.cwdInode : sample.inode,
        isDirectory: () => sample.directory,
      };
    }),
    statfsSync: options.missingStatfs
      ? undefined
      : jest.fn((entryPath: string, statOptions: { bigint: boolean }) => {
          expect(statOptions).toEqual({ bigint: true });
          const { pid } = descriptorFor(Number(entryPath.split('/').pop()));
          fail('statfs', pid);
          return { type: currentSample(pid).filesystemType };
        }),
    readdirSync: jest.fn((entryPath: string) => {
      const { pid } = descriptorFor(Number(entryPath.split('/').pop()));
      fail('enumerate', pid);
      return currentSample(pid).entries;
    }),
    closeSync: jest.fn((fd: number) => {
      const { pid, cwd } = descriptorFor(fd);
      closed.push(fd);
      descriptors.delete(fd);
      fail(cwd ? 'cwd-close' : 'root-close', pid);
    }),
  };
  jest.doMock('fs', () => fsMock);
  const webContents = Object.assign(new EventEmitter(), {
    setWindowOpenHandler: jest.fn(),
    getOSProcessId: jest.fn(() => {
      if (options.pidThrows) throw new Error('renderer PID unavailable');
      return options.finalPid !== undefined && finalSample(20)
        ? options.finalPid
        : (options.initialPid ?? 20);
    }),
    isDestroyed: jest.fn(
      () => options.finalDestroyed === true && finalSample(20)
    ),
  });
  const window = Object.assign(new EventEmitter(), {
    webContents,
    loadURL: jest.fn(() => Promise.resolve()),
    isDestroyed: jest.fn(() => false),
    destroy: jest.fn(),
  });
  const canarySession = {
    isPersistent: () => false,
    getStoragePath: () => null,
    clearStorageData: () => Promise.resolve(),
    clearCache: () => Promise.resolve(),
    clearAuthCache: () => Promise.resolve(),
    clearHostResolverCache: () => Promise.resolve(),
    closeAllConnections: () => Promise.resolve(),
  };
  jest.doMock('electron', () => ({
    app: loaded.app,
    BrowserWindow: jest.fn(() => window),
    session: { fromPartition: () => canarySession },
  }));
  // Reload after replacing the Darwin mocks without leaking runtime state.
  jest.resetModules();
  let runtime!: typeof DappSandboxAvailability;
  jest.isolateModules(() => {
    runtime = require('./dappSandboxAvailability');
  });
  return {
    runtime,
    webContents,
    fsMock,
    assertClosed: () => {
      expect(closed.slice().sort()).toEqual(opened.slice().sort());
      expect(descriptors.size).toBe(0);
    },
  };
};

const exerciseLinuxRuntime = async (
  options: LinuxRuntimeOptions,
  startup: boolean
) => {
  const fixture = loadLinuxRuntime({ ...options, startup });
  const result = startup
    ? (
        await fixture.runtime.startDappSandboxAvailabilityCheck({
          cluster: 'preview',
          isDevelopment: true,
        })
      ).status === 'available'
    : fixture.runtime.validateDappRendererSandbox(fixture.webContents as never);
  expect(typeof result).toBe('boolean');
  fixture.assertClosed();
  return result;
};

describe.each([true, false])(
  'Linux syscall containment (startup=%s)',
  (startup) => {
    test('shared mount with exact empty proc-fdinfo proof is accepted', async () => {
      expect(await exerciseLinuxRuntime({}, startup)).toBe(true);
    });
    test('exact enumeration ESRCH accepts dead-proc proof', async () => {
      expect(
        await exerciseLinuxRuntime(
          {
            syscallError: { operation: 'enumerate', code: 'ESRCH' },
          },
          startup
        )
      ).toBe(true);
    });
    test.each(['device', 'inode'] as const)(
      'only %s differs from main root',
      async (component) => {
        expect(
          await exerciseLinuxRuntime(
            {
              configure: (sample, main) => {
                if (main) return;
                sample.device = BigInt(component === 'device' ? 2 : 1);
                sample.cwdDevice = sample.device;
                sample.inode = BigInt(component === 'inode' ? 20 : 10);
                sample.cwdInode = sample.inode;
              },
            },
            startup
          )
        ).toBe(true);
      }
    );
    test('canonical whitespace-equivalent final maps preserve identity', async () => {
      expect(
        await exerciseLinuxRuntime(
          {
            configure: (sample, _main, final) => {
              if (final) {
                sample.uidMap = ` \t${sample.uidMap.trim().replace(/ /g, '\t ')} \n`;
                sample.gidMap = `\n${sample.gidMap.trim().replace(/ /g, '  ')}\t\n`;
              }
            },
          },
          startup
        )
      ).toBe(true);
    });
    test.each(['main', 'renderer', 'both'])(
      '%s missing root proof rejects shared mounts',
      async (missing) => {
        expect(
          await exerciseLinuxRuntime(
            {
              configure: (sample, main) => {
                if (missing === 'both' || main === (missing === 'main'))
                  sample.filesystemMissing = true;
              },
            },
            startup
          )
        ).toBe(false);
      }
    );

    const invalidProofs: Array<
      [string, (sample: LinuxSample, main: boolean) => void]
    > = [
      [
        'same root',
        (sample, main) => {
          if (!main) {
            sample.device = BigInt(1);
            sample.cwdDevice = sample.device;
            sample.inode = BigInt(10);
            sample.cwdInode = sample.inode;
          }
        },
      ],
      [
        'nonempty root',
        (sample, main) => {
          if (!main) sample.entries = ['entry'];
        },
      ],
      [
        'nonprocfs root',
        (sample, main) => {
          if (!main) sample.filesystemType = BigInt(61267);
        },
      ],
      [
        'arbitrary empty root',
        (sample, main) => {
          if (!main) sample.rootLink = '/empty';
        },
      ],
      [
        'cwd outside root',
        (sample, main) => {
          if (!main) sample.cwdInode = BigInt(21);
        },
      ],
      [
        'invalid main root identity',
        (sample, main) => {
          if (main) sample.inode = BigInt(-1);
        },
      ],
      [
        'invalid renderer root identity',
        (sample, main) => {
          if (!main) sample.device = BigInt(-1);
        },
      ],
      [
        'shared PID namespace',
        (sample, main) => {
          if (!main) sample.namespaces.pid = 'pid:[1]';
        },
      ],
      [
        'shared user namespace',
        (sample, main) => {
          if (!main) sample.namespaces.user = 'user:[1]';
        },
      ],
      [
        'shared network namespace',
        (sample, main) => {
          if (!main) sample.namespaces.net = 'net:[1]';
        },
      ],
      [
        'effective capabilities',
        (sample, main) => {
          if (!main) sample.status.CapEff = '1';
        },
      ],
      [
        'permitted capabilities',
        (sample, main) => {
          if (!main) sample.status.CapPrm = '1';
        },
      ],
      [
        'inheritable capabilities',
        (sample, main) => {
          if (!main) sample.status.CapInh = '1';
        },
      ],
      [
        'malformed UID map',
        (sample, main) => {
          if (!main) sample.uidMap = 'invalid';
        },
      ],
      [
        'empty GID map',
        (sample, main) => {
          if (!main) sample.gidMap = '';
        },
      ],
      [
        'zero map count',
        (sample, main) => {
          if (!main) sample.uidMap = '0 1000 0';
        },
      ],
      [
        'overflowing map range',
        (sample, main) => {
          if (!main) sample.gidMap = '0 4294967296 1';
        },
      ],
      [
        'multiple renderer ranges',
        (sample, main) => {
          if (!main) sample.uidMap += '1 1001 1\n';
        },
      ],
      [
        'identical structural maps',
        (sample, main) => {
          if (main) sample.uidMap = ' 0\t1000  1\n';
        },
      ],
    ];
    test.each(invalidProofs)('%s fails closed', async (_name, configure) => {
      expect(await exerciseLinuxRuntime({ configure }, startup)).toBe(false);
    });
    test.each(['EACCES', 'ENOENT', 'EIO'])(
      'enumeration %s is not dead-proc proof',
      async (code) => {
        expect(
          await exerciseLinuxRuntime(
            {
              syscallError: { operation: 'enumerate', code },
            },
            startup
          )
        ).toBe(false);
      }
    );
    test.each(['nonprocfs', 'nonfdinfo', 'nondirectory'])(
      'ESRCH with %s metadata rejects',
      async (kind) => {
        expect(
          await exerciseLinuxRuntime(
            {
              syscallError: { operation: 'enumerate', code: 'ESRCH' },
              configure: (sample, main) => {
                if (main) return;
                if (kind === 'nonprocfs') sample.filesystemType = BigInt(61267);
                if (kind === 'nonfdinfo') sample.rootLink = '/empty';
                if (kind === 'nondirectory') sample.directory = false;
              },
            },
            startup
          )
        ).toBe(false);
      }
    );
    const failingOperations: LinuxOperation[] = [
      'root-open',
      'cwd-open',
      'root-stat',
      'cwd-stat',
      'statfs',
      'enumerate',
      'root-close',
      'cwd-close',
      'root-link',
    ];
    describe.each([true, false])('filesystem failure main=%s', (main) => {
      const faults: Array<{ operation: LinuxOperation; code: string }> = [];
      failingOperations.forEach((operation) => {
        ['EACCES', 'ENOENT', 'ESRCH'].forEach((code) => {
          if (main || operation !== 'enumerate' || code !== 'ESRCH')
            faults.push({ operation, code });
        });
      });
      test.each(faults)(
        '$operation $code closes every acquired descriptor',
        async ({ operation, code }) => {
          expect(
            await exerciseLinuxRuntime(
              {
                syscallError: { operation, code, main },
              },
              startup
            )
          ).toBe(false);
        }
      );
    });
    test('absent runtime statfs support rejects shared mounts', async () => {
      expect(await exerciseLinuxRuntime({ missingStatfs: true }, startup)).toBe(
        false
      );
    });
    test('changing root symlink during collection rejects proof', async () => {
      expect(
        await exerciseLinuxRuntime({ rootLinkChanged: true }, startup)
      ).toBe(false);
    });
    test('changing bracketed status rejects mixed evidence', async () => {
      expect(
        await exerciseLinuxRuntime({ bracketChanged: true }, startup)
      ).toBe(false);
    });
    describe.each([true, false])(
      'mandatory proc failure main=%s',
      (procErrorMain) => {
        test.each([
          'status',
          'stat',
          'cmdline',
          'uid_map',
          'gid_map',
          'ns/pid',
          'ns/user',
          'ns/mnt',
          'ns/net',
        ])('mandatory %s failure fails closed', async (procError) => {
          expect(
            await exerciseLinuxRuntime({ procError, procErrorMain }, startup)
          ).toBe(false);
        });
      }
    );
    test('separate mounts tolerate optional root proof failure', async () => {
      expect(
        await exerciseLinuxRuntime(
          {
            syscallError: { operation: 'root-open', code: 'EACCES' },
            configure: (sample, main) => {
              if (!main) sample.namespaces.mnt = 'mnt:[2]';
            },
          },
          startup
        )
      ).toBe(true);
    });
    test.each(['main', 'renderer', 'both'])(
      'separate mounts tolerate disappearing final %s filesystem proof',
      async (missing) => {
        expect(
          await exerciseLinuxRuntime(
            {
              configure: (sample, main, final) => {
                if (!main) sample.namespaces.mnt = 'mnt:[2]';
                if (
                  final &&
                  (missing === 'both' || main === (missing === 'main'))
                )
                  sample.filesystemMissing = true;
              },
            },
            startup
          )
        ).toBe(true);
      }
    );

    const finalChanges: Array<
      [string, (sample: LinuxSample, main: boolean) => void]
    > = [
      [
        'renderer seccomp',
        (sample, main) => {
          if (!main) sample.status.Seccomp = '0';
        },
      ],
      [
        'renderer no-new-privs',
        (sample, main) => {
          if (!main) sample.status.NoNewPrivs = '0';
        },
      ],
      [
        'renderer filters',
        (sample, main) => {
          if (!main) sample.status.Seccomp_filters = '0';
        },
      ],
      [
        'renderer effective capabilities',
        (sample, main) => {
          if (!main) sample.status.CapEff = '1';
        },
      ],
      [
        'renderer permitted capabilities',
        (sample, main) => {
          if (!main) sample.status.CapPrm = '1';
        },
      ],
      [
        'renderer inheritable capabilities',
        (sample, main) => {
          if (!main) sample.status.CapInh = '1';
        },
      ],
      [
        'renderer UID map',
        (sample, main) => {
          if (!main) sample.uidMap = '0 1001 1';
        },
      ],
      [
        'renderer GID map',
        (sample, main) => {
          if (!main) sample.gidMap = '0 1001 1';
        },
      ],
      [
        'main UID baseline',
        (sample, main) => {
          if (main) sample.uidMap = '0 0 1';
        },
      ],
      [
        'main GID baseline',
        (sample, main) => {
          if (main) sample.gidMap = '0 0 1';
        },
      ],
      [
        'renderer root',
        (sample, main) => {
          if (!main) {
            sample.inode = BigInt(21);
            sample.cwdInode = sample.inode;
          }
        },
      ],
      [
        'main root baseline',
        (sample, main) => {
          if (main) {
            sample.inode = BigInt(11);
            sample.cwdInode = sample.inode;
          }
        },
      ],
      [
        'renderer cwd',
        (sample, main) => {
          if (!main) sample.cwdInode = BigInt(21);
        },
      ],
      [
        'renderer emptiness',
        (sample, main) => {
          if (!main) sample.entries = ['entry'];
        },
      ],
      [
        'renderer procfs',
        (sample, main) => {
          if (!main) sample.filesystemType = BigInt(61267);
        },
      ],
      [
        'renderer missing proof',
        (sample, main) => {
          if (!main) sample.filesystemMissing = true;
        },
      ],
      [
        'main missing proof',
        (sample, main) => {
          if (main) sample.filesystemMissing = true;
        },
      ],
      ...(['pid', 'user', 'mnt', 'net'] as const).map(
        (namespace): [string, (sample: LinuxSample, main: boolean) => void] => [
          `renderer ${namespace} namespace`,
          (sample, main) => {
            if (!main) sample.namespaces[namespace] = `${namespace}:[9]`;
          },
        ]
      ),
      [
        'main namespace baseline',
        (sample, main) => {
          if (main) sample.namespaces.net = 'net:[9]';
        },
      ],
      [
        'PID reuse',
        (sample, main) => {
          if (!main) sample.startTime = '201';
        },
      ],
      [
        'main instance replacement',
        (sample, main) => {
          if (main) sample.startTime = '101';
        },
      ],
    ];
    test.each(finalChanges)(
      'final %s rejects valid initial proof',
      async (_name, mutate) => {
        expect(
          await exerciseLinuxRuntime(
            {
              configure: (sample, main, final) => {
                if (final) mutate(sample, main);
              },
            },
            startup
          )
        ).toBe(false);
      }
    );
    test.each(['root-open', 'enumerate', 'cwd-close'] as LinuxOperation[])(
      'disappearing final %s proof rejects',
      async (operation) => {
        expect(
          await exerciseLinuxRuntime(
            {
              syscallError: { operation, code: 'ENOENT', final: true },
            },
            startup
          )
        ).toBe(false);
      }
    );
    test('changed final WebContents PID rejects', async () => {
      expect(await exerciseLinuxRuntime({ finalPid: 21 }, startup)).toBe(false);
    });
    test('destroyed final WebContents rejects', async () => {
      expect(
        await exerciseLinuxRuntime({ finalDestroyed: true }, startup)
      ).toBe(false);
    });
  }
);

describe('Linux live renderer PID failures', () => {
  test('getOSProcessId throwing returns false synchronously', () => {
    const fixture = loadLinuxRuntime({ pidThrows: true });
    expect(
      fixture.runtime.validateDappRendererSandbox(fixture.webContents as never)
    ).toBe(false);
    fixture.assertClosed();
  });
  test.each([0, 1, -1, 20.5, NaN, Infinity, Number.MAX_SAFE_INTEGER + 1])(
    'invalid PID %s returns false synchronously',
    (initialPid) => {
      const fixture = loadLinuxRuntime({ initialPid });
      expect(
        fixture.runtime.validateDappRendererSandbox(
          fixture.webContents as never
        )
      ).toBe(false);
      expect(fixture.fsMock.readFileSync).not.toHaveBeenCalled();
      fixture.assertClosed();
    }
  );
});
