import crypto from 'crypto';
import fs from 'fs';
import path from 'path';
import type { ProcessMetric } from 'electron';
import {
  hasSandboxBypass,
  requireDappSandboxAvailable,
  validateDarwinPackageIdentity,
  validateMetricRendererEvidence,
  validatePackageIdentity,
  validateRendererEvidence,
  validateWindowsPackageIdentity,
  validateNixOSProductionPackage,
} from './dappSandboxAvailability';

jest.mock('electron', () => ({
  app: { getAppMetrics: jest.fn(() => []), getPath: jest.fn(() => '/tmp') },
  BrowserWindow: jest.fn(() => ({
    loadURL: jest.fn(),
    on: jest.fn(),
    webContents: { getOSProcessId: jest.fn(() => 0) },
    destroy: jest.fn(),
  })),
  session: {
    fromPartition: jest.fn(() => ({ setPermissionRequestHandler: jest.fn() })),
  },
}));

const mainEvidence = {
  pid: 10,
  argv: ['/electron', '.'],
  status: { Pid: '10' },
  startTime: '100',
  uidMap: '0 0 4294967295\n',
  gidMap: '0 0 4294967295\n',
  namespaces: {
    pid: 'pid:[10]',
    user: 'user:[10]',
    mnt: 'mnt:[10]',
    net: 'net:[10]',
  },
};

const rendererEvidence = {
  pid: 20,
  argv: ['/electron', '--type=renderer'],
  status: {
    Pid: '20',
    NoNewPrivs: '1',
    Seccomp: '2',
    Seccomp_filters: '1',
    CapEff: '0000000000000000',
    CapPrm: '0000000000000000',
    CapInh: '0000000000000000',
  },
  startTime: '200',
  uidMap: '0 1000 1\n',
  gidMap: '0 1000 1\n',
  namespaces: {
    pid: 'pid:[20]',
    user: 'user:[20]',
    mnt: 'mnt:[20]',
    net: 'net:[20]',
  },
};

const mainFilesystem = {
  device: '1',
  inode: '2',
  filesystemType: '61267',
  isDirectory: true,
  empty: false,
  deadProcRoot: false,
  chromiumFdinfoRoot: false,
  cwdMatchesRoot: false,
};

const restrictedFilesystem = {
  device: '3',
  inode: '4',
  filesystemType: '40864',
  isDirectory: true,
  empty: true,
  deadProcRoot: false,
  chromiumFdinfoRoot: true,
  cwdMatchesRoot: true,
};

const sharedMain = { ...mainEvidence, filesystem: mainFilesystem };
const sharedRenderer = {
  ...rendererEvidence,
  namespaces: {
    ...rendererEvidence.namespaces,
    mnt: mainEvidence.namespaces.mnt,
  },
  filesystem: restrictedFilesystem,
};

const archManifest = {
  schemaVersion: 2,
  packageFamily: 'arch',
  matrixRevision: 'task-111-matrix-2026-09-02',
  matrixRow: 'arch-2026.09.01',
  distribution: {
    id: 'arch',
    versionId: '2026.09.01',
    buildId: 'rolling',
    kernelRelease: '7.2.2-arch1-1',
  },
  supportState: 'supported',
  cluster: 'mainnet',
  policy: { kind: 'none' },
  helper: { mode: '0755' },
};

const windowsPackage = {
  appName: 'Daedalus Mainnet',
  appPath: 'C:\\Program Files\\Daedalus Mainnet\\resources\\app',
  architecture: 'x64',
  executablePath: 'C:\\Program Files\\Daedalus Mainnet\\Daedalus Mainnet.exe',
  installRoot: 'C:\\Program Files\\Daedalus Mainnet',
  isPackaged: true,
  configPath: 'C:\\Program Files\\Daedalus Mainnet\\daedalus-config.json',
  resourcesPath: 'C:\\Program Files\\Daedalus Mainnet\\resources',
};

const darwinPackage = {
  appName: 'Daedalus Preview',
  appPath: '/Applications/Daedalus Preview.app/Contents/Resources/app',
  architecture: 'arm64',
  executablePath: '/Applications/Daedalus Preview.app/Contents/MacOS/Frontend',
  installRoot: '/Applications/Daedalus Preview.app',
  isPackaged: true,
  configPath:
    '/Applications/Daedalus Preview.app/Contents/Resources/daedalus-config.json',
  resourcesPath: '/Applications/Daedalus Preview.app/Contents/Resources',
};

const windowsRendererMetric = {
  creationTime: 100,
  integrityLevel: 'low',
  pid: 20,
  sandboxed: true,
  type: 'Tab',
} as ProcessMetric;

describe('dApp sandbox availability', () => {
  test.each([
    '--disable-gpu-sandbox',
    '--disable-namespace-sandbox',
    '--disable-sandbox',
    '--disable-seccomp-filter-sandbox',
    '--disable-setuid-sandbox',
    '--in-process-gpu',
    '--no-sandbox',
    '--single-process',
  ])('rejects the %s Chromium switch', (forbiddenSwitch) => {
    expect(hasSandboxBypass([forbiddenSwitch], {})).toBe(true);
    expect(hasSandboxBypass([`${forbiddenSwitch}=true`], {})).toBe(true);
  });

  test('rejects ELECTRON_DISABLE_SANDBOX by presence', () => {
    expect(hasSandboxBypass([], { ELECTRON_DISABLE_SANDBOX: '' })).toBe(true);
    expect(hasSandboxBypass([], { ELECTRON_DISABLE_SANDBOX: '0' })).toBe(true);
    expect(hasSandboxBypass([], {})).toBe(false);
  });

  test('accepts the exact sandboxed renderer evidence', () => {
    expect(validateRendererEvidence(mainEvidence, rendererEvidence)).toBe(true);
    expect(
      validateRendererEvidence(mainEvidence, {
        ...rendererEvidence,
        argv: ['/electron', '--type=zygote'],
      })
    ).toBe(true);
  });

  test('requires a distinct user namespace for Arch packages', () => {
    expect(validateRendererEvidence(mainEvidence, rendererEvidence, true)).toBe(
      true
    );
    expect(
      validateRendererEvidence(
        mainEvidence,
        {
          ...rendererEvidence,
          namespaces: { ...rendererEvidence.namespaces, user: 'user:[10]' },
        },
        true
      )
    ).toBe(false);
  });

  test('accepts only proved Chromium roots on shared mounts', () => {
    expect(validateRendererEvidence(sharedMain, sharedRenderer)).toBe(true);
    expect(
      validateRendererEvidence(sharedMain, {
        ...sharedRenderer,
        filesystem: {
          ...restrictedFilesystem,
          empty: false,
          deadProcRoot: true,
        },
      })
    ).toBe(true);
    expect(
      validateRendererEvidence(mainEvidence, {
        ...rendererEvidence,
        namespaces: sharedRenderer.namespaces,
      })
    ).toBe(false);
  });

  test.each([
    [
      'missing main root',
      { ...sharedMain, filesystem: undefined },
      sharedRenderer,
    ],
    [
      'missing renderer root',
      sharedMain,
      { ...sharedRenderer, filesystem: undefined },
    ],
    [
      'both missing roots',
      mainEvidence,
      { ...sharedRenderer, filesystem: undefined },
    ],
    [
      'same root',
      sharedMain,
      {
        ...sharedRenderer,
        filesystem: { ...restrictedFilesystem, device: '1', inode: '2' },
      },
    ],
    [
      'invalid main root',
      {
        ...sharedMain,
        filesystem: { ...mainFilesystem, device: 'invalid' },
      },
      sharedRenderer,
    ],
    [
      'non-directory main root',
      {
        ...sharedMain,
        filesystem: { ...mainFilesystem, isDirectory: false },
      },
      sharedRenderer,
    ],
    [
      'invalid renderer root',
      sharedMain,
      {
        ...sharedRenderer,
        filesystem: { ...restrictedFilesystem, inode: '' },
      },
    ],
  ])('rejects shared mounts with %s', (_name, main, renderer) => {
    expect(
      validateRendererEvidence(
        main as typeof sharedMain,
        renderer as typeof sharedRenderer
      )
    ).toBe(false);
  });

  test.each([
    ['nonempty root', { empty: false }],
    ['nonprocfs root', { filesystemType: '61267' }],
    ['non-directory root', { isDirectory: false }],
    ['arbitrary empty root', { chromiumFdinfoRoot: false }],
    ['escaped cwd', { cwdMatchesRoot: false }],
    [
      'nonprocfs dead root',
      { filesystemType: '61267', empty: false, deadProcRoot: true },
    ],
    [
      'arbitrary dead root',
      { chromiumFdinfoRoot: false, empty: false, deadProcRoot: true },
    ],
  ])('rejects %s', (_name, patch) => {
    expect(
      validateRendererEvidence(sharedMain, {
        ...sharedRenderer,
        filesystem: { ...restrictedFilesystem, ...(patch as object) },
      })
    ).toBe(false);
  });

  test.each(['pid', 'user', 'net'] as Array<'pid' | 'user' | 'net'>)(
    'rejects shared %s namespace on the shared-mount route',
    (namespace) => {
      expect(
        validateRendererEvidence(sharedMain, {
          ...sharedRenderer,
          namespaces: {
            ...sharedRenderer.namespaces,
            [namespace]: mainEvidence.namespaces[namespace],
          },
        })
      ).toBe(false);
    }
  );

  test.each(['pid', 'user', 'net'] as Array<'pid' | 'user' | 'net'>)(
    'rejects missing %s namespace proof',
    (namespace) => {
      expect(
        validateRendererEvidence(
          {
            ...sharedMain,
            namespaces: { ...sharedMain.namespaces, [namespace]: '' },
          },
          sharedRenderer
        )
      ).toBe(false);
      expect(
        validateRendererEvidence(sharedMain, {
          ...sharedRenderer,
          namespaces: { ...sharedRenderer.namespaces, [namespace]: '' },
        })
      ).toBe(false);
    }
  );

  test.each(['CapEff', 'CapPrm', 'CapInh'])(
    'requires zero %s on the shared-mount route',
    (field) => {
      for (const value of ['0001', '', 'invalid']) {
        expect(
          validateRendererEvidence(sharedMain, {
            ...sharedRenderer,
            status: { ...sharedRenderer.status, [field]: value },
          })
        ).toBe(false);
      }
    }
  );

  test.each([
    '',
    '0 1000 0',
    '0 1000 2',
    '0 1000 1\n1 1001 1',
    '-1 1000 1',
    '0 +1000 1',
    '0 1.5 1',
    '0 1000',
    '4294967296 1000 1',
    '0 4294967296 1',
    '4294967295 1000 2',
    '0 4294967295 2',
    '0 1000 4294967296',
  ])('rejects invalid or non-singleton renderer mappings %s', (map) => {
    for (const field of ['uidMap', 'gidMap']) {
      expect(
        validateRendererEvidence(sharedMain, {
          ...sharedRenderer,
          [field]: map,
        })
      ).toBe(false);
    }
  });

  test('validates and structurally compares both main mappings', () => {
    for (const field of ['uidMap', 'gidMap']) {
      for (const map of ['', '0 0 0', '0 4294967295 2', '  0   1000  1  \n']) {
        expect(
          validateRendererEvidence(
            {
              ...sharedMain,
              [field]: map,
            },
            sharedRenderer
          )
        ).toBe(false);
      }
    }
    expect(
      validateRendererEvidence(
        {
          ...sharedMain,
          uidMap: '0 0 1000\n1000 2000 1000\n',
          gidMap: '0 0 1000\n1000 2000 1000\n',
        },
        {
          ...sharedRenderer,
          uidMap: '\t0  1000\t1\n',
          gidMap: '0 1000 1',
        }
      )
    ).toBe(true);
  });

  test('accepts root identities differing in either component', () => {
    for (const identity of [
      { device: '1', inode: '4' },
      { device: '3', inode: '2' },
    ]) {
      expect(
        validateRendererEvidence(sharedMain, {
          ...sharedRenderer,
          filesystem: { ...restrictedFilesystem, ...identity },
        })
      ).toBe(true);
    }
  });

  test('preserves the separated-mount route without optional root proof', () => {
    expect(
      validateRendererEvidence(mainEvidence, {
        ...rendererEvidence,
        status: { ...rendererEvidence.status, CapPrm: '1', CapInh: '1' },
        namespaces: {
          ...rendererEvidence.namespaces,
          user: mainEvidence.namespaces.user,
          net: mainEvidence.namespaces.net,
        },
      })
    ).toBe(true);
  });

  test.each([
    { argv: ['/electron', '--type=renderer', '--no-sandbox'] },
    { argv: ['/electron', '--type=gpu-process'] },
    { status: { ...rendererEvidence.status, Pid: '21' } },
    { status: { ...rendererEvidence.status, NoNewPrivs: '0' } },
    { status: { ...rendererEvidence.status, Seccomp: '0' } },
    { status: { ...rendererEvidence.status, Seccomp_filters: '0' } },
  ])('keeps common containment checks on shared mounts', (patch) => {
    expect(
      validateRendererEvidence(sharedMain, {
        ...sharedRenderer,
        ...patch,
      })
    ).toBe(false);
  });

  test.each([
    {
      name: 'the exact Arch snapshot',
      manifest: archManifest,
      host: archManifest.distribution,
      accepted: true,
    },
    {
      name: 'the exact Omarchy snapshot',
      manifest: {
        ...archManifest,
        matrixRow: 'omarchy-4.0.2',
        distribution: {
          id: 'omarchy',
          versionId: '4.0.2',
          buildId: '4.0.2',
          kernelRelease: '7.1.9-arch1-2',
        },
      },
      host: {
        id: 'omarchy',
        versionId: '4.0.2',
        buildId: '4.0.2',
        kernelRelease: '7.1.9-arch1-2',
      },
      accepted: true,
    },
    {
      name: 'a stale Arch kernel',
      manifest: archManifest,
      host: { ...archManifest.distribution, kernelRelease: '7.2.3-arch1-1' },
      accepted: false,
    },
    {
      name: 'an Arch manifest with setuid helper policy',
      manifest: { ...archManifest, helper: { mode: '4755' } },
      host: archManifest.distribution,
      accepted: false,
    },
    {
      name: 'an Arch manifest with a non-none policy',
      manifest: { ...archManifest, policy: { kind: 'apparmor' } },
      host: archManifest.distribution,
      accepted: false,
    },
    {
      name: 'a stale matrix revision',
      manifest: {
        ...archManifest,
        matrixRevision: 'task-108-matrix-2026-08-18',
      },
      host: archManifest.distribution,
      accepted: false,
    },
  ])('handles $name', ({ manifest, host, accepted }) => {
    expect(Boolean(validatePackageIdentity(manifest, host, 'mainnet'))).toBe(
      accepted
    );
  });

  test('accepts only the exact protected Windows package layout', () => {
    expect(validateWindowsPackageIdentity(windowsPackage, 'mainnet')).toBe(
      true
    );
    expect(
      validateWindowsPackageIdentity(
        {
          ...windowsPackage,
          executablePath: 'C:\\Users\\alice\\Daedalus Mainnet.exe',
        },
        'mainnet'
      )
    ).toBe(false);
    expect(
      validateWindowsPackageIdentity(
        { ...windowsPackage, installRoot: 'D:\\Daedalus Mainnet' },
        'mainnet'
      )
    ).toBe(false);
    expect(validateWindowsPackageIdentity(windowsPackage, 'preview')).toBe(
      false
    );
    expect(
      validateWindowsPackageIdentity(
        { ...windowsPackage, isPackaged: false },
        'mainnet'
      )
    ).toBe(false);
  });

  test.each([
    ['mainnet', 'Daedalus Mainnet'],
    ['mainnet-flight', 'Daedalus Flight'],
    ['preprod', 'Daedalus Pre-Prod'],
    ['preview', 'Daedalus Preview'],
  ])('accepts the shipped Windows %s package identity', (cluster, appName) => {
    const installRoot = `C:\\Program Files\\${appName}`;
    expect(
      validateWindowsPackageIdentity(
        {
          ...windowsPackage,
          appName,
          appPath: `${installRoot}\\resources\\app`,
          executablePath: `${installRoot}\\${appName}.exe`,
          installRoot,
          configPath: `${installRoot}\\daedalus-config.json`,
          resourcesPath: `${installRoot}\\resources`,
        },
        cluster
      )
    ).toBe(true);
  });

  test.each([
    ['x64', '/Applications'],
    ['arm64', '/Applications'],
    ['arm64', '/Users/alice/Desktop'],
  ])('accepts a Darwin %s bundle under %s', (architecture, parent) => {
    const installRoot = `${parent}/Daedalus Preview.app`;
    expect(
      validateDarwinPackageIdentity(
        {
          ...darwinPackage,
          architecture,
          appPath: `${installRoot}/Contents/Resources/app`,
          executablePath: `${installRoot}/Contents/MacOS/Frontend`,
          installRoot,
          configPath: `${installRoot}/Contents/Resources/daedalus-config.json`,
          resourcesPath: `${installRoot}/Contents/Resources`,
        },
        'preview'
      )
    ).toBe(true);
  });

  test('accepts the Flight Darwin package identity', () => {
    const installRoot = '/Applications/Daedalus Flight.app';
    expect(
      validateDarwinPackageIdentity(
        {
          ...darwinPackage,
          appName: 'Daedalus Flight',
          appPath: `${installRoot}/Contents/Resources/app`,
          executablePath: `${installRoot}/Contents/MacOS/Frontend`,
          installRoot,
          configPath: `${installRoot}/Contents/Resources/daedalus-config.json`,
          resourcesPath: `${installRoot}/Contents/Resources`,
        },
        'mainnet-flight'
      )
    ).toBe(true);
  });

  test.each([
    ['unknown cluster', darwinPackage, 'unknown'],
    ['mismatched product', darwinPackage, 'mainnet'],
    [
      'product-named launcher',
      {
        ...darwinPackage,
        executablePath:
          '/Applications/Daedalus Preview.app/Contents/MacOS/Daedalus Preview',
      },
      'preview',
    ],
    ['external app path', { ...darwinPackage, appPath: '/tmp/app' }, 'preview'],
    [
      'external resources path',
      { ...darwinPackage, resourcesPath: '/tmp/Resources' },
      'preview',
    ],
    [
      'external config',
      { ...darwinPackage, configPath: '/tmp/daedalus-config.json' },
      'preview',
    ],
    [
      'unpackaged execution',
      { ...darwinPackage, isPackaged: false },
      'preview',
    ],
    [
      'relative root',
      { ...darwinPackage, installRoot: 'Daedalus Preview.app' },
      'preview',
    ],
    [
      'non-normalized root',
      {
        ...darwinPackage,
        installRoot: '/Applications/../Applications/Daedalus Preview.app',
      },
      'preview',
    ],
    [
      'unsupported architecture',
      { ...darwinPackage, architecture: 'ia32' },
      'preview',
    ],
  ])('rejects a Darwin package with %s', (_name, identity, cluster) => {
    expect(validateDarwinPackageIdentity(identity, cluster)).toBe(false);
  });

  test.each([
    {
      name: 'an unsandboxed renderer',
      metric: { ...windowsRendererMetric, sandboxed: false },
    },
    {
      name: 'a medium-integrity renderer',
      metric: { ...windowsRendererMetric, integrityLevel: 'medium' },
    },
    {
      name: 'a browser process',
      metric: { ...windowsRendererMetric, type: 'Browser' },
    },
    {
      name: 'a different process',
      metric: { ...windowsRendererMetric, pid: 21 },
    },
  ])('rejects Windows evidence with $name', ({ metric }) => {
    expect(
      validateMetricRendererEvidence([metric] as ProcessMetric[], 20, 'win32')
    ).toBe(false);
  });

  test('accepts native Windows sandbox evidence for the exact renderer', () => {
    expect(
      validateMetricRendererEvidence([windowsRendererMetric], 20, 'win32')
    ).toBe(true);
    expect(
      validateMetricRendererEvidence(
        [{ ...windowsRendererMetric, integrityLevel: 'untrusted' }],
        20,
        'win32'
      )
    ).toBe(true);
  });

  test('accepts exact Darwin metric evidence without Windows integrity', () => {
    expect(
      validateMetricRendererEvidence(
        [
          {
            creationTime: 100.25,
            pid: 20,
            sandboxed: true,
            type: 'Tab',
          } as ProcessMetric,
        ],
        20,
        'darwin'
      )
    ).toBe(true);
  });

  test.each([
    ['missing sandbox status', { creationTime: 100.25, pid: 20, type: 'Tab' }],
    [
      'false sandbox status',
      { creationTime: 100.25, pid: 20, sandboxed: false, type: 'Tab' },
    ],
    [
      'wrong PID',
      { creationTime: 100.25, pid: 21, sandboxed: true, type: 'Tab' },
    ],
    [
      'wrong process type',
      { creationTime: 100.25, pid: 20, sandboxed: true, type: 'Browser' },
    ],
    ['missing timestamp', { pid: 20, sandboxed: true, type: 'Tab' }],
    [
      'string timestamp',
      { creationTime: '100.25', pid: 20, sandboxed: true, type: 'Tab' },
    ],
    [
      'zero timestamp',
      { creationTime: 0, pid: 20, sandboxed: true, type: 'Tab' },
    ],
    [
      'non-finite timestamp',
      {
        creationTime: Number.POSITIVE_INFINITY,
        pid: 20,
        sandboxed: true,
        type: 'Tab',
      },
    ],
  ])('rejects Darwin evidence with %s', (_name, metric) => {
    expect(
      validateMetricRendererEvidence([metric] as ProcessMetric[], 20, 'darwin')
    ).toBe(false);
  });

  test.each([1, 1.5, Number.NaN])(
    'rejects invalid renderer PID %p',
    (rendererPid) => {
      expect(
        validateMetricRendererEvidence(
          [
            {
              creationTime: 100.25,
              pid: rendererPid,
              sandboxed: true,
              type: 'Tab',
            } as ProcessMetric,
          ],
          rendererPid,
          'darwin'
        )
      ).toBe(false);
    }
  );

  test.each([
    {
      name: 'forbidden renderer switch',
      evidence: {
        ...rendererEvidence,
        argv: ['/electron', '--type=renderer', '--no-sandbox'],
      },
    },
    {
      name: 'missing no-new-privileges',
      evidence: {
        ...rendererEvidence,
        status: { ...rendererEvidence.status, NoNewPrivs: '0' },
      },
    },
    {
      name: 'missing seccomp',
      evidence: {
        ...rendererEvidence,
        status: { ...rendererEvidence.status, Seccomp: '0' },
      },
    },
    {
      name: 'zero seccomp filters',
      evidence: {
        ...rendererEvidence,
        status: { ...rendererEvidence.status, Seccomp_filters: '0' },
      },
    },
    {
      name: 'effective capability',
      evidence: {
        ...rendererEvidence,
        status: {
          ...rendererEvidence.status,
          CapEff: '0000000000000001',
        },
      },
    },
    {
      name: 'shared PID namespace',
      evidence: {
        ...rendererEvidence,
        namespaces: { ...rendererEvidence.namespaces, pid: 'pid:[10]' },
      },
    },
    {
      name: 'shared mount namespace',
      evidence: {
        ...rendererEvidence,
        namespaces: { ...rendererEvidence.namespaces, mnt: 'mnt:[10]' },
      },
    },
    {
      name: 'wrong observed PID',
      evidence: {
        ...rendererEvidence,
        status: { ...rendererEvidence.status, Pid: '21' },
      },
    },
  ])('rejects $name', ({ evidence }) => {
    expect(validateRendererEvidence(mainEvidence, evidence)).toBe(false);
  });

  test('requires the check to pass before guest construction', async () => {
    await expect(requireDappSandboxAvailable()).rejects.toMatchObject({
      name: 'DappSandboxUnavailableError',
      reason: 'not-checked',
    });
  });
});

describe('NixOS package identity', () => {
  const root = '/nix/store/aaaaaaaa-daedalus';
  const configPath = `${root}/config/daedalus-config.json`;
  const electronPath = `${root}/libexec/bundle-electron/lib/electron/electron`;
  const manifestPath = '/var/lib/daedalus/preprod/sandbox-identity.json';
  const helperPath = '/var/lib/daedalus/preprod/chrome-sandbox';
  const helper = Buffer.from('sandbox helper');
  const helperHash = crypto.createHash('sha256').update(helper).digest('hex');
  const execPath = Object.getOwnPropertyDescriptor(process, 'execPath');
  const hostPathSeparator = path.sep;
  const posixResolve = path.posix.resolve;
  const posixJoin = path.posix.join;

  const installFixture = (
    options: {
      host?: string;
      helperHash?: string;
      root?: string;
    } = {}
  ) => {
    const fixtureRoot = options.root || root;
    // The fixture models Linux, regardless of the Jest host's native paths.
    jest.spyOn(path, 'resolve').mockImplementation(posixResolve);
    jest.spyOn(path, 'join').mockImplementation(posixJoin);
    Object.defineProperty(path, 'sep', { value: path.posix.sep });
    process.env.ENTRYPOINT_DIR = fixtureRoot;
    process.env.DAEDALUS_CONFIG_FILE = `${fixtureRoot}/config/daedalus-config.json`;
    Object.defineProperty(process, 'execPath', {
      configurable: true,
      value: electronPath,
    });
    jest.spyOn(fs, 'realpathSync').mockImplementation((value) => String(value));
    jest.spyOn(fs, 'lstatSync').mockImplementation(((value: fs.PathLike) => {
      const file = String(value);
      let mode = 0o444;
      if (file === helperPath) mode = 0o755;
      else if (file === manifestPath) mode = 0o644;
      return {
        gid: 0,
        isFile: () => true,
        isSymbolicLink: () => false,
        mode,
        uid: 0,
      };
    }) as typeof fs.lstatSync);
    jest.spyOn(fs, 'readFileSync').mockImplementation(((
      value: fs.PathLike | number
    ) => {
      const file = String(value);
      if (file === '/etc/os-release')
        return options.host || 'ID=nixos\nVERSION_ID=26.05\n';
      if (file === manifestPath)
        return JSON.stringify({
          schemaVersion: 2,
          packageFamily: 'nix',
          matrixRevision: 'task-112-matrix-2026-09-11',
          matrixRow: 'nixos-26.05',
          distribution: { id: 'nixos', versionId: '26.05' },
          supportState: 'supported',
          cluster: 'preprod',
          policy: { kind: 'none' },
          helper: { mode: '0755', sha256: options.helperHash || helperHash },
          launch: { electron: electronPath },
        });
      if (file === helperPath) return helper;
      throw new Error(`unexpected read: ${file}`);
    }) as typeof fs.readFileSync);
  };

  afterEach(() => {
    delete process.env.ENTRYPOINT_DIR;
    delete process.env.DAEDALUS_CONFIG_FILE;
    if (execPath) Object.defineProperty(process, 'execPath', execPath);
    jest.restoreAllMocks();
    Object.defineProperty(path, 'sep', { value: hostPathSeparator });
  });

  test('accepts the exact store config, manifest, helper, and Electron identity', () => {
    installFixture();
    expect(validateNixOSProductionPackage('preprod')).toEqual({
      failure: null,
      usernsOnly: true,
    });
  });

  test.each([
    ['an unknown host', { host: 'ID=nixos\nVERSION_ID=unstable\n' }],
    ['a helper hash mismatch', { helperHash: '0'.repeat(64) }],
    ['an entrypoint outside the Nix store', { root: '/tmp/daedalus' }],
  ])('rejects %s', (_name, options) => {
    installFixture(options);
    expect(validateNixOSProductionPackage('preprod').failure).not.toBeNull();
  });

  test('rejects a substituted config path', () => {
    installFixture();
    process.env.DAEDALUS_CONFIG_FILE = '/tmp/daedalus-config.json';
    expect(validateNixOSProductionPackage('preprod').failure).toBe(
      'unsupported-package'
    );
  });
});
