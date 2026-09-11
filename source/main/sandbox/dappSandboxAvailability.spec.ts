import type { ProcessMetric } from 'electron';
import {
  hasSandboxBypass,
  requireDappSandboxAvailable,
  validateDarwinPackageIdentity,
  validateMetricRendererEvidence,
  validatePackageIdentity,
  validateRendererEvidence,
  validateWindowsPackageIdentity,
} from './dappSandboxAvailability';
const mainEvidence = {
  pid: 10,
  argv: ['/electron', '.'],
  status: { Pid: '10' },
  startTime: '100',
  namespaces: {
    pid: 'pid:[10]',
    user: 'user:[10]',
    mnt: 'mnt:[10]',
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
  },
  startTime: '200',
  namespaces: {
    pid: 'pid:[20]',
    user: 'user:[20]',
    mnt: 'mnt:[20]',
  },
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
  launcherConfigPath:
    'C:\\Program Files\\Daedalus Mainnet\\launcher-config.yaml',
  resourcesPath: 'C:\\Program Files\\Daedalus Mainnet\\resources',
};

const darwinPackage = {
  appName: 'Daedalus Preview',
  appPath: '/Applications/Daedalus Preview.app/Contents/Resources/app',
  architecture: 'arm64',
  executablePath: '/Applications/Daedalus Preview.app/Contents/MacOS/Frontend',
  installRoot: '/Applications/Daedalus Preview.app',
  isPackaged: true,
  launcherConfigPath:
    '/Applications/Daedalus Preview.app/Contents/Resources/launcher-config.yaml',
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
          kernelRelease: '7.1.8-arch1-Watanare-T2-3-t2',
        },
      },
      host: {
        id: 'omarchy',
        versionId: '4.0.2',
        buildId: '4.0.2',
        kernelRelease: '7.1.8-arch1-Watanare-T2-3-t2',
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
    ['mainnet_flight', 'Daedalus Flight'],
    ['preprod', 'Daedalus Pre-Prod'],
    ['preview', 'Daedalus Preview'],
    ['selfnode', 'Daedalus Selfnode'],
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
          launcherConfigPath: `${installRoot}\\launcher-config.yaml`,
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
          launcherConfigPath: `${installRoot}/Contents/Resources/launcher-config.yaml`,
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
          launcherConfigPath: `${installRoot}/Contents/Resources/launcher-config.yaml`,
          resourcesPath: `${installRoot}/Contents/Resources`,
        },
        'mainnet_flight'
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
      'external launcher config',
      { ...darwinPackage, launcherConfigPath: '/tmp/launcher-config.yaml' },
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
