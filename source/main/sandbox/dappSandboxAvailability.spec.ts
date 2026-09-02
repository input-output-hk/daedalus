import {
  hasSandboxBypass,
  requireDappSandboxAvailable,
  validatePackageIdentity,
  validateRendererEvidence,
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
