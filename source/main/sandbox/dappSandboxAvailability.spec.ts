import {
  hasSandboxBypass,
  requireDappSandboxAvailable,
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
