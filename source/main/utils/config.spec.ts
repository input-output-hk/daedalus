import { readFileSync } from 'fs';

import { readDappRuntimeConfig } from './config';

jest.mock('fs', () => ({ readFileSync: jest.fn() }));

const hash = 'a'.repeat(64);
const config = {
  cardanoWalletSourceRevision: 'd408e4f29e46dc34454b160b3994ee9974b5cde2',
  cip30Enabled: true,
  dappBrowserPolicy: {
    revision: 1,
    globalEnabled: true,
  },
  dappSandboxPackageCluster: 'mainnet-flight',
  dappNetwork: {
    cluster: 'mainnet',
    genesisFile: 'genesis.json',
    genesisHash: hash,
  },
};

beforeEach(() => {
  (readFileSync as jest.Mock).mockReturnValue(JSON.stringify(config));
});

it('reads and freezes the packaged dApp identity', () => {
  const windows = process.platform === 'win32';
  const result = readDappRuntimeConfig(
    windows
      ? 'C:\\opt\\daedalus\\config\\daedalus-config.json'
      : '/opt/daedalus/config/daedalus-config.json'
  );

  expect(result).toEqual({
    ...config,
    dappNetwork: {
      ...config.dappNetwork,
      genesisFile: windows
        ? 'C:\\opt\\daedalus\\config\\genesis.json'
        : '/opt/daedalus/config/genesis.json',
    },
  });
  expect(Object.isFrozen(result)).toBe(true);
  expect(Object.isFrozen(result.dappNetwork)).toBe(true);
});

it.each([
  ['relative path', 'daedalus-config.json', config],
  ['array root', '/config/daedalus-config.json', []],
  [
    'unknown package cluster',
    '/config/daedalus-config.json',
    { ...config, dappSandboxPackageCluster: 'nightly' },
  ],
  [
    'mismatched logical cluster',
    '/config/daedalus-config.json',
    { ...config, dappNetwork: { ...config.dappNetwork, cluster: 'preprod' } },
  ],
  [
    'absolute genesis file',
    '/config/daedalus-config.json',
    {
      ...config,
      dappNetwork: { ...config.dappNetwork, genesisFile: '/tmp/genesis.json' },
    },
  ],
  [
    'traversing genesis file',
    '/config/daedalus-config.json',
    {
      ...config,
      dappNetwork: { ...config.dappNetwork, genesisFile: '../genesis.json' },
    },
  ],
  [
    'uppercase genesis hash',
    '/config/daedalus-config.json',
    {
      ...config,
      dappNetwork: { ...config.dappNetwork, genesisHash: hash.toUpperCase() },
    },
  ],
])('rejects %s', (_name, configPath, value) => {
  (readFileSync as jest.Mock).mockReturnValue(JSON.stringify(value));

  expect(() => readDappRuntimeConfig(configPath)).toThrow(
    'Invalid Daedalus configuration'
  );
});

it('accepts a missing policy so launch policy parsing can fail closed', () => {
  const { dappBrowserPolicy: _policy, ...identity } = config;
  (readFileSync as jest.Mock).mockReturnValue(JSON.stringify(identity));

  expect(
    readDappRuntimeConfig('/config/daedalus-config.json').dappBrowserPolicy
  ).toBeUndefined();
});

it.each([
  undefined,
  null,
  123,
  {},
  '',
  'a'.repeat(39),
  'a'.repeat(41),
  'A'.repeat(40),
  'g'.repeat(40),
])('rejects malformed wallet revision %p', (cardanoWalletSourceRevision) => {
  (readFileSync as jest.Mock).mockReturnValue(
    JSON.stringify({ ...config, cardanoWalletSourceRevision })
  );

  expect(() => readDappRuntimeConfig('/config/daedalus-config.json')).toThrow(
    'Invalid Daedalus configuration'
  );
});

it.each([
  'f293d922f1930714aca7ede3cbb5ebf3fb710af6',
  'd408e4f29e46dc34454b160b3994ee9974b5cde2',
])('accepts full wallet revision %s', (cardanoWalletSourceRevision) => {
  (readFileSync as jest.Mock).mockReturnValue(
    JSON.stringify({ ...config, cardanoWalletSourceRevision })
  );

  expect(
    readDappRuntimeConfig('/config/daedalus-config.json')
      .cardanoWalletSourceRevision
  ).toBe(cardanoWalletSourceRevision);
});

it.each([
  ['mainnet', false, 'mainnet'],
  ['selfnode', false, 'selfnode'],
  ['preprod', true, 'preprod'],
  ['preview', true, 'preview'],
  ['mainnet-flight', true, 'mainnet'],
])(
  'requires the package capability for %s independently of launcher availability',
  (dappSandboxPackageCluster, cip30Enabled, cluster) => {
    (readFileSync as jest.Mock).mockReturnValue(
      JSON.stringify({
        ...config,
        cip30Enabled,
        dappSandboxPackageCluster,
        dappBrowserPolicy: {
          ...config.dappBrowserPolicy,
          globalEnabled: false,
        },
        dappNetwork: { ...config.dappNetwork, cluster },
      })
    );

    const result = readDappRuntimeConfig('/config/daedalus-config.json');

    expect(result.cip30Enabled).toBe(cip30Enabled);
    expect(result.dappSandboxPackageCluster).toBe(dappSandboxPackageCluster);
    expect(result.dappNetwork.cluster).toBe(cluster);
    expect(Object.isFrozen(result)).toBe(true);
  }
);

it.each([undefined, null, 0, 1, '', 'true', 'false', [], {}])(
  'rejects missing or malformed CIP-30 capability %p',
  (cip30Enabled) => {
    (readFileSync as jest.Mock).mockReturnValue(
      JSON.stringify({ ...config, cip30Enabled })
    );

    expect(() => readDappRuntimeConfig('/config/daedalus-config.json')).toThrow(
      'Invalid Daedalus configuration'
    );
  }
);

it.each([
  ['mainnet', true, 'mainnet'],
  ['selfnode', true, 'selfnode'],
  ['preprod', false, 'preprod'],
  ['preview', false, 'preview'],
  ['mainnet-flight', false, 'mainnet'],
])(
  'rejects a contradictory CIP-30 capability for %s',
  (dappSandboxPackageCluster, cip30Enabled, cluster) => {
    (readFileSync as jest.Mock).mockReturnValue(
      JSON.stringify({
        ...config,
        cip30Enabled,
        dappSandboxPackageCluster,
        dappNetwork: { ...config.dappNetwork, cluster },
      })
    );

    expect(() => readDappRuntimeConfig('/config/daedalus-config.json')).toThrow(
      'Invalid Daedalus configuration'
    );
  }
);
