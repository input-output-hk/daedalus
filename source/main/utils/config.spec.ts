import { readFileSync } from 'fs';

import { readDappRuntimeConfig } from './config';

jest.mock('fs', () => ({ readFileSync: jest.fn() }));

const hash = 'a'.repeat(64);
const config = {
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
  const result = readDappRuntimeConfig(
    '/opt/daedalus/config/daedalus-config.json'
  );

  expect(result).toEqual({
    ...config,
    dappNetwork: {
      ...config.dappNetwork,
      genesisFile: '/opt/daedalus/config/genesis.json',
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
