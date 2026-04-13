import {
  resolveNetworkConfig,
  normalizeVerificationKey,
  buildMithrilEnv,
  MITHRIL_NETWORK_CONFIG,
} from './mithrilNetworkConfig';

jest.mock('../environment', () => ({
  environment: {
    network: 'mainnet',
  },
}));

jest.mock('../utils/logging', () => ({
  logger: {
    warn: jest.fn(),
  },
}));

describe('resolveNetworkConfig', () => {
  it('returns mainnet config when network is mainnet', () => {
    const config = resolveNetworkConfig();
    expect(config).toBe(MITHRIL_NETWORK_CONFIG.mainnet);
    expect(config.aggregatorEndpoint).toContain('release-mainnet');
  });

  it('throws for unsupported networks', () => {
    jest.resetModules();
    jest.doMock('../environment', () => ({
      environment: { network: 'sanchonet' },
    }));
    // Re-require after mock reset to pick up new network
    const { resolveNetworkConfig: resolve } = require('./mithrilNetworkConfig');
    expect(() => resolve()).toThrow(
      'Mithril not supported for network: sanchonet'
    );
  });
});

describe('normalizeVerificationKey', () => {
  it('returns valid hex strings as-is', () => {
    const hex = 'deadbeef';
    expect(normalizeVerificationKey(hex)).toBe('deadbeef');
  });

  it('converts byte array JSON to hex string', () => {
    const byteArray = JSON.stringify([0, 1, 255, 16]);
    expect(normalizeVerificationKey(byteArray)).toBe('0001ff10');
  });

  it('trims whitespace from plain strings', () => {
    expect(normalizeVerificationKey('  abc123  ')).toBe('abc123');
  });

  it('returns trimmed string when byte array has invalid values', () => {
    const result = normalizeVerificationKey('[1, 999, 2]');
    // 999 is out of byte range — should fall through to error handler and return trimmed
    expect(result).toBe('[1, 999, 2]');
  });

  it('returns trimmed string for odd-length hex (not pure hex)', () => {
    // Odd length fails the hex check, falls through to plain trim
    expect(normalizeVerificationKey(' abc ')).toBe('abc');
  });
});

describe('buildMithrilEnv', () => {
  it('includes AGGREGATOR_ENDPOINT in env', async () => {
    const env = await buildMithrilEnv(false);
    expect(env.AGGREGATOR_ENDPOINT).toBe(
      MITHRIL_NETWORK_CONFIG.mainnet.aggregatorEndpoint
    );
  });

  it('does not include verification keys when requireKeys is false', async () => {
    const env = await buildMithrilEnv(false);
    expect(env.GENESIS_VERIFICATION_KEY).toBeUndefined();
    expect(env.ANCILLARY_VERIFICATION_KEY).toBeUndefined();
  });
});
