import type { Environment } from '../common/types/environment.types';

describe('environment selected by the shortcut configuration', () => {
  const originalEnv = process.env;

  beforeEach(() => {
    jest.resetModules();
    process.env = {
      ...originalEnv,
      NODE_ENV: 'production',
      NETWORK: 'mainnet',
    };
    delete process.env.DAEDALUS_NETWORK_NAME;
  });

  afterEach(() => {
    process.env = originalEnv;
    jest.resetModules();
  });

  const load = (): Environment => require('./environment').environment;

  it.each(['preview', 'preprod'])(
    'uses the existing runtime %s network instead of the shared build network',
    (network) => {
      process.env.DAEDALUS_NETWORK_NAME = network;
      const environment = load();
      expect(environment.network).toBe(network);
      expect(environment.isPreview).toBe(network === 'preview');
      expect(environment.isPreprod).toBe(network === 'preprod');
      expect(environment.isMainnet).toBe(false);
    }
  );

  it('keeps Flight on mainnet when its configuration selects mainnet', () => {
    process.env.NETWORK = 'preprod';
    process.env.DAEDALUS_NETWORK_NAME = 'mainnet';
    process.env.DAEDALUS_IS_FLIGHT = 'true';
    const environment = load();
    expect(environment.network).toBe('mainnet');
    expect(environment.isMainnet).toBe(true);
    expect(environment.isPreprod).toBe(false);
  });

  it('preserves standalone Mainnet', () => {
    process.env.DAEDALUS_NETWORK_NAME = 'mainnet';
    expect(load().isMainnet).toBe(true);
  });

  it.each(['preprod', 'mainnet-flight'])(
    'preserves the %s build fallback without a runtime network',
    (network) => {
      process.env.NETWORK = network;
      expect(load().network).toBe(
        network === 'mainnet-flight' ? 'mainnet' : network
      );
    }
  );

  it('preserves the development fallback without either network variable', () => {
    delete process.env.NETWORK;
    expect(load().isDevelopment).toBe(true);
  });
});
