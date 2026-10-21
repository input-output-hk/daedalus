import { parseDappCollateralRequest } from './collateral';

jest.mock('../config', () => ({
  dappRuntimeConfig: {
    cip30Enabled: true,
    dappSandboxPackageCluster: 'preprod',
  },
}));
jest.mock('../cip30/Cip30Broker', () => ({
  getCollateralService: jest.fn(),
}));
jest.mock('./dappBrowser', () => ({
  getCurrentDappRouteLease: jest.fn(),
}));
jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn(() => ({ onRequest: jest.fn() })),
}));
jest.mock('./lib/trustedRendererIpcAuthority', () => ({
  onTrustedRendererInvalidated: jest.fn(),
}));

const preparation = {
  operationId: '12345678-1234-4123-8123-123456789abc',
  walletId: 'ab'.repeat(20),
  networkGenesis: 'cd'.repeat(32),
};
const track = {
  type: 'track-preparation' as const,
  preparation,
  transactionId: '12'.repeat(32),
};

describe('collateral IPC', () => {
  it.each(['snapshot', 'prepare', 'clear', 'repair'] as const)(
    'accepts only the exact %s request',
    (type) => {
      expect(parseDappCollateralRequest({ type })).toEqual({ type });
      expect(() =>
        parseDappCollateralRequest({ type, walletId: 'renderer' })
      ).toThrow('Invalid collateral request');
    }
  );

  it('accepts exact operation-bound completion and cancellation', () => {
    expect(parseDappCollateralRequest(track)).toEqual(track);
    expect(
      parseDappCollateralRequest({ type: 'cancel-preparation', preparation })
    ).toEqual({ type: 'cancel-preparation', preparation });
  });

  it.each([
    { ...track, transactionId: '12' },
    { ...track, preparation: undefined },
    { type: 'cancel-preparation' },
    ...[
      { operationId: 'forged' },
      { walletId: 'ab' },
      { networkGenesis: 'cd' },
      { routeEpoch: 1 },
    ].map((fields) => ({
      ...track,
      preparation: { ...preparation, ...fields },
    })),
  ])('rejects malformed operation identity %#', (value) => {
    expect(() => parseDappCollateralRequest(value)).toThrow(
      'Invalid collateral request'
    );
  });

  it.each([undefined, null, {}, { type: 'submit' }, [], 'snapshot'])(
    'rejects malformed request %#',
    (value) => {
      expect(() => parseDappCollateralRequest(value)).toThrow(
        'Invalid collateral request'
      );
    }
  );

  it('rejects unknown symbols, non-enumerables and accessors without reading them', () => {
    const getter = jest.fn(() => preparation);
    const malformed = [
      { ...track, [Symbol('extra')]: true },
      Object.defineProperty({ ...track }, 'extra', { value: true }),
      Object.defineProperty({ ...track }, 'preparation', { get: getter }),
      { ...track, preparation: { ...preparation, [Symbol('extra')]: true } },
      {
        ...track,
        preparation: Object.defineProperty({ ...preparation }, 'extra', {
          value: true,
        }),
      },
      {
        ...track,
        preparation: Object.defineProperty({ ...preparation }, 'walletId', {
          get: getter,
        }),
      },
      Object.defineProperty({}, 'type', { get: getter }),
      Object.create(track),
    ];
    for (const value of malformed)
      expect(() => parseDappCollateralRequest(value)).toThrow(
        'Invalid collateral request'
      );
    expect(getter).not.toHaveBeenCalled();
  });

  it('validates non-enumerable own data fields instead of overlooking them', () => {
    const value = Object.defineProperties(
      {},
      {
        type: { value: 'cancel-preparation' },
        preparation: { value: preparation },
      }
    );
    expect(parseDappCollateralRequest(value)).toEqual({
      type: 'cancel-preparation',
      preparation,
    });
  });
});
