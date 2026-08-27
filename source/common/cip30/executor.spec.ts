import { parseCip30WalletRequest, parseCip30WalletResponse } from './executor';

const network = {
  networkId: 0 as const,
  networkMagic: 42,
  genesisHash: '11'.repeat(32),
};
const request = {
  operation: 'capabilities' as const,
  walletId: 'wallet',
  network,
  sourceRevision: '22'.repeat(20),
};

describe('CIP-30 wallet executor contract', () => {
  it('accepts only exact plain requests without invoking accessors', () => {
    expect(parseCip30WalletRequest(request)).toEqual(request);
    const getter = jest.fn(() => 'wallet');
    const malicious = { ...request } as Record<string, unknown>;
    Object.defineProperty(malicious, 'walletId', {
      enumerable: true,
      get: getter,
    });
    expect(() => parseCip30WalletRequest(malicious)).toThrow(
      'Invalid CIP-30 wallet request'
    );
    expect(getter).not.toHaveBeenCalled();
  });

  it('binds fulfilled responses to the requested wallet and network', () => {
    expect(
      parseCip30WalletResponse(request, {
        status: 'fulfilled',
        operation: 'capabilities',
        value: {
          walletId: 'wallet',
          walletName: 'Wallet',
          walletKind: 'shelley-software',
          network,
          backendApiVersion: 1,
          backendExtensions: [95, 103],
        },
      })
    ).toMatchObject({ status: 'fulfilled', operation: 'capabilities' });
    expect(() =>
      parseCip30WalletResponse(request, {
        status: 'fulfilled',
        operation: 'capabilities',
        value: {
          walletId: 'other',
          walletName: 'Wallet',
          walletKind: 'shelley-software',
          network,
          backendApiVersion: 1,
          backendExtensions: [95, 103],
        },
      })
    ).toThrow('Invalid CIP-30 wallet capabilities');
  });

  it('permits plain context data and fixed rejection reasons only', () => {
    const contextRequest = { ...request, operation: 'context' as const };
    expect(
      parseCip30WalletResponse(contextRequest, {
        status: 'fulfilled',
        operation: 'context',
        value: { revision: 1, outputs: [] },
      })
    ).toMatchObject({ status: 'fulfilled', operation: 'context' });
    expect(
      parseCip30WalletResponse(contextRequest, {
        status: 'rejected',
        reason: 'account-change',
      })
    ).toEqual({ status: 'rejected', reason: 'account-change' });
    expect(() =>
      parseCip30WalletResponse(contextRequest, {
        status: 'rejected',
        reason: 'backend-detail',
      })
    ).toThrow('Invalid CIP-30 wallet response');
  });
});
