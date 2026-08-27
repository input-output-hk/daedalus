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

  it('accepts only exact transient sign-data requests and fixed results', () => {
    const signDataRequest = {
      ...request,
      operation: 'sign-data' as const,
      address: `60${'11'.repeat(28)}`,
      payload: '00',
      passphrase: 'secret',
    };
    expect(parseCip30WalletRequest(signDataRequest)).toEqual(signDataRequest);
    for (const value of [
      { ...signDataRequest, passphrase: 1 },
      { ...signDataRequest, payload: '0' },
      { ...signDataRequest, replacement: '00' },
    ])
      expect(() => parseCip30WalletRequest(value)).toThrow(
        'Invalid CIP-30 wallet request'
      );
    expect(
      parseCip30WalletResponse(signDataRequest, {
        status: 'fulfilled',
        operation: 'sign-data',
        value: {
          revision: 1,
          credential_kind: 'payment',
          credential: '11'.repeat(28),
          cose_sign1: '00',
          cose_key: '00',
        },
      })
    ).toMatchObject({ status: 'fulfilled', operation: 'sign-data' });
    expect(
      parseCip30WalletResponse(signDataRequest, {
        status: 'rejected',
        reason: 'proof-generation',
      })
    ).toEqual({ status: 'rejected', reason: 'proof-generation' });
  });
  it('accepts only exact raw CIP-95 key-state results', () => {
    const keyStateRequest = {
      ...request,
      operation: 'cip95-key-state' as const,
    };
    expect(parseCip30WalletRequest(keyStateRequest)).toEqual(keyStateRequest);
    const keyState = {
      drep_public_key: '33'.repeat(32),
      registered_stake_public_keys: ['44'.repeat(32)],
      unregistered_stake_public_keys: ['55'.repeat(32)],
    };
    expect(
      parseCip30WalletResponse(keyStateRequest, {
        status: 'fulfilled',
        operation: 'cip95-key-state',
        value: keyState,
      })
    ).toEqual({
      status: 'fulfilled',
      operation: 'cip95-key-state',
      value: keyState,
    });
    for (const value of [
      { ...keyState, drep_public_key: '33'.repeat(31) },
      {
        ...keyState,
        registered_stake_public_keys: ['AA'.repeat(32)],
      },
      { ...keyState, extra: true },
    ])
      expect(() =>
        parseCip30WalletResponse(keyStateRequest, {
          status: 'fulfilled',
          operation: 'cip95-key-state',
          value,
        })
      ).toThrow('Invalid CIP-95 key state');
    const getter = jest.fn(() => '33'.repeat(32));
    const malicious = { ...keyState } as Record<string, unknown>;
    Object.defineProperty(malicious, 'drep_public_key', {
      enumerable: true,
      get: getter,
    });
    expect(() =>
      parseCip30WalletResponse(keyStateRequest, {
        status: 'fulfilled',
        operation: 'cip95-key-state',
        value: malicious,
      })
    ).toThrow('Invalid CIP-95 key state');
    expect(getter).not.toHaveBeenCalled();
  });
});
