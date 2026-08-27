import { parseDappConnectionsRequest } from './dappConnections';

jest.mock('../cip30/Cip30Broker', () => ({
  getDappConnectionService: jest.fn(),
}));

const identity = {
  origin: 'https://example.com',
  walletId: 'wallet-1',
  networkGenesis: 'genesis-1',
  launch: { kind: 'diagnostics' as const },
};

describe('dApp connection IPC validation', () => {
  it('accepts only exact management shapes and disclosure scopes', () => {
    expect(
      parseDappConnectionsRequest({
        type: 'revoke-scope',
        identity,
        scope: 'governance-key-disclosure',
      })
    ).toEqual({
      type: 'revoke-scope',
      identity,
      scope: 'governance-key-disclosure',
    });
    expect(() =>
      parseDappConnectionsRequest({
        type: 'revoke-scope',
        identity,
        scope: 'transaction-signing',
      })
    ).toThrow('Invalid request');
    expect(() =>
      parseDappConnectionsRequest({ type: 'list', origin: 'private' })
    ).toThrow('Invalid request');
  });

  it('rejects duplicate, malformed, and oversized wallet pruning inputs', () => {
    expect(
      parseDappConnectionsRequest({
        type: 'prune-wallets',
        walletIds: ['wallet-1', 'wallet-2'],
      })
    ).toEqual({
      type: 'prune-wallets',
      walletIds: ['wallet-1', 'wallet-2'],
    });
    expect(() =>
      parseDappConnectionsRequest({
        type: 'prune-wallets',
        walletIds: ['wallet-1', 'wallet-1'],
      })
    ).toThrow('Invalid request');
    expect(() =>
      parseDappConnectionsRequest({
        type: 'forget',
        identity: { ...identity, origin: '' },
      })
    ).toThrow('Invalid request');
  });
});
