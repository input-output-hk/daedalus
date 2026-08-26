import Cip30ConsentStore from './Cip30ConsentStore';

jest.mock('../ipc/dappConsent', () => ({
  bindDappConsentRenderer: jest.fn(() => jest.fn()),
}));

const request = {
  requestId: 'request-1',
  kind: 'connection' as const,
  origin: 'https://example.test',
  walletName: 'Wallet',
  networkName: 'Preview',
  scopes: ['connection', 'read'],
  extensions: [95],
};

const createStore = () =>
  new Cip30ConsentStore(undefined as any, undefined as any, undefined as any);

describe('Cip30ConsentStore', () => {
  it('correlates approval by main-issued ID and restores trusted focus', async () => {
    const store = createStore();
    const origin = document.createElement('button');
    document.body.appendChild(origin);
    origin.focus();
    const decision = store.receive({ type: 'present', request });

    store.approve();
    store.approve();
    await expect(decision).resolves.toEqual({
      requestId: request.requestId,
      approved: true,
    });

    await store.receive({ type: 'terminal', requestId: 'stale' });
    expect(store.current).toEqual(request);
    await store.receive({ type: 'terminal', requestId: request.requestId });
    expect(store.current).toBeNull();
    expect(document.activeElement).toBe(origin);
    origin.remove();
  });

  it('returns only request identity and refusal', async () => {
    const store = createStore();
    const decision = store.receive({ type: 'present', request });
    store.reject();

    await expect(decision).resolves.toEqual({
      requestId: request.requestId,
      approved: false,
    });
  });
});
