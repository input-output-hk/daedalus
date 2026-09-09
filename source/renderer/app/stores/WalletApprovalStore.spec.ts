import { createMemoryHistory } from 'history';
import { RouterStore, syncHistoryWithStore } from 'mobx-react-router';
import RouterActions from '../actions/router-actions';
import AppStore from './AppStore';
import WalletsStore from './WalletsStore';
import type { DappConsentPresentation } from '../../../common/ipc/api';
import type { Api } from '../api';
import type { ActionsMap } from '../actions';
import type { AnalyticsTracker } from '../analytics';
import WalletApprovalStore from './WalletApprovalStore';

jest.mock('../ipc/walletApproval', () => ({
  bindWalletApprovalRenderer: jest.fn(() => jest.fn()),
}));

const request = {
  requestId: 'request-1',
  walletId: 'ab'.repeat(20),
  kind: 'connection' as const,
  origin: 'https://example.test',
  walletName: 'Wallet',
  networkName: 'Preview',
  scopes: ['connection', 'read'],
  extensions: [95],
};

const createStore = () => {
  const apiMethod = jest.fn();
  const apiNamespace = new Proxy({}, { get: () => apiMethod });
  const api = new Proxy({}, { get: () => apiNamespace }) as Api;
  const actions = { router: new RouterActions() } as ActionsMap;
  const history = createMemoryHistory({
    initialEntries: [`/wallets/${'cd'.repeat(20)}/send`],
  });
  const router = new RouterStore();
  syncHistoryWithStore(history, router);
  const app = new AppStore(api, actions, undefined as never);
  app.configure({ router } as never);
  actions.router.goToRoute.listen(app._updateRouteLocation);
  const store = new WalletApprovalStore(
    api,
    actions,
    (undefined as unknown) as AnalyticsTracker
  );
  store.stores = {
    router,
    wallets: new WalletsStore(api, actions, undefined as never),
    transactions: {
      trackSubmission: jest.fn(async () => undefined),
      getTransaction: jest.fn(),
      dismissReceipt: jest.fn(),
      openTransaction: jest.fn(),
    },
  } as any;
  return store;
};

const transactionReview = {
  mode: 'sign' as const,
  transactionId: '11'.repeat(32),
  bodyCbor: 'a0',
  fullCbor: '84a0a0f5f6',
  fullCborDigest: '22'.repeat(32),
  witnessSetCbor: 'a0',
  auxiliaryDataCbor: 'f6',
  isValid: true,
  display: {
    entries: [],
    walletInputs: null,
    walletOutputs: null,
    walletChange: null,
    fee: '0',
    deposits: null,
    refunds: null,
    maximumCollateralLoss: null,
    mint: [],
    withdrawals: [],
    certificates: [],
    votes: [],
    proposalCount: 0,
    donation: null,
  },
  effects: [],
  existingVkeyWitnesses: [],
  existingBootstrapWitnesses: [],
  commitmentsVerified: true,
  approvable: true,
  refusalReasons: [],
};

const nativeRequest = {
  requestId: 'native-request',
  walletId: 'ab'.repeat(20),
  kind: 'native-transaction' as const,
  attemptId: 'attempt',
  walletName: 'Wallet',
  networkName: 'Mainnet',
  action: 'payment' as const,
  authorization: { kind: 'hardware' as const, vendor: 'ledger' as const },
  collection: 'single' as const,
  acknowledgements: [],
  items: [],
};

describe('WalletApprovalStore', () => {
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

  it('replaces a stale presentation before accepting a native transaction', async () => {
    const store = createStore();
    const staleDecision = store.receive({ type: 'present', request });
    const nativeDecision = store.receive({
      type: 'present',
      request: nativeRequest,
    });

    await expect(staleDecision).resolves.toEqual({
      requestId: request.requestId,
      approved: false,
    });
    expect(store.current).toEqual(nativeRequest);
    store.approve();
    await expect(nativeDecision).resolves.toEqual({
      requestId: nativeRequest.requestId,
      approved: true,
    });
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

  it('forwards a transient passphrase only for approved data signing', async () => {
    const store = createStore();
    const decision = store.receive({
      type: 'present',
      request: {
        ...request,
        kind: 'data-sign',
        review: {
          address: `60${'11'.repeat(28)}`,
          credentialKind: 'payment',
          payload: '00',
          utf8Preview: null,
        },
      },
    });
    store.approve('secret');
    await expect(decision).resolves.toEqual({
      requestId: request.requestId,
      approved: true,
      passphrase: 'secret',
    });
  });

  it('forwards a transient passphrase for account-key disclosure', async () => {
    const store = createStore();
    const decision = store.receive({
      type: 'present',
      request: {
        ...request,
        kind: 'key-disclosure',
        scopes: ['account-public-key-disclosure'],
        extensions: [104],
        requiresPassphrase: true,
      },
    });
    store.approve('secret');
    await expect(decision).resolves.toEqual({
      requestId: request.requestId,
      approved: true,
      passphrase: 'secret',
    });
  });

  it('forwards a passphrase for single/batch signing but never submission', async () => {
    const signing = createStore();
    const signRequest: DappConsentPresentation = {
      ...request,
      kind: 'transaction-sign',
      authorization: { kind: 'software' },
      review: transactionReview,
    };
    const signDecision = signing.receive({
      type: 'present',
      request: signRequest,
    });
    signing.approve('secret');
    await expect(signDecision).resolves.toEqual({
      requestId: request.requestId,
      approved: true,
      passphrase: 'secret',
    });

    const batchSigning = createStore();
    const batchSignRequest: DappConsentPresentation = {
      ...request,
      kind: 'batch-sign',
      authorization: { kind: 'software' },
      review: {
        mode: 'sign',
        approvable: true,
        items: [],
      },
    };
    const batchSignDecision = batchSigning.receive({
      type: 'present',
      request: batchSignRequest,
    });
    batchSigning.approve('batch-secret');
    await expect(batchSignDecision).resolves.toEqual({
      requestId: request.requestId,
      approved: true,
      passphrase: 'batch-secret',
    });

    const submission = createStore();
    const submitRequest: DappConsentPresentation = {
      ...request,
      kind: 'transaction-submit',
      authorization: { kind: 'none' },
      review: { ...transactionReview, mode: 'submit' },
    };
    const submitDecision = submission.receive({
      type: 'present',
      request: submitRequest,
    });
    submission.approve('must-not-cross');
    await expect(submitDecision).resolves.toEqual({
      requestId: request.requestId,
      approved: true,
    });
  });
  it('accepts only correlated execution progress for the active item', async () => {
    const store = createStore();
    const decision = store.receive({
      type: 'present',
      request: {
        ...request,
        kind: 'transaction-sign',
        authorization: { kind: 'hardware', vendor: 'ledger' },
        review: transactionReview,
      },
    });
    store.approve();
    await decision;

    await store.receive({
      type: 'progress',
      requestId: request.requestId,
      phase: 'waiting-for-device',
      itemIndex: 0,
      submissionAuthorized: false,
    });
    expect(store.phase).toBe('waiting-for-device');
    expect(store.activeItemIndex).toBe(0);

    await store.receive({
      type: 'progress',
      requestId: 'stale',
      phase: 'submitting',
      itemIndex: 0,
      submissionAuthorized: true,
    });
    expect(store.phase).toBe('waiting-for-device');
    await store.receive({ type: 'terminal', requestId: request.requestId });
    expect(store.current).toBeNull();
  });

  it('routes a dismissed native receipt to its wallet history and releases teardown waits', async () => {
    const store = createStore();
    const decision = store.receive({ type: 'present', request: nativeRequest });
    store.approve();
    await decision;
    const terminal = store.receive({
      type: 'terminal',
      requestId: nativeRequest.requestId,
      result: {
        status: 'submitted',
        transactionIds: ['55'.repeat(32)],
      },
    });
    const duplicateTerminal = store.receive({
      type: 'terminal',
      requestId: nativeRequest.requestId,
      result: { status: 'rejected', errorCode: 'failed' },
    });
    expect(duplicateTerminal).toBe(terminal);
    const settled = jest.fn();
    terminal.then(settled);
    await Promise.resolve();

    expect(store.current).toEqual(nativeRequest);
    expect(store.result).toEqual({
      status: 'submitted',
      transactionIds: ['55'.repeat(32)],
    });
    expect(settled).not.toHaveBeenCalled();

    store.dismissResult();
    await expect(terminal).resolves.toBeUndefined();
    expect(store.current).toBeNull();
    expect(store.stores.router.location.pathname).toBe(
      `/wallets/${nativeRequest.walletId}/transactions`
    );

    store.receive({ type: 'present', request: nativeRequest });
    const teardownTerminal = store.receive({
      type: 'terminal',
      requestId: nativeRequest.requestId,
      result: { status: 'rejected', errorCode: 'failed' },
    });
    store.teardown();
    await expect(teardownTerminal).resolves.toBeUndefined();
  });

  it('clears a retryable native attempt without retaining a result', async () => {
    const store = createStore();
    const decision = store.receive({ type: 'present', request: nativeRequest });
    store.approve('wrong');
    await decision;

    await store.receive({
      type: 'terminal',
      requestId: nativeRequest.requestId,
    });
    expect(store.current).toBeNull();
    expect(store.result).toBeUndefined();

    const retry = {
      ...nativeRequest,
      requestId: 'native-retry',
      attemptId: 'attempt-retry',
    };
    store.receive({ type: 'present', request: retry });
    expect(store.current).toEqual(retry);
  });
});
