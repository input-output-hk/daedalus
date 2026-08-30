import type { DappConsentPresentation } from '../../../common/ipc/api';
import type { Api } from '../api';
import type { ActionsMap } from '../actions';
import type { AnalyticsTracker } from '../analytics';
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
  new Cip30ConsentStore(
    (undefined as unknown) as Api,
    (undefined as unknown) as ActionsMap,
    (undefined as unknown) as AnalyticsTracker
  );

const transactionReview = {
  mode: 'sign' as const,
  transactionId: '11'.repeat(32),
  bodyCbor: 'a0',
  fullCbor: '84a0a0f5f6',
  fullCborDigest: '22'.repeat(32),
  witnessSetCbor: 'a0',
  auxiliaryDataCbor: 'f6',
  isValid: true,
  effects: [],
  existingVkeyWitnesses: [],
  existingBootstrapWitnesses: [],
  commitmentsVerified: true,
  approvable: true,
  refusalReasons: [],
};

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

  it('forwards a passphrase for single/batch signing but never submission', async () => {
    const signing = createStore();
    const signRequest: DappConsentPresentation = {
      ...request,
      kind: 'transaction-sign',
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
});
