/** @jest-environment node */
import { EventEmitter } from 'events';
import { deserialize, serialize } from 'v8';
import cbor from 'cbor';
import { ipcMain } from 'electron';
import { IpcChannel } from '../../common/ipc/lib/IpcChannel';
import {
  WALLET_APPROVAL_RENDER_CHANNEL,
  WALLET_TRANSACTION_APPROVAL_CHANNEL,
} from '../../common/ipc/api';
import type {
  WalletApprovalRenderMainRequest,
  WalletApprovalRenderRendererResponse,
  NativeTransactionPresentation,
  WalletTransactionApprovalMainResponse,
  WalletTransactionApprovalRendererRequest,
} from '../../common/ipc/api';
import { parseWalletApprovalRender } from '../../common/ipc/walletApproval';
import { createCip30TransactionReview } from '../../common/cip30/review';
import {
  decodeConwayOutput,
  decodeConwayTransaction,
} from '../../common/cardano/transaction';
import type { CommitmentContext } from '../../common/cardano/transaction';
import type { NativePreparedApproval } from '../../common/transactions/nativePlan';
import { parseConwayTransactionEnvelope } from '../../common/cardano/transactionEnvelope';
import semanticFixture from '../../common/cardano/fixtures/exact-cbor/semantic-conway-v1.json';
import { currentWindowSender } from './lib/currentWindowSender';
import { consentCoordinator } from './walletApproval';
import { handleNativeTransactionApprovalRequests } from './nativeTransactionApproval';

jest.mock('../utils/logging', () => ({ logger: { error: jest.fn() } }));
jest.mock('../config', () => ({ isFlight: false }));
jest.mock('electron', () => ({
  ipcMain: new (require('events').EventEmitter)(),
}));
jest.mock('./lib/trustedRendererIpcAuthority', () => ({
  authorizeTrustedRenderer: () => ({ isCurrent: () => true }),
  isTrustedRendererEvent: () => true,
  onTrustedRendererInvalidated: () => () => {},
}));
jest.mock('./dappBrowser', () => ({
  setDappBrowserConsentPending: jest.fn(),
  setDappConsentLifecycleRevoker: jest.fn(),
}));

afterEach(() => {
  consentCoordinator.cancel(() => true);
  ipcMain.removeAllListeners();
});

const createRendererEndpoint = () => {
  const renderer = new EventEmitter();
  const webContents = {
    id: 1,
    send: (name: string, envelope: unknown) =>
      renderer.emit(
        name,
        {
          sender: {
            send: (reply: string, response: unknown) =>
              ipcMain.emit(reply, { sender: webContents }, response),
          },
        },
        envelope
      ),
  };
  currentWindowSender.bind({ isDestroyed: () => false, webContents } as any);
  // The renderer has its own module registry in the real application.
  IpcChannel._instances = {};
  const endpoint = new IpcChannel<
    WalletApprovalRenderMainRequest,
    WalletApprovalRenderRendererResponse
  >(WALLET_APPROVAL_RENDER_CHANNEL);
  return { endpoint, renderer, webContents };
};

it('presents consent and dismisses it after a renderer refusal without executing the operation', async () => {
  const { endpoint, renderer } = createRendererEndpoint();
  let visible: string | undefined;
  let presentedOrigin: string | undefined;
  endpoint.onRequest(async (value) => {
    const message = parseWalletApprovalRender(value);
    if (
      message.type === 'present' &&
      message.request.kind !== 'native-transaction'
    ) {
      visible = message.request.requestId;
      presentedOrigin = message.request.origin;
      return { requestId: visible, approved: false };
    }
    if (message.type !== 'present' && visible === message.requestId)
      visible = undefined;
  }, renderer);
  const execute = jest.fn(async () => 'must not execute');
  await expect(
    consentCoordinator.request({
      identity: {
        kind: 'dapp',
        guestWebContentsId: 7,
        documentGeneration: 1,
        origin: 'https://example.test',
        connectionId: '',
        walletId: 'aa'.repeat(20),
        routeEpoch: 1,
        networkGenesis: 'genesis',
      },
      presentation: {
        kind: 'connection',
        origin: 'https://example.test',
        walletName: 'Wallet',
        networkName: 'Preview',
        scopes: ['connection', 'read'],
        extensions: [],
      },
      payload: {},
      declined: { type: 'api-error', value: { code: -3, info: 'Refused' } },
      execute,
    })
  ).rejects.toMatchObject({ type: 'api-error', value: { code: -3 } });
  expect(presentedOrigin).toBe('https://example.test');
  expect(visible).toBeUndefined();
  expect(execute).not.toHaveBeenCalled();
}, 1000);

it('keeps native Ledger approval pending until an explicit renderer decision', async () => {
  const { endpoint, renderer } = createRendererEndpoint();
  let visible: WalletApprovalRenderMainRequest | undefined;
  let terminalResult: WalletApprovalRenderMainRequest | undefined;
  let approve = (): void => {
    throw new Error('Native approval was not presented');
  };
  endpoint.onRequest(async (value) => {
    const message = parseWalletApprovalRender(value);
    if (message.type === 'present') {
      visible = message;
      return new Promise<WalletApprovalRenderRendererResponse>((resolve) => {
        approve = () =>
          resolve({
            requestId: message.request.requestId,
            approved: true,
          });
      });
    }
    if (message.type === 'terminal') {
      terminalResult = message;
      visible = undefined;
    }
    return undefined;
  }, renderer);
  const review = createCip30TransactionReview(
    decodeConwayTransaction(
      parseConwayTransactionEnvelope(
        Buffer.from(semanticFixture.cborHex, 'hex')
      )
    ),
    'sign',
    {
      outputs: [],
      ownership: [],
      network: { networkId: 0, networkMagic: 42, genesisHash: '00'.repeat(32) },
    }
  );
  const execute = jest.fn(async () => ({
    status: 'submitted' as const,
    transactionIds: ['55'.repeat(32)],
  }));
  const result = consentCoordinator.request({
    identity: {
      kind: 'native',
      trustedWebContentsId: 1,
      attemptId: 'native-attempt',
      walletId: 'aa'.repeat(20),
      networkGenesis: '00'.repeat(32),
    },
    presentation: {
      kind: 'native-transaction',
      attemptId: 'native-attempt',
      walletName: 'Ledger wallet',
      networkName: 'Preview',
      action: 'payment',
      authorization: { kind: 'hardware', vendor: 'ledger' },
      collection: 'single',
      acknowledgements: [],
      items: [{ kind: 'exact-cbor', review }],
    },
    payload: {},
    declined: {
      type: 'native-transaction-error',
      value: { code: 'user_declined', info: 'Transaction was rejected' },
    },
    execute,
  });
  const failure = jest.fn();
  result.catch(failure);
  await new Promise<void>((resolve) => {
    setTimeout(resolve, 0);
  });

  expect(visible).toMatchObject({
    type: 'present',
    request: {
      kind: 'native-transaction',
      items: [{ kind: 'exact-cbor', review }],
    },
  });
  expect(failure).not.toHaveBeenCalled();
  expect(execute).not.toHaveBeenCalled();
  approve();
  await expect(result).resolves.toEqual({
    status: 'submitted',
    transactionIds: ['55'.repeat(32)],
  });
  expect(execute).toHaveBeenCalledTimes(1);
  expect(visible).toBeUndefined();
  expect(terminalResult).toEqual({
    type: 'terminal',
    requestId: expect.any(String),
    result: {
      status: 'submitted',
      transactionIds: ['55'.repeat(32)],
    },
  });
}, 1000);
it('carries a signed-only dApp receipt without changing the signing result', async () => {
  const { endpoint, renderer } = createRendererEndpoint();
  let terminalResult: WalletApprovalRenderMainRequest | undefined;
  endpoint.onRequest(async (value) => {
    const message = parseWalletApprovalRender(value);
    if (message.type === 'present')
      return { requestId: message.request.requestId, approved: true };
    if (message.type === 'terminal') terminalResult = message;
    return undefined;
  }, renderer);
  const review = createCip30TransactionReview(
    decodeConwayTransaction(
      parseConwayTransactionEnvelope(
        Buffer.from(semanticFixture.cborHex, 'hex')
      )
    ),
    'sign',
    {
      outputs: [],
      ownership: [],
      network: { networkId: 0, networkMagic: 42, genesisHash: '00'.repeat(32) },
    }
  );

  const result = consentCoordinator.request({
    identity: {
      kind: 'dapp',
      guestWebContentsId: 7,
      documentGeneration: 1,
      origin: 'https://example.test',
      connectionId: 'connection',
      walletId: 'aa'.repeat(20),
      routeEpoch: 1,
      networkGenesis: '00'.repeat(32),
    },
    presentation: {
      kind: 'transaction-sign',
      origin: 'https://example.test',
      walletName: 'Wallet',
      networkName: 'Preview',
      scopes: ['transaction-signing'],
      extensions: [],
      authorization: { kind: 'software' },
      review,
    },
    payload: {},
    declined: {
      type: 'tx-sign-error',
      value: { code: 2, info: 'User declined' },
    },
    execute: async () => 'public-witness',
  });

  await expect(result).resolves.toBe('public-witness');
  expect(terminalResult).toEqual({
    type: 'terminal',
    requestId: expect.any(String),
    result: {
      status: 'signed',
      transactionIds: [review.transactionId],
    },
  });
}, 1000);


it('preserves authenticated input evidence when reviewing a native self-transfer', async () => {
  const { endpoint, renderer, webContents } = createRendererEndpoint();
  handleNativeTransactionApprovalRequests();
  const client = new IpcChannel<
    WalletTransactionApprovalMainResponse,
    WalletTransactionApprovalRendererRequest
  >(WALLET_TRANSACTION_APPROVAL_CHANNEL);
  const presented: NativeTransactionPresentation[] = [];
  endpoint.onRequest(async (value) => {
    const message = parseWalletApprovalRender(value);
    if (
      message.type === 'present' &&
      message.request.kind === 'native-transaction'
    ) {
      presented.push(message.request);
      return { requestId: message.request.requestId, approved: false };
    }
    return undefined;
  }, renderer);
  const paymentCredential = 'aa'.repeat(28);
  const stakeCredential = 'bb'.repeat(28);
  const address = Buffer.from(
    `01${paymentCredential}${stakeCredential}`,
    'hex'
  );
  const inputId = '11'.repeat(32);
  const input = [Buffer.from(inputId, 'hex'), 1];
  const sourceOutput = cbor.encodeCanonical([address, 410080283]);
  const transactionCbor = cbor
    .encodeCanonical([
      new Map<number, unknown>([
        [0, [input]],
        [
          1,
          [
            [address, 5000000],
            [address, 407460984],
          ],
        ],
        [2, 174565],
        [5, new Map([[Buffer.from(`e1${stakeCredential}`, 'hex'), 2555266]])],
      ]),
      new Map(),
      true,
      null,
    ])
    .toString('hex');
  const transactionEnvelope = parseConwayTransactionEnvelope(
    Buffer.from(transactionCbor, 'hex')
  );
  const commitments: CommitmentContext = {
    resolvedInputs: [
      {
        outpoint: decodeConwayTransaction(transactionEnvelope).inputs.normal[0],
        value: decodeConwayOutput(sourceOutput).value,
        walletMember: true,
      },
    ],
    verifiedWitnesses: new Set(),
    ownedPaymentCredentials: new Set([paymentCredential]),
  };
  const prepared: NativePreparedApproval = {
    walletId: 'ab'.repeat(20),
    network: {
      networkId: 1,
      networkMagic: 764824073,
      genesisHash: '00'.repeat(32),
    },
    action: 'payment',
    authorization: 'ledger',
    context: { walletName: 'Synthetic Ledger wallet', networkName: 'Mainnet' },
    items: [
      {
        kind: 'exact-cbor',
        cbor: transactionCbor,
        selectionFacts: { deposits: '0', refunds: '0' },
        transactionContext: {
          walletId: 'ab'.repeat(20),
          network: {
            networkId: 1,
            networkMagic: 764824073,
            genesisHash: '00'.repeat(32),
          },
          chainPoint: { kind: 'genesis' },
          walletGeneration: BigInt(1),
          pendingGeneration: BigInt(1),
          contextDigest: 'cc'.repeat(32),
          contextToken: 'dd'.repeat(32),
          records: [],
          transactions: [transactionCbor],
          outputs: [
            {
              outpoint: { transactionId: inputId, index: 1 },
              sourceCbor: sourceOutput.toString('hex'),
              inputCbor: cbor.encodeCanonical(input).toString('hex'),
              canonicalCbor: sourceOutput.toString('hex'),
              unspentCbor: cbor
                .encodeCanonical([input, [address, 410080283]])
                .toString('hex'),
              provenance: ['node'],
              roles: ['normal'],
              walletMember: true,
              pendingState: 'none',
            },
          ],
          pendingTransactions: [],
          ownership: [
            {
              credentialKind: 'payment',
              credential: paymentCredential,
              ownership: 'owned_key',
              derivationPath: [],
              proofKinds: ['normal_input'],
            },
            {
              credentialKind: 'stake',
              credential: stakeCredential,
              ownership: 'owned_key',
              derivationPath: [],
              proofKinds: ['withdrawal'],
            },
          ],
          requiredProofs: [],
          commitmentContexts: [commitments],
          transactionsSemantic: [
            decodeConwayTransaction(
              parseConwayTransactionEnvelope(
                Buffer.from(transactionCbor, 'hex')
              ),
              commitments
            ),
          ],
          preExistingWitnesses: [],
        },
      },
    ],
  };
  const send = () =>
    client.request(
      {
        type: 'request-native',
        attemptId: 'native-context-attempt',
        walletId: prepared.walletId,
        network: prepared.network,
        prepared,
      },
      {
        send: (name, envelope) =>
          ipcMain.emit(
            name,
            { sender: webContents },
            deserialize(serialize(envelope))
          ),
      },
      renderer
    );
  await send();
  const reviewItem = presented[0]?.items[0];
  if (reviewItem?.kind !== 'exact-cbor')
    throw new Error('Missing exact review');
  expect(reviewItem.review).toMatchObject({
    approvable: true,
    commitmentsVerified: true,
    refusalReasons: [],
    fullCbor: transactionCbor,
    display: {
      fee: '174565',
      walletChange: { coin: '2380701', assets: [] },
      withdrawals: [{ coin: '2555266', ownership: 'wallet' }],
    },
  });

  const preparedItem = prepared.items[0];
  if (preparedItem.kind !== 'exact-cbor')
    throw new Error('Missing exact preparation');
  const snapshot = preparedItem.transactionContext;
  // Presentation must still refuse absent input proof, even with cached semantics.
  Object.assign(snapshot, {
    commitmentContexts: [{ ...commitments, resolvedInputs: [] }],
  });
  await send();
  expect(presented[1].items[0]).toMatchObject({
    review: {
      approvable: false,
      commitmentsVerified: false,
      refusalReasons: [
        `resolved-input:${inputId}:1:missing authenticated resolved input`,
      ],
    },
  });
  Object.assign(snapshot, {
    transactions: [],
    commitmentContexts: [commitments],
  });
  expect(await send()).toMatchObject({
    result: { status: 'rejected', errorCode: 'failed' },
  });
  expect(presented).toHaveLength(2);
}, 1000);
