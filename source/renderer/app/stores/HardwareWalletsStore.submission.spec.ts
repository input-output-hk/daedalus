import * as http from 'http';
import * as https from 'https';
import { URL } from 'url';
import type { AddressInfo } from 'net';
import HardwareWalletsStore from './HardwareWalletsStore';
import TransactionsStore from './TransactionsStore';
import { noopAnalyticsTracker } from '../analytics';
import type { Api } from '../api';
import type { ActionsMap } from '../actions';
import type { StoresMap } from './index';
import type { RequestConfig } from '../api/common/types';
import type AdaApiClass from '../api/api';

jest.mock('@trezor/connect', () => ({}));
jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

// Only TLS is replaced: the real API, request serializer and HTTP body are used.
// The server is an isolated loopback fixture, never a wallet backend.
const originalHttps = global.https;
global.https = https;
const AdaApi = require('../api/api').default as typeof AdaApiClass;

const originalGate = global.cip30Enabled;
const signedTransaction =
  '84a1008182582000017f80ff02030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e00a0f5f6';
const transactionId = 'ab'.repeat(32);
let server: http.Server;
let hardwareWallets: HardwareWalletsStore;

beforeEach(() => {
  global.cip30Enabled = false;
});
afterEach(async () => {
  if (hardwareWallets)
    clearInterval(hardwareWallets.checkTransactionTimeInterval);
  jest.restoreAllMocks();
  if (server)
    await new Promise<void>((resolve, reject) => {
      server.close((error) => (error ? reject(error) : resolve()));
    });
  global.cip30Enabled = originalGate;
});
afterAll(() => {
  global.https = originalHttps;
});

it.each([202, 400])(
  'submits verified hardware hex as exact bytes and releases the wallet after HTTP %s',
  async (statusCode) => {
    const received: Array<{
      method: string;
      path: string;
      headers: http.IncomingHttpHeaders;
      body: Buffer;
    }> = [];
    server = http.createServer((request, response) => {
      const chunks: Buffer[] = [];
      request.on('data', (chunk: Buffer) => chunks.push(chunk));
      request.on('end', () => {
        received.push({
          method: request.method,
          path: request.url,
          headers: request.headers,
          body: Buffer.concat(chunks),
        });
        response.writeHead(statusCode, { 'Content-Type': 'application/json' });
        const result = JSON.stringify(
          statusCode === 202
            ? { id: transactionId }
            : {
                code: 'created_invalid_transaction',
                message: 'Fixture backend rejected the transaction',
              }
        );
        response.write(result.slice(0, 12));
        response.end(result.slice(12));
      });
    });
    await new Promise<void>((resolve) =>
      server.listen(0, '127.0.0.1', resolve)
    );
    jest
      .spyOn(https, 'request')
      .mockImplementation((options: https.RequestOptions | string | URL) => {
        if (typeof options === 'string' || options instanceof URL) {
          throw new Error('Expected wallet request options');
        }
        return http.request({ ...options, agent: false });
      });
    const config: RequestConfig = {
      hostname: '127.0.0.1',
      port: (server.address() as AddressInfo).port,
      ca: new Uint8Array(),
      cert: new Uint8Array(),
      key: new Uint8Array(),
    };
    const api = {
      ada: new AdaApi(false, config),
      localStorage: {},
    } as unknown as Api;
    const actions = {
      dialogs: { closeActiveDialog: { trigger: jest.fn() } },
    } as unknown as ActionsMap;
    hardwareWallets = new HardwareWalletsStore(
      api,
      actions,
      noopAnalyticsTracker
    );
    const transactions = new TransactionsStore(
      api,
      actions,
      noopAnalyticsTracker
    );
    const stores = {
      hardwareWallets,
      transactions,
      wallets: { active: { id: 'wallet' }, refreshWalletsData: jest.fn() },
    } as unknown as StoresMap;
    hardwareWallets.configure(stores);
    transactions.configure(stores);
    // The authenticated Ledger/Trezor verification channel returns CBOR hex.
    hardwareWallets.signedTx = signedTransaction;
    const sending = hardwareWallets._sendMoney();
    const outcome = await sending.then(
      (transaction) => ({ transaction }),
      (error) => ({ error })
    );
    const expectedError = expect.any(Error);
    expect(outcome).toEqual(
      statusCode === 202
        ? { transaction: { id: transactionId } }
        : { error: expectedError }
    );
    expect(hardwareWallets.isTransactionPending).toBe(statusCode === 202);
    expect(hardwareWallets.sendMoneyRequest.isError).toBe(statusCode === 400);
    expect(received).toEqual([
      {
        method: 'POST',
        path: '/v2/proxy/transactions',
        headers: expect.objectContaining({
          'content-type': 'application/octet-stream',
          'content-length': String(signedTransaction.length / 2),
          accept: 'application/json',
        }),
        body: Buffer.from(signedTransaction, 'hex'),
      },
    ]);
    const next = await transactions.acquireWalletSendLock('wallet');
    next.release();
  }
);
