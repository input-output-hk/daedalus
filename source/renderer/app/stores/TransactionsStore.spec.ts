import BigNumber from 'bignumber.js';
import TransactionsStore from './TransactionsStore';
import Request from './lib/LocalizedRequest';
import { WalletTransaction } from '../domains/WalletTransaction';
import type { SubmissionTransactionsData } from '../../../common/types/electron-store.types';
import type { TransactionReviewDisplay } from '../../../common/transactions/reviewDisplay';
import type { GetTransactionsResponse } from '../api/transactions/types';

const store = () =>
  new TransactionsStore(
    {
      ada: {
        deleteTransaction: jest.fn(),
        createExternalTransaction: jest.fn(),
        calculateTransactionFee: jest.fn(),
      },
    } as any,
    undefined as any,
    undefined as any
  );

const deferred = () => {
  let resolve = (): void => undefined;
  const promise = new Promise<void>((next) => {
    resolve = next;
  });
  return { promise, resolve };
};

describe('TransactionsStore wallet send coordinator', () => {
  it('serializes the same wallet while allowing different wallets', async () => {
    const transactions = store();
    const first = deferred();
    const order: string[] = [];
    const run1 = transactions.withWalletSendLock('wallet-a', async () => {
      order.push('a1-start');
      await first.promise;
      order.push('a1-end');
    });
    const run2 = transactions.withWalletSendLock('wallet-a', async () => {
      order.push('a2');
    });
    const other = transactions.withWalletSendLock('wallet-b', async () => {
      order.push('b');
    });
    await new Promise((resolve) => setTimeout(resolve, 0));
    expect(order).toEqual(['a1-start', 'b']);
    first.resolve();
    await Promise.all([run1, run2, other]);
    expect(order).toEqual(['a1-start', 'b', 'a1-end', 'a2']);
  });

  it('releases exactly once after failure or manual cancellation', async () => {
    const transactions = store();
    await expect(
      transactions.withWalletSendLock('wallet', async () => {
        throw new Error('failed');
      })
    ).rejects.toThrow('failed');
    await expect(
      transactions.withWalletSendLock('wallet', async () => 'next')
    ).resolves.toBe('next');

    const lease = await transactions.acquireWalletSendLock('wallet');
    const next = transactions.withWalletSendLock('wallet', async () => 'done');
    lease.release();
    lease.release();
    await expect(next).resolves.toBe('done');
  });
});

describe('TransactionsStore durable submissions', () => {
  const transactionId = 'ab'.repeat(32);
  const walletId = 'wallet-a';
  const display = ({
    entries: [
      {
        role: 'input',
        address: 'addr_test1',
        ownership: 'wallet',
        value: { coin: '4300000', assets: [] },
      },
      {
        role: 'output',
        address: 'addr_test1',
        ownership: 'wallet',
        value: { coin: '2000000', assets: [] },
      },
    ],
    walletChange: { coin: '-2300000', assets: [] },
    fee: '300000',
    mint: [],
    withdrawals: [],
    certificates: [],
  } as unknown) as TransactionReviewDisplay;

  const harness = (
    saved: Record<string, SubmissionTransactionsData>,
    getTransactions = jest.fn().mockResolvedValue({
      transactions: [],
      total: 0,
    })
  ) => {
    const confirmed = jest.fn();
    const deleteTransaction = jest.fn().mockResolvedValue(undefined);
    const recentRequest = new Request<GetTransactionsResponse>(getTransactions);
    const allRequest = new Request<GetTransactionsResponse>(getTransactions);
    const transactions = new TransactionsStore(
      {
        ada: {
          deleteTransaction,
          createExternalTransaction: jest.fn(),
          calculateTransactionFee: jest.fn(),
          getTransactions,
          getWithdrawals: jest.fn(),
        },
        localStorage: {
          getSubmissionTransactions: jest.fn(
            async (id: string) =>
              saved[id] || { version: 1 as const, records: [] }
          ),
          setSubmissionTransactions: jest.fn(
            async (id: string, data: SubmissionTransactionsData) => {
              saved[id] = data;
            }
          ),
        },
      } as never,
      {
        transactions: {
          transactionConfirmed: { trigger: confirmed },
        },
        router: { goToRoute: { trigger: jest.fn() } },
      } as never,
      undefined as never
    );
    const wallet = { id: walletId, isLegacy: false };
    transactions.configure({
      wallets: {
        active: wallet,
        all: [wallet],
        getWalletById: (id: string) => (id === walletId ? wallet : undefined),
        refreshWalletsData: jest.fn(),
        getWalletRoute: (id: string, page: string) => `/wallets/${id}/${page}`,
      },
      networkStatus: { isConnected: true },
      addresses: {
        _getAddressesAllRequest: () => ({
          result: [{ id: 'addr_test1' }],
        }),
      },
    } as never);
    transactions.transactionsRequests = [
      {
        walletId,
        isLegacy: false,
        recentRequest,
        allRequest,
        withdrawalsRequest: new Request(jest.fn().mockResolvedValue({})),
      },
    ];
    return {
      transactions,
      confirmed,
      deleteTransaction,
      recentRequest,
      allRequest,
    };
  };
  const backendTransaction = (
    overrides: Partial<ConstructorParameters<typeof WalletTransaction>[0]> = {}
  ) =>
    new WalletTransaction({
      id: transactionId,
      type: 'expend',
      title: 'Ada sent',
      amount: new BigNumber('-2.3'),
      fee: new BigNumber('0.3'),
      deposit: new BigNumber(0),
      date: new Date(),
      assets: [],
      description: '',
      addresses: {
        from: ['addr_test1'],
        to: ['addr_test1'],
        withdrawals: [],
      },
      state: 'in_ledger',
      confirmations: 1,
      slotNumber: 1,
      epochNumber: 1,
      metadata: null,
      amountIsKnown: true,
      hasCertificates: false,
      hasOnlyAda: true,
      ...overrides,
    });

  it('restores, merges and confirms one dismissed placeholder without guessing on errors', async () => {
    const saved: Record<string, SubmissionTransactionsData> = {};
    const first = harness(saved);
    await first.transactions.trackSubmission({
      walletId,
      transactionId,
      state: 'submission-unknown',
      display,
      payment: { address: 'addr_test1', amount: '2000000' },
    });
    expect(
      first.transactions.getTransaction(walletId, transactionId)
    ).toMatchObject({
      id: transactionId,
      state: 'submission-unknown',
      amountIsKnown: true,
      isSelfTransfer: true,
      localSubmission: true,
    });
    first.transactions.dismissReceipt(walletId, [transactionId]);
    const firstInternals = (first.transactions as unknown) as {
      _persistSubmissions: (id: string) => Promise<void>;
    };
    await firstInternals._persistSubmissions(walletId);

    const transportFailure = new Error('offline');
    const reloaded = harness(
      saved,
      jest.fn().mockRejectedValue(transportFailure)
    );
    const reloadedInternals = (reloaded.transactions as unknown) as {
      _ensureSubmissionsLoaded: (id: string) => Promise<void>;
      _reconcileSubmissions: (
        id: string,
        transactions: WalletTransaction[]
      ) => Promise<void>;
    };
    await reloadedInternals._ensureSubmissionsLoaded(walletId);
    await reloaded.transactions._refreshTransactionData();
    await expect(reloaded.recentRequest.promise).rejects.toBe(transportFailure);
    await expect(reloaded.allRequest.promise).rejects.toBe(transportFailure);
    expect(
      reloaded.transactions.getTransaction(walletId, transactionId)?.state
    ).toBe('submission-unknown');

    const confirmedTransaction = backendTransaction();
    const reconcile = reloadedInternals._reconcileSubmissions;
    await reconcile.call(reloaded.transactions, walletId, []);
    expect(
      reloaded.transactions.getTransaction(walletId, transactionId)?.state
    ).toBe('submission-unknown');
    await reconcile.call(reloaded.transactions, walletId, [
      confirmedTransaction,
    ]);
    expect(reloaded.confirmed).not.toHaveBeenCalled();

    await reloaded.transactions.trackSubmission({
      walletId,
      transactionId,
      state: 'submission-unknown',
    });
    await reconcile.call(reloaded.transactions, walletId, [
      confirmedTransaction,
    ]);
    await reconcile.call(reloaded.transactions, walletId, [
      backendTransaction({ state: 'pending' }),
    ]);
    reloaded.recentRequest.result = {
      transactions: [
        backendTransaction({ state: 'pending' }),
        confirmedTransaction,
      ],
      total: 2,
    };
    expect(reloaded.transactions.recent).toHaveLength(1);
    expect(reloaded.transactions.recent[0].state).toBe('in_ledger');
    expect(reloaded.confirmed).toHaveBeenCalledTimes(1);
  });

  it('keeps an unavailable amount out of the placeholder model', async () => {
    const saved: Record<string, SubmissionTransactionsData> = {};
    const { transactions } = harness(saved);
    await transactions.trackSubmission({
      walletId,
      transactionId,
      state: 'submission-unknown',
    });
    const unknown = transactions.getTransaction(walletId, transactionId);
    expect(unknown?.amountIsKnown).toBe(false);
    expect(unknown?.transferAmount).toBeUndefined();
  });

  it('stores outgoing net assets as filterable movement magnitudes', async () => {
    const saved: Record<string, SubmissionTransactionsData> = {};
    const { transactions } = harness(saved);
    const policyId = '12'.repeat(28);
    await transactions.trackSubmission({
      walletId,
      transactionId,
      state: 'pending',
      display: {
        ...display,
        entries: display.entries.map((entry) =>
          entry.role === 'output'
            ? { ...entry, address: 'addr_external', ownership: 'other' }
            : entry
        ),
        walletChange: {
          coin: '-2300000',
          assets: [
            {
              policyId,
              assetName: '',
              fingerprint: 'asset1placeholder',
              quantity: '-7',
            },
          ],
        },
      },
      payment: { address: 'addr_external', amount: '2000000' },
    });
    const asset = transactions.getTransaction(walletId, transactionId)
      ?.assets[0];
    expect(asset?.address).toBe('addr_external');
    expect(asset?.quantity.toString()).toBe('7');
  });
  it('keeps matched principal but requires complete ownership for self-transfer', async () => {
    const saved: Record<string, SubmissionTransactionsData> = {};
    const { transactions } = harness(saved);
    await transactions.trackSubmission({
      walletId,
      transactionId,
      state: 'pending',
      display: {
        ...display,
        entries: display.entries.map((entry) =>
          entry.role === 'input' ? { ...entry, ownership: 'unknown' } : entry
        ),
      },
      payment: { address: 'addr_test1', amount: '2000000' },
    });
    const tracked = transactions.getTransaction(walletId, transactionId);
    expect(tracked?.transferAmount?.toString()).toBe('2');
    expect(tracked?.isSelfTransfer).not.toBe(true);
  });

  it('identifies an internal payment even when rewards increase the wallet balance', async () => {
    const { transactions } = harness({});
    await transactions.trackSubmission({
      walletId,
      transactionId,
      state: 'pending',
      display: {
        ...display,
        entries: [
          ...display.entries.map((entry) => ({
            ...entry,
            value: {
              coin: entry.role === 'input' ? '764189118' : '5000000',
              assets: [],
            },
          })),
          {
            ...display.entries[1],
            value: { coin: '761569819', assets: [] },
          },
        ],
        walletChange: { coin: '2380701', assets: [] },
        fee: '174565',
        withdrawals: [
          { account: 'stake_test1', coin: '2555266', ownership: 'wallet' },
        ],
      },
      payment: { address: 'addr_test1', amount: '5000000' },
    });
    const tracked = transactions.getTransaction(walletId, transactionId);
    expect(tracked?.isSelfTransfer).toBe(true);
    expect(tracked?.transferAmount?.toString()).toBe('5');
    expect(tracked?.fee.toString()).toBe('0.174565');
    expect(tracked?.amount.toString()).toBe('2.380701');
  });

  it('requires nonempty positively-owned backend inputs and outputs', () => {
    const saved: Record<string, SubmissionTransactionsData> = {};
    const { transactions, recentRequest } = harness(saved);
    recentRequest.result = {
      transactions: [
        backendTransaction({
          id: 'cd'.repeat(32),
          addresses: {
            from: [null],
            to: ['addr_test1'],
            withdrawals: [],
          },
        }),
        backendTransaction(),
      ],
      total: 2,
    };
    expect(
      transactions.recent.find(({ id }) => id === 'cd'.repeat(32))
        ?.isSelfTransfer
    ).not.toBe(true);
    expect(
      transactions.recent.find(({ id }) => id === transactionId)?.isSelfTransfer
    ).toBe(true);
  });

  it('suppresses confirmation after an already-confirmed receipt is dismissed', async () => {
    const saved: Record<string, SubmissionTransactionsData> = {};
    const { transactions, confirmed, recentRequest } = harness(saved);
    await transactions.trackSubmission({
      walletId,
      transactionId,
      state: 'pending',
    });
    const backend = backendTransaction();
    recentRequest.result = { transactions: [backend], total: 1 };
    transactions.dismissReceipt(walletId, [transactionId]);
    const internals = (transactions as unknown) as {
      _reconcileSubmissions: (
        id: string,
        history: WalletTransaction[]
      ) => Promise<void>;
    };
    const reconcile = internals._reconcileSubmissions;
    await reconcile.call(transactions, walletId, [backend]);
    await reconcile.call(transactions, walletId, [
      backendTransaction({ state: 'pending' }),
    ]);
    await reconcile.call(transactions, walletId, [backend]);
    expect(confirmed).not.toHaveBeenCalled();
    expect(transactions.getTransaction(walletId, transactionId)?.state).toBe(
      'in_ledger'
    );
  });

  it('removes local submissions only after backend removal succeeds', async () => {
    const saved: Record<string, SubmissionTransactionsData> = {};
    const { transactions, deleteTransaction } = harness(saved);
    await transactions.trackSubmission({
      walletId,
      transactionId,
      state: 'expired',
    });
    deleteTransaction.mockRejectedValueOnce(new Error('backend unavailable'));
    await expect(
      transactions.deletePendingTransaction({ walletId, transactionId })
    ).rejects.toThrow('backend unavailable');
    expect(saved[walletId].records).toHaveLength(1);

    await transactions.deletePendingTransaction({ walletId, transactionId });
    expect(saved[walletId].records).toEqual([]);
    expect(
      transactions.getTransaction(walletId, transactionId)
    ).toBeUndefined();
  });
});
