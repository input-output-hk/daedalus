import TransactionsStore from './TransactionsStore';

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
