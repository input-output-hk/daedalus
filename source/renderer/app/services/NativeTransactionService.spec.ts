/** @jest-environment node */
import NativeTransactionService from './NativeTransactionService';

const prepared = {
  walletId: 'wallet',
  network: {
    networkId: 0 as const,
    networkMagic: 42,
    genesisHash: '11'.repeat(32),
  },
  action: 'payment' as const,
  authorization: 'software' as const,
  items: [],
  context: { walletName: 'Savings', networkName: 'Preview' },
};

test('releases the wallet lock while approval is pending and reacquires for execution', async () => {
  const order: string[] = [];
  const withLock = async <T>(_walletId: string, work: () => Promise<T>) => {
    order.push('lock');
    const result = await work();
    order.push('unlock');
    return result;
  };
  const approval = {
    request: async (_prepared: unknown, execute: Function) => {
      order.push('approval');
      return execute('', new AbortController().signal, async () => undefined);
    },
  };
  const service = new NativeTransactionService(withLock, approval as any);
  await expect(
    service.run({
      walletId: 'wallet',
      ownerSignal: new AbortController().signal,
      prepare: async () => {
        order.push('prepare');
        return prepared;
      },
      execute: async () => {
        order.push('execute');
        return { status: 'submitted', transactionIds: ['transaction'] };
      },
    })
  ).resolves.toEqual({ status: 'submitted', transactionIds: ['transaction'] });
  expect(order).toEqual([
    'lock',
    'prepare',
    'unlock',
    'approval',
    'lock',
    'execute',
    'unlock',
  ]);
});
