import { _conditionToTxState, _createTransactionFromServerData } from './api';
import type { Transaction } from './transactions/types';
import { WalletUnits } from '../domains/Wallet';

jest.mock('./utils/request', () => ({ request: jest.fn() }));

const transaction = (overrides: Partial<Transaction> = {}): Transaction => ({
  id: 'ab'.repeat(32),
  amount: { quantity: 1000000, unit: WalletUnits.LOVELACE },
  fee: { quantity: 200000, unit: WalletUnits.LOVELACE },
  deposit_taken: { quantity: 0, unit: WalletUnits.LOVELACE },
  deposit_returned: { quantity: 0, unit: WalletUnits.LOVELACE },
  inserted_at: {
    time: new Date('2026-01-01T00:00:00.000Z'),
    block: { slot_number: 1, epoch_number: 2 },
  },
  depth: { quantity: 3, unit: 'block' },
  direction: 'outgoing',
  inputs: [
    {
      id: 'cd'.repeat(32),
      index: 0,
      address: 'addr_test1',
      amount: { quantity: 1200000, unit: WalletUnits.LOVELACE },
    },
  ],
  outputs: [
    {
      address: 'addr_test1',
      amount: { quantity: 1000000, unit: WalletUnits.LOVELACE },
    },
  ],
  withdrawals: [],
  status: 'in_ledger',
  ...overrides,
});

describe('transaction history conversion', () => {
  it('keeps backend states honest and carries certificate/pure-ADA evidence', () => {
    expect(_conditionToTxState('expired')).toBe('expired');
    expect(_conditionToTxState('failed')).toBe('failed');
    expect(_conditionToTxState('unexpected')).toBe('submission-unknown');

    const plain = _createTransactionFromServerData(transaction());
    expect(plain).toMatchObject({
      amountIsKnown: true,
      hasCertificates: false,
      hasOnlyAda: true,
    });

    const unresolvedShape = _createTransactionFromServerData(
      transaction({ inputs: [] })
    );
    expect(unresolvedShape.hasOnlyAda).toBe(false);

    const certified = _createTransactionFromServerData(
      transaction({
        certificates: [
          {
            certificate_type: 'cast_vote',
            reward_account_path: [],
          },
        ],
      })
    );
    expect(certified.hasCertificates).toBe(true);
    expect(certified.type).toBe('vote');
  });
});
