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
  const originalCapability = global.cip30Enabled;
  beforeEach(() => {
    global.cip30Enabled = true;
  });
  afterEach(() => {
    global.cip30Enabled = originalCapability;
  });

  it('preserves upstream pending, confirmed and expired amounts and assets', () => {
    global.cip30Enabled = false;
    expect(_conditionToTxState('pending')).toBe('pending');
    expect(_conditionToTxState('in_ledger')).toBe('in_ledger');
    expect(_conditionToTxState('expired')).toBe('expired');
    expect(_conditionToTxState('unexpected')).toBe('in_ledger');
    const converted = _createTransactionFromServerData(
      transaction({
        status: 'expired',
        outputs: [
          {
            address: 'target-address',
            amount: { quantity: 1000000, unit: WalletUnits.LOVELACE },
            assets: [
              { policy_id: 'cd'.repeat(28), asset_name: '41', quantity: 7 },
            ],
          },
        ],
      })
    );
    expect(converted.state).toBe('expired');
    expect(converted.amount.toString()).toBe('-1');
    expect(converted.fee.toString()).toBe('0.2');
    expect(converted.assets[0]).toMatchObject({
      policyId: 'cd'.repeat(28),
      assetName: '41',
      address: 'target-address',
    });
    expect(converted.assets[0].quantity.toString()).toBe('7');
  });

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
