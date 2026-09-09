import LocalStorageApi, {
  isSubmissionTransactionsData,
  parseSubmissionTransactionsData,
} from './localStorage';
import type { SubmissionTransactionRecord } from '../../../../common/types/electron-store.types';

const record: SubmissionTransactionRecord = {
  transactionId: 'ab'.repeat(32),
  state: 'submission-unknown',
  createdAt: '2026-01-01T00:00:00.000Z',
  amount: '0',
  fee: '0.2',
  amountIsKnown: false,
  hasCertificates: false,
  type: 'expend',
  title: 'Ada sent',
  assets: [],
  dismissed: false,
  notified: false,
};

describe('submission persistence validation', () => {
  it('accepts the review asset bound and rejects private or oversized data', () => {
    const assets = Array.from({ length: 1000 }, (_, index) => ({
      policyId: index.toString(16).padStart(56, '0'),
      assetName: '',
      quantity: '1',
    }));
    const large = { ...record, assets };
    expect(
      parseSubmissionTransactionsData({ version: 1, records: [large] }).records
    ).toEqual([large]);
    expect(
      parseSubmissionTransactionsData({
        version: 1,
        records: [{ ...large, assets: [...assets, assets[0]] }],
      }).records
    ).toEqual([]);
    expect(
      parseSubmissionTransactionsData({
        version: 1,
        records: [{ ...record, signedTransaction: 'private-cbor' }],
      }).records
    ).toEqual([]);
  });

  it('rejects invalid writes instead of silently storing a sanitized value', async () => {
    const set = jest.spyOn(LocalStorageApi, 'set').mockResolvedValue();
    const storage = new LocalStorageApi();
    const invalid = {
      version: 1,
      records: [{ ...record, signedTransaction: 'private-cbor' }],
    };
    expect(isSubmissionTransactionsData(invalid)).toBe(false);
    await expect(
      storage.setSubmissionTransactions(
        'wallet-a',
        (invalid as unknown) as { version: 1; records: typeof record[] }
      )
    ).rejects.toThrow('Invalid submission transactions data');
    expect(set).not.toHaveBeenCalled();
  });
});
