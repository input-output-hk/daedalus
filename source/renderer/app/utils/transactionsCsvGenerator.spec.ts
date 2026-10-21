import BigNumber from 'bignumber.js';
import { addLocaleData, IntlProvider } from 'react-intl';
import ja from 'react-intl/locale-data/ja';
import type { TransactionState } from '../api/transactions/types';
import { WalletTransaction } from '../domains/WalletTransaction';
import enTranslations from '../i18n/locales/en-US.json';
import jaTranslations from '../i18n/locales/ja-JP.json';
import { generateCsvChannel } from '../ipc/generateCsvChannel';
import { showSaveDialogChannel } from '../ipc/show-file-dialog-channels';
import transactionsCsvGenerator from './transactionsCsvGenerator';

jest.mock('../ipc/generateCsvChannel', () => ({
  generateCsvChannel: { send: jest.fn() },
}));
jest.mock('../ipc/show-file-dialog-channels', () => ({
  showSaveDialogChannel: { send: jest.fn() },
}));

const saveDialog = showSaveDialogChannel.send as jest.MockedFunction<
  typeof showSaveDialogChannel.send
>;
const generateCsv = generateCsvChannel.send as jest.MockedFunction<
  typeof generateCsvChannel.send
>;
const filePath = '/desktop/transactions.csv';
const date = new Date('2026-01-02T03:04:05.000Z');
const createTransaction = (
  state: TransactionState,
  transactionDate: Date | null
) =>
  new WalletTransaction({
    id: state,
    type: 'expend',
    title: '',
    amount: new BigNumber('-1.2'),
    fee: new BigNumber('0.2'),
    deposit: new BigNumber(0),
    date: transactionDate,
    assets: [],
    description: '',
    addresses: {
      from: ['source-address'],
      to: ['target-address'],
      withdrawals: [],
    },
    state,
    confirmations: 0,
    slotNumber: null,
    epochNumber: null,
    metadata: null,
  });
const params = {
  desktopDirectoryPath: '/desktop',
  walletName: 'Test wallet',
  transactions: [
    createTransaction('pending', date),
    createTransaction('in_ledger', date),
    createTransaction('expired', null),
  ],
  getAsset: jest.fn(),
  isInternalAddress: jest.fn(),
};

describe('transactionsCsvGenerator', () => {
  const originalCapability = global.cip30Enabled;
  afterEach(() => {
    global.cip30Enabled = originalCapability;
  });
  beforeAll(() => addLocaleData(ja));

  beforeEach(() => {
    global.cip30Enabled = true;
    saveDialog.mockResolvedValue({ canceled: false, filePath });
  });

  test.each([
    {
      locale: 'en-US',
      messages: enTranslations,
      cip30Enabled: true,
      statuses: ['Pending', 'Confirmed', 'Expired'],
    },
    {
      locale: 'ja-JP',
      messages: jaTranslations,
      cip30Enabled: true,
      statuses: ['保留中', '承認済み', '有効期限切れ'],
    },
    {
      locale: 'en-US',
      messages: enTranslations,
      cip30Enabled: false,
      statuses: ['Pending', 'Confirmed', 'Failed'],
    },
    {
      locale: 'ja-JP',
      messages: jaTranslations,
      cip30Enabled: false,
      statuses: ['保留中', '承認済み', '失敗'],
    },
  ])(
    'exports transaction statuses in $locale',
    async ({ locale, messages, statuses, cip30Enabled }) => {
      global.cip30Enabled = cip30Enabled;
      const { intl } = new IntlProvider(
        { locale, messages },
        {}
      ).getChildContext();

      await expect(transactionsCsvGenerator({ ...params, intl })).resolves.toBe(
        true
      );

      expect(generateCsv).toHaveBeenCalledTimes(1);
      const [request] = generateCsv.mock.calls[0];
      expect(request.filePath).toBe(filePath);
      const [columns, ...rows] = request.fileContent;
      const statusColumn = columns.indexOf(
        messages['wallet.transactions.csv.column.status']
      );
      const dateColumn = columns.indexOf(
        messages['wallet.transactions.csv.column.dateTime']
      );

      expect(rows.map((row) => row[0])).toEqual([
        'pending',
        'in_ledger',
        'expired',
      ]);
      expect(rows.map((row) => row[statusColumn])).toEqual(statuses);
      expect(rows.map((row) => row[dateColumn])).toEqual([
        date.toISOString(),
        date.toISOString(),
        '',
      ]);
    }
  );

  it('exports upstream backend amounts, fees, deposits and external assets without receipt overlays', async () => {
    global.cip30Enabled = false;
    const tx = createTransaction('expired', date);
    tx.amount = new BigNumber('-3.2');
    tx.deposit = new BigNumber('2');
    tx.transferAmount = new BigNumber('99');
    tx.amountIsKnown = false;
    tx.assets = [
      {
        policyId: 'policy',
        assetName: '41',
        uniqueId: 'policy41',
        quantity: new BigNumber(7),
        address: 'target-address',
      },
      {
        policyId: 'policy',
        assetName: '41',
        uniqueId: 'policy41',
        quantity: new BigNumber(9),
        address: 'change-address',
      },
    ];
    const { intl } = new IntlProvider(
      { locale: 'en-US', messages: enTranslations },
      {}
    ).getChildContext();
    await transactionsCsvGenerator({
      ...params,
      intl,
      transactions: [tx],
      getAsset: () => ({ fingerprint: 'asset1token', decimals: 0 }),
      isInternalAddress: (address: string) => address === 'change-address',
    });
    const [columns, row] = generateCsv.mock.calls[0][0].fileContent;
    const value = (key: keyof typeof enTranslations) =>
      row[columns.indexOf(enTranslations[key])];
    expect(value('wallet.transactions.csv.column.amount.total')).toBe(
      '-3.200000'
    );
    expect(value('wallet.transactions.csv.column.amount.sent')).toBe(
      '3.000000'
    );
    expect(value('wallet.transactions.csv.column.amount.deposit')).toBe(
      '2.000000'
    );
    expect(value('wallet.transactions.csv.column.amount.fee')).toBe('0.200000');
    expect(row.join(',')).toContain('7 (asset1token)');
    expect(row.join(',')).not.toContain('9 (asset1token)');
  });

  it('does not generate a CSV when the save dialog is canceled', async () => {
    saveDialog.mockResolvedValue({ canceled: true });
    const { intl } = new IntlProvider(
      { locale: 'en-US', messages: enTranslations },
      {}
    ).getChildContext();

    await expect(transactionsCsvGenerator({ ...params, intl })).resolves.toBe(
      false
    );
    expect(generateCsv).not.toHaveBeenCalled();
  });
});
