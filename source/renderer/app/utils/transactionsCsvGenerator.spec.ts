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
  beforeAll(() => addLocaleData(ja));

  beforeEach(() => {
    saveDialog.mockResolvedValue({ canceled: false, filePath });
  });

  test.each([
    {
      locale: 'en-US',
      messages: enTranslations,
      statuses: ['Pending', 'Confirmed', 'Failed'],
    },
    {
      locale: 'ja-JP',
      messages: jaTranslations,
      statuses: ['保留中', '承認済み', '失敗しました'],
    },
  ])(
    'exports transaction statuses in $locale',
    async ({ locale, messages, statuses }) => {
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
