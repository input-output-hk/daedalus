// @flow
import path from 'path';
import moment from 'moment';
import BigNumber from 'bignumber.js';
import { intlShape, defineMessages } from 'react-intl';
import { generateFileNameWithTimestamp } from '../../../common/utils/files';
import { showSaveDialogChannel } from '../ipc/show-file-dialog-channels';
import { WalletTransaction } from '../domains/WalletTransaction';
import { downloadCsv } from './csvGenerator';
import { formattedWalletAmount } from './formatters';

const messages = defineMessages({
  columnID: {
    id: 'wallet.transactions.csv.column.id',
    defaultMessage: '!!!ID',
    description: 'Transactions CSV column - ID',
  },
  columnType: {
    id: 'wallet.transactions.csv.column.type',
    defaultMessage: '!!!Type',
    description: 'Transactions CSV column - Type',
  },
  columnAmount: {
    id: 'wallet.transactions.csv.column.amount',
    defaultMessage: '!!!Amount',
    description: 'Transactions CSV column - Amount',
  },
  columnDateTime: {
    id: 'wallet.transactions.csv.column.dateTime',
    defaultMessage: '!!!Date & time',
    description: 'Transactions CSV column - DateTime',
  },
  columnStatus: {
    id: 'wallet.transactions.csv.column.status',
    defaultMessage: '!!!Status',
    description: 'Transactions CSV column - Status',
  },
  columnAddressesFrom: {
    id: 'wallet.transactions.csv.column.addressesFrom',
    defaultMessage: '!!!Addresses from',
    description: 'Transactions CSV column - AddressesFrom',
  },
  columnAddressesTo: {
    id: 'wallet.transactions.csv.column.addressesTo',
    defaultMessage: '!!!Addresses to',
    description: 'Transactions CSV column - AddressesTo',
  },
  columnWithdrawals: {
    id: 'wallet.transactions.csv.column.withdrawals',
    defaultMessage: '!!!Withdrawals',
    description: 'Transactions CSV column - Withdrawals',
  },
  valueTypeExpend: {
    id: 'wallet.transactions.csv.value.type.expend',
    defaultMessage: '!!!Expend',
    description: 'Transactions CSV value - Type Expend',
  },
  valueTypeIncome: {
    id: 'wallet.transactions.csv.value.type.income',
    defaultMessage: '!!!Income',
    description: 'Transactions CSV value - Type Income',
  },
  valueAmount: {
    id: 'wallet.transactions.csv.value.amount',
    defaultMessage: '!!!{amount} ADA',
    description: 'Transactions CSV value - Amount',
  },
  valueStatusConfirmed: {
    id: 'wallet.transactions.csv.value.statusConfirmed',
    defaultMessage: '!!!Confirmed',
    description: 'Transactions CSV value - Status Confirmed',
  },
  valueStatusPending: {
    id: 'wallet.transactions.csv.value.statusPending',
    defaultMessage: '!!!Pending',
    description: 'Transactions CSV value - Status Pending',
  },
});

type Params = {
  desktopDirectoryPath: string,
  intl: intlShape,
  transactions: Array<WalletTransaction>,
};

const transactionsCsvGenerator = async ({
  desktopDirectoryPath,
  intl,
  transactions,
}: Params) => {
  const fileName = generateFileNameWithTimestamp({
    prefix: 'transactions',
    extension: 'csv',
    isUTC: true,
  });
  const defaultPath = path.join(desktopDirectoryPath, fileName);
  const params = {
    defaultPath,
    filters: [
      {
        extensions: ['csv'],
      },
    ],
  };
  const { filePath } = await showSaveDialogChannel.send(params);

  // if cancel button is clicked or path is empty
  if (!filePath) return;

  const columns = [
    intl.formatMessage(messages.columnID),
    intl.formatMessage(messages.columnType),
    intl.formatMessage(messages.columnAmount),
    intl.formatMessage(messages.columnDateTime),
    intl.formatMessage(messages.columnStatus),
    intl.formatMessage(messages.columnAddressesFrom),
    intl.formatMessage(messages.columnAddressesTo),
    intl.formatMessage(messages.columnWithdrawals),
  ];

  const fileContent = [columns];

  transactions.forEach(
    ({ id, type, amount, date, addresses, state }: WalletTransaction) => {
      const valueType =
        type === 'expend'
          ? intl.formatMessage(messages.valueTypeExpend)
          : intl.formatMessage(messages.valueTypeIncome);
      const amountNumber = formattedWalletAmount(new BigNumber(amount), false);
      const valueAmount = intl.formatMessage(messages.valueAmount, {
        amount: amountNumber,
      });
      const valueDateTime = `${moment(date)
        .utc()
        .format('YYYY-MM-DDTHHmmss.0SSS')}Z`;
      const valueStatus =
        state === 'pending'
          ? intl.formatMessage(messages.valueStatusPending)
          : intl.formatMessage(messages.valueStatusConfirmed);
      const valueAddressesFrom = addresses.from.join(', ');
      const valueAddressesTo = addresses.to.join(', ');
      const valueWithdrawals = addresses.withdrawals.join(', ');
      const txValues = [
        id,
        valueType,
        valueAmount,
        `${valueDateTime}`,
        valueStatus,
        valueAddressesFrom,
        valueAddressesTo,
        valueWithdrawals,
      ];
      fileContent.push(txValues);
    }
  );

  downloadCsv({ filePath, fileContent });
};

export default transactionsCsvGenerator;
