// @flow
import path from 'path';
import moment from 'moment';
import BigNumber from 'bignumber.js';
import { intlShape, defineMessages } from 'react-intl';
import { includes } from 'lodash';
import { generateFileNameWithTimestamp } from '../../../common/utils/files';
import { showSaveDialogChannel } from '../ipc/show-file-dialog-channels';
import {
  WalletTransaction,
  TransactionTypes,
} from '../domains/WalletTransaction';
import { downloadCsv } from './csvGenerator';
import { formattedWalletAmount } from './formatters';
import globalMessages from '../i18n/global-messages';

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
  columnAsset: {
    id: 'wallet.transactions.csv.column.asset',
    defaultMessage: '!!!Asset',
    description: 'Transactions CSV column - Amount currency',
  },
  columnFee: {
    id: 'wallet.transactions.csv.column.fee',
    defaultMessage: '!!!Fee (ADA)',
    description: 'Transactions CSV column - Fee',
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
  valueTypeSent: {
    id: 'wallet.transactions.csv.value.type.sent',
    defaultMessage: '!!!Sent',
    description: 'Transactions CSV value - Type Sent',
  },
  valueTypeReceived: {
    id: 'wallet.transactions.csv.value.type.received',
    defaultMessage: '!!!Received',
    description: 'Transactions CSV value - Type Received',
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
  filenamePrefix: {
    id: 'wallet.transactions.csv.filenamePrefix',
    defaultMessage: '!!!Transactions',
    description: 'Transactions CSV "Transactions" filename',
  },
});

type Params = {
  desktopDirectoryPath: string,
  intl: intlShape,
  transactions: Array<WalletTransaction>,
  walletName: string,
};

const transactionsCsvGenerator = async ({
  desktopDirectoryPath,
  intl,
  transactions,
  walletName,
}: Params): Promise<boolean> => {
  const prefix = `${intl.formatMessage(messages.filenamePrefix)}-${walletName}`;
  const fileName = generateFileNameWithTimestamp({
    prefix,
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
  if (!filePath) return false;

  const columns = [
    intl.formatMessage(messages.columnID),
    intl.formatMessage(messages.columnType),
    intl.formatMessage(messages.columnAmount),
    intl.formatMessage(messages.columnAsset),
    intl.formatMessage(messages.columnFee),
    intl.formatMessage(messages.columnDateTime),
    intl.formatMessage(messages.columnStatus),
    intl.formatMessage(messages.columnAddressesFrom),
    intl.formatMessage(messages.columnAddressesTo),
    intl.formatMessage(messages.columnWithdrawals),
  ];

  const fileContent = [columns];

  transactions.forEach(
    ({ id, type, amount, fee, date, addresses, state }: WalletTransaction) => {
      const valueType =
        type === 'expend'
          ? intl.formatMessage(messages.valueTypeSent)
          : intl.formatMessage(messages.valueTypeReceived);
      const valueAmount = formattedWalletAmount(amount, false);
      const valueAsset = intl.formatMessage(globalMessages.unitAda);
      const hasFee = type === TransactionTypes.EXPEND && !fee.isZero();
      const valueTransactionFee = hasFee
        ? formattedWalletAmount(fee, false)
        : null;
      const valueDateTime = `${moment(date)
        .utc()
        .format('YYYY-MM-DDTHHmmss.0SSS')}Z`;
      const valueStatus =
        state === 'pending'
          ? intl.formatMessage(messages.valueStatusPending)
          : intl.formatMessage(messages.valueStatusConfirmed);
      const valueAddressesFrom = !includes(addresses.from, null)
        ? addresses.from.join(', ')
        : ' ';
      const valueAddressesTo = addresses.to.join(', ');
      const valueWithdrawals = addresses.withdrawals.join(', ');
      const txValues = [
        id,
        valueType,
        valueAmount,
        valueAsset,
        valueTransactionFee,
        `${valueDateTime}`,
        valueStatus,
        valueAddressesFrom,
        valueAddressesTo,
        valueWithdrawals,
      ];
      fileContent.push(txValues);
    }
  );

  await downloadCsv({ filePath, fileContent });
  return true;
};

export default transactionsCsvGenerator;
