import path from 'path';
import { intlShape, defineMessages } from 'react-intl';
import { includes } from 'lodash';
import { generateFileNameWithTimestamp } from '../../../common/utils/files';
import { showSaveDialogChannel } from '../ipc/show-file-dialog-channels';
import {
  WalletTransaction,
  TransactionTypes,
} from '../domains/WalletTransaction';
import { downloadCsv } from './csvGenerator';
import {
  formattedWalletAmount,
  formattedTokenWalletAmount,
} from './formatters';
import { WALLET_ASSETS_ENABLED } from '../config/walletsConfig';
import { filterAssets } from './assets';

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
  columnTotal: {
    id: 'wallet.transactions.csv.column.amount.total',
    defaultMessage: '!!!TOTAL (ADA)',
    description: 'Transactions CSV column - TOTAL',
  },
  columnSentAmount: {
    id: 'wallet.transactions.csv.column.amount.sent',
    defaultMessage: '!!!Sent amount (ADA)',
    description: 'Transactions CSV column - Sent amount',
  },
  columnDepositAmount: {
    id: 'wallet.transactions.csv.column.amount.deposit',
    defaultMessage: '!!!Deposit amount (ADA)',
    description: 'Transactions CSV column - Deposit amount',
  },
  columnFee: {
    id: 'wallet.transactions.csv.column.amount.fee',
    defaultMessage: '!!!Fee (ADA)',
    description: 'Transactions CSV column - Fee',
  },
  columnTokens: {
    id: 'wallet.transactions.csv.column.tokens',
    defaultMessage: '!!!Tokens (unformatted amount)',
    description: 'Transactions CSV column - Tokens',
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
  desktopDirectoryPath: string;
  intl: intlShape;
  transactions: Array<WalletTransaction>;
  walletName: string;
  getAsset: (...args: Array<any>) => any;
  isInternalAddress: (...args: Array<any>) => any;
};

const transactionsCsvGenerator = async ({
  desktopDirectoryPath,
  intl,
  transactions,
  walletName,
  getAsset,
  isInternalAddress,
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
    intl.formatMessage(messages.columnTotal),
    intl.formatMessage(messages.columnSentAmount),
    intl.formatMessage(messages.columnDepositAmount),
    intl.formatMessage(messages.columnFee),
    intl.formatMessage(messages.columnDateTime),
    intl.formatMessage(messages.columnStatus),
    intl.formatMessage(messages.columnAddressesFrom),
    intl.formatMessage(messages.columnAddressesTo),
    intl.formatMessage(messages.columnWithdrawals),
  ];

  if (WALLET_ASSETS_ENABLED) {
    columns.splice(6, 0, intl.formatMessage(messages.columnTokens));
  }

  const fileContent = [columns];
  transactions.forEach(
    ({
      id,
      type,
      amount,
      deposit,
      fee,
      date,
      addresses,
      state,
      assets,
    }: WalletTransaction) => {
      const valueType =
        type === TransactionTypes.EXPEND
          ? intl.formatMessage(messages.valueTypeSent)
          : intl.formatMessage(messages.valueTypeReceived);
      const valueTotal = formattedWalletAmount(amount, false);
      let valueSentAmount = '';
      let valueDepositAmount = '';
      let valueTransactionFee = '';

      if (type === TransactionTypes.EXPEND) {
        const amountWithoutFees = -amount.minus(-fee);
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
        valueSentAmount = formattedWalletAmount(amountWithoutFees, false);
        valueDepositAmount = formattedWalletAmount(deposit, false);
        valueTransactionFee = formattedWalletAmount(fee, false);
      }

      const valueTokens = filterAssets(assets, type, isInternalAddress)
        .map(({ policyId, assetName, quantity }) => {
          const { fingerprint, metadata } = getAsset(policyId, assetName);
          const formattedAmount = formattedTokenWalletAmount(
            quantity,
            metadata,
            0
          );
          return `${formattedAmount} (${fingerprint})`;
        })
        .join(', ');
      const valueDateTime = date ? date.toISOString() : '';
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
        valueTotal,
        valueSentAmount,
        valueDepositAmount,
        valueTransactionFee,
        `${valueDateTime}`,
        valueStatus,
        valueAddressesFrom,
        valueAddressesTo,
        valueWithdrawals,
      ];

      if (WALLET_ASSETS_ENABLED) {
        txValues.splice(6, 0, valueTokens);
      }

      fileContent.push(txValues);
    }
  );
  await downloadCsv({
    filePath,
    fileContent,
  });
  return true;
};

export default transactionsCsvGenerator;
