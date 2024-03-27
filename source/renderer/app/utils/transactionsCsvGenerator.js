'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const path_1 = __importDefault(require('path'));
const react_intl_1 = require('react-intl');
const lodash_1 = require('lodash');
const files_1 = require('../../../common/utils/files');
const show_file_dialog_channels_1 = require('../ipc/show-file-dialog-channels');
const WalletTransaction_1 = require('../domains/WalletTransaction');
const csvGenerator_1 = require('./csvGenerator');
const formatters_1 = require('./formatters');
const walletsConfig_1 = require('../config/walletsConfig');
const assets_1 = require('./assets');
const messages = (0, react_intl_1.defineMessages)({
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
const transactionsCsvGenerator = async ({
  desktopDirectoryPath,
  intl,
  transactions,
  walletName,
  getAsset,
  isInternalAddress,
}) => {
  const prefix = `${intl.formatMessage(messages.filenamePrefix)}-${walletName}`;
  const fileName = (0, files_1.generateFileNameWithTimestamp)({
    prefix,
    extension: 'csv',
    isUTC: true,
  });
  const defaultPath = path_1.default.join(desktopDirectoryPath, fileName);
  const params = {
    defaultPath,
    filters: [
      {
        extensions: ['csv'],
      },
    ],
  };
  const {
    filePath,
  } = await show_file_dialog_channels_1.showSaveDialogChannel.send(params);
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
  if (walletsConfig_1.WALLET_ASSETS_ENABLED) {
    columns.splice(6, 0, intl.formatMessage(messages.columnTokens));
  }
  const fileContent = [columns];
  transactions.forEach(
    ({ id, type, amount, deposit, fee, date, addresses, state, assets }) => {
      const valueType =
        type === WalletTransaction_1.TransactionTypes.EXPEND
          ? intl.formatMessage(messages.valueTypeSent)
          : intl.formatMessage(messages.valueTypeReceived);
      const valueTotal = (0, formatters_1.formattedWalletAmount)(amount, false);
      let valueSentAmount = '';
      let valueDepositAmount = '';
      let valueTransactionFee = '';
      if (type === WalletTransaction_1.TransactionTypes.EXPEND) {
        const amountWithoutFees = -amount.minus(-fee);
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
        valueSentAmount = (0, formatters_1.formattedWalletAmount)(
          amountWithoutFees,
          false
        );
        valueDepositAmount = (0, formatters_1.formattedWalletAmount)(
          deposit,
          false
        );
        valueTransactionFee = (0, formatters_1.formattedWalletAmount)(
          fee,
          false
        );
      }
      const valueTokens = (0, assets_1.filterAssets)(
        assets,
        type,
        isInternalAddress
      )
        .map(({ policyId, assetName, quantity }) => {
          const { fingerprint, metadata } = getAsset(policyId, assetName);
          const formattedAmount = (0, formatters_1.formattedTokenWalletAmount)(
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
      const valueAddressesFrom = !(0, lodash_1.includes)(addresses.from, null)
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
      if (walletsConfig_1.WALLET_ASSETS_ENABLED) {
        txValues.splice(6, 0, valueTokens);
      }
      fileContent.push(txValues);
    }
  );
  await (0, csvGenerator_1.downloadCsv)({
    filePath,
    fileContent,
  });
  return true;
};
exports.default = transactionsCsvGenerator;
//# sourceMappingURL=transactionsCsvGenerator.js.map
