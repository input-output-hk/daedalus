// @flow
import path from 'path';
import { defineMessages } from 'react-intl';
import { generateFileNameWithTimestamp } from '../../../common/utils/files';
import { showSaveDialogChannel } from '../ipc/show-file-dialog-channels';
import { WalletTransaction } from '../domains/WalletTransaction';
import { downloadCsv } from './csvGenerator';

const messages = defineMessages({
  creationDate: {
    id: 'wallet.transactions.csv.incoming',
    defaultMessage: '!!!PDF creation date {date} {time}',
    description: 'PDF title',
  },
});

type Params = {
  desktopDirectoryPath: string,
  transactions: Array<>,
};

const transactionsCsvGenerator = async ({
  desktopDirectoryPath,
  transactions,
}) => {
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

  const fileContent = transactions.reduce(
    (list, tx: WalletTransaction, index) => {
      if (index === 0) {
        list.push(Object.keys(tx));
      }
      list.push(Object.values(tx));
      return list;
    },
    []
  );

  downloadCsv({ filePath, fileContent });
};

export default transactionsCsvGenerator;

// export const addressPDFGenerator = async ({
//   address,
//   note,
//   filePath,
//   currentLocale,
//   currentDateFormat,
//   currentTimeFormat,
//   network,
//   isMainnet,
//   intl,
// }: Params) => {
//   const date = moment().format(currentDateFormat);
//   const time = moment().format(currentTimeFormat);
//   await generateAddressPDFChannel.send({
//     address,
//     filePath,
//     note,
//     currentLocale,
//     isMainnet,
//     networkLabel: intl.formatMessage(messages.networkLabel),
//     networkName: intl.formatMessage(globalMessages[`network_${network}`]),
//     creationDate: intl.formatMessage(messages.creationDate, { date, time }),
//     noteLabel: intl.formatMessage(messages.noteLabel),
//     title: intl.formatMessage(messages.title),
//     author: intl.formatMessage(messages.author),
//   });
// };
