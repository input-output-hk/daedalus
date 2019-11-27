// @flow
import moment from 'moment';
import { defineMessages } from 'react-intl';
import { generateAddressPDFChannel } from '../ipc/generateAddressPDFChannel';
import type { networkId } from '../../../common/types/environment.types';

const messages = defineMessages({
  creationDate: {
    id: 'wallet.receive.pdf.creationDate',
    defaultMessage: '!!!PDF creation date {date} {time}',
    description: 'PDF title',
  },
  noteLabel: {
    id: 'wallet.receive.pdf.noteLabel',
    defaultMessage: '!!!Note:',
    description: 'PDF title',
  },
  title: {
    id: 'wallet.receive.pdf.title',
    defaultMessage: '!!!Daedalus address',
    description: 'PDF title',
  },
  author: {
    id: 'wallet.receive.pdf.author',
    defaultMessage: '!!!Daedalus wallet',
    description: 'PDF author',
  },
  networkLabel: {
    id: 'wallet.receive.pdf.networkLabel',
    defaultMessage: '!!!Cardano Network:',
    description: 'PDF networkLabel',
  },
  networkName_mainnet: {
    id: 'wallet.receive.pdf.networkName_mainnet',
    defaultMessage: '!!!mainnet',
    description: 'PDF networkName mainnet',
  },
  networkName_staging: {
    id: 'wallet.receive.pdf.networkName_staging',
    defaultMessage: '!!!Staging',
    description: 'PDF networkName staging',
  },
  networkName_testnet: {
    id: 'wallet.receive.pdf.networkName_testnet',
    defaultMessage: '!!!Testnet',
    description: 'PDF networkName testnet',
  },
  networkName_development: {
    id: 'wallet.receive.pdf.networkName_development',
    defaultMessage: '!!!Development',
    description: 'PDF networkName development',
  },
  networkName_itn_balance_check: {
    id: 'wallet.receive.pdf.networkName_itn_balance_check',
    defaultMessage: '!!!Incentivized Testnet',
    description: 'PDF networkName itn_balance_check',
  },
});

type Params = {
  address: string,
  note: string,
  filePath: string,
  currentLocale: string,
  currentDateFormat: string,
  currentTimeFormat: string,
  network: networkId,
  isMainnet: boolean,
  intl: Object,
};

export const addressPDFGenerator = async ({
  address,
  note,
  filePath,
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  network,
  isMainnet,
  intl,
}: Params) => {
  const date = moment().format(currentDateFormat);
  const time = moment().format(currentTimeFormat);
  await generateAddressPDFChannel.send({
    address,
    filePath,
    note,
    currentLocale,
    isMainnet,
    networkLabel: intl.formatMessage(messages.networkLabel),
    networkName: intl.formatMessage(messages[`networkName_${network}`]),
    creationDate: intl.formatMessage(messages.creationDate, { date, time }),
    noteLabel: intl.formatMessage(messages.noteLabel),
    title: intl.formatMessage(messages.title),
    author: intl.formatMessage(messages.author),
  });
};
