import moment from 'moment';
import { defineMessages } from 'react-intl';
import { generateAddressPDFChannel } from '../ipc/generateAddressPDFChannel';
import type { Network } from '../../../common/types/environment.types';
import globalMessages from '../i18n/global-messages';

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
    defaultMessage: '!!!Daedalus Cardano ada address',
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
});
type Params = {
  address: string;
  note: string;
  filePath: string;
  currentLocale: string;
  currentDateFormat: string;
  currentTimeFormat: string;
  network: Network;
  isMainnet: boolean;
  intl: Record<string, any>;
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
    networkName: intl.formatMessage(globalMessages[`network_${network}`]),
    creationDate: intl.formatMessage(messages.creationDate, {
      date,
      time,
    }),
    noteLabel: intl.formatMessage(messages.noteLabel),
    title: intl.formatMessage(messages.title),
    author: intl.formatMessage(messages.author),
  });
};
