// @flow
import moment from 'moment';
import { defineMessages } from 'react-intl';
import { generateAddressPDFChannel } from '../ipc/generateAddressPDFChannel';

const messages = defineMessages({
  creationDate: {
    id: 'wallet.receive.pdf.creationDate',
    defaultMessage: '!!!PDF creation date {date} {time}',
    description: 'PDF title',
  },
  noteTitle: {
    id: 'wallet.receive.pdf.noteTitle',
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
});

type Params = {
  address: string,
  note: string,
  filePath: string,
  currentDateFormat: string,
  currentTimeFormat: string,
  intl: Object,
};

export const addressPDFGenerator = async ({
  address,
  note,
  filePath,
  intl,
  currentDateFormat,
  currentTimeFormat,
}: Params) => {
  const date = moment().format(currentDateFormat);
  const time = moment().format(currentTimeFormat);
  await generateAddressPDFChannel.send({
    address,
    filePath,
    note,
    creationDate: intl.formatMessage(messages.creationDate, { date, time }),
    noteTitle: intl.formatMessage(messages.noteTitle),
    title: intl.formatMessage(messages.title),
    author: intl.formatMessage(messages.author),
  });
};
