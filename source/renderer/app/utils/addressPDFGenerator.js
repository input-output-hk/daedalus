// @flow
import { defineMessages } from 'react-intl';
import { generateAddressPDFChannel } from '../ipc/generateAddressPDFChannel';

const messages = defineMessages({
  fileTitle: {
    id: 'wallet.receive.pdf.infoTitle',
    defaultMessage: '!!!Daedalus wallet address',
    description: 'PDF title',
  },
  fileAuthor: {
    id: 'wallet.receive.pdf.infoAuthor',
    defaultMessage: '!!!Daedalus wallet',
    description: 'PDF author',
  },
});

type Params = {
  address: string,
  contentTitle: string,
  filePath: string,
  intl: Object,
};

export const addressPDFGenerator = async ({
  address,
  contentTitle,
  filePath,
  intl,
}: Params) => {
  await generateAddressPDFChannel.send({
    address,
    contentTitle,
    filePath,
    fileTitle: intl.formatMessage(messages.fileTitle),
    fileAuthor: intl.formatMessage(messages.fileAuthor),
  });
};
