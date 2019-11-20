// @flow
import { defineMessages } from 'react-intl';
import { generateAddressPDFChannel } from '../ipc/generateAddressPDFChannel';

const messages = defineMessages({
  infoTitle: {
    id: 'wallet.receive.pdf.infoTitle',
    defaultMessage: '!!!Daedalus wallet address',
    description: 'PDF title',
  },
  infoAuthor: {
    id: 'wallet.receive.pdf.infoAuthor',
    defaultMessage: '!!!Daedalus wallet',
    description: 'PDF author',
  },
});

type Params = {
  address: string,
  contentTitle: string,
  fileName: string,
  intl: Object,
};

export default async ({ address, contentTitle, filePath, intl }: Params) => {
  console.log('address', address);
  console.log('contentTitle', contentTitle);
  console.log('filePath', filePath);
  await generateAddressPDFChannel.send({
    address,
    contentTitle,
    filePath,
    fileTitle: intl.formatMessage(messages.fileTitle),
    fileAuthor: intl.formatMessage(messages.fileAuthor),
  });
};
