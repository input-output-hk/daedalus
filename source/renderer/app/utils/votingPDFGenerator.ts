import moment from 'moment';
import path from 'path';
import { defineMessages } from 'react-intl';
import { generateVotingPDFChannel } from '../ipc/generateVotingPDFChannel';
import type { Network } from '../../../common/types/environment.types';
import { generateFileNameWithTimestamp } from '../../../common/utils/files';
import { showSaveDialogChannel } from '../ipc/show-file-dialog-channels';
import globalMessages from '../i18n/global-messages';

const messages = defineMessages({
  title: {
    id: 'voting.votingRegistration.pdf.title',
    defaultMessage: '!!!Fund{nextVotingFundNumber} Voting Registration',
    description: 'PDF title',
  },
  walletNameLabel: {
    id: 'voting.votingRegistration.pdf.walletNameLabel',
    defaultMessage: '!!!Wallet name',
    description: 'PDF wallet name title',
  },
  filename: {
    id: 'voting.votingRegistration.pdf.filename',
    defaultMessage: '!!!voting-registration',
    description: 'PDF filename title',
  },
  networkLabel: {
    id: 'voting.votingRegistration.pdf.networkLabel',
    defaultMessage: '!!!Cardano network:',
    description: 'PDF networkLabel label',
  },
  author: {
    id: 'voting.votingRegistration.pdf.author',
    defaultMessage: '!!!Daedalus wallet',
    description: 'PDF author',
  },
});
type Params = {
  nextVotingFundNumber: number;
  qrCode: string;
  walletName: string;
  currentLocale: string;
  currentDateFormat: string;
  currentTimeFormat: string;
  desktopDirectoryPath: string;
  network: Network;
  isMainnet: boolean;
  intl: Record<string, any>;
};
export const votingPDFGenerator = async ({
  nextVotingFundNumber,
  qrCode,
  walletName,
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  desktopDirectoryPath,
  network,
  isMainnet,
  intl,
}: Params) => {
  // Consolidate data
  const title = intl.formatMessage(messages.title, {
    nextVotingFundNumber,
  });
  const creationDate = moment().format(
    `${currentDateFormat} ${currentTimeFormat}`
  );
  const walletNameLabel = intl.formatMessage(messages.walletNameLabel);
  const networkLabel = intl.formatMessage(messages.networkLabel);
  const networkName = intl.formatMessage(globalMessages[`network_${network}`]);
  const author = intl.formatMessage(messages.author);
  // Generate the filePath
  const localizedFileName = intl.formatMessage(messages.filename);
  const prefix = `fund${nextVotingFundNumber}-${localizedFileName}-${walletName}`;
  const name = generateFileNameWithTimestamp({
    prefix,
    extension: '',
    isUTC: false,
  });
  const fileExtension = 'pdf';
  const defaultPath = path.join(
    desktopDirectoryPath,
    `${name}.${fileExtension}`
  );
  const params = {
    defaultPath,
    filters: [
      {
        name,
        extensions: [fileExtension],
      },
    ],
  };
  const dialogPath = await showSaveDialogChannel.send(params);
  const filePath = dialogPath.filePath || '';
  await generateVotingPDFChannel.send({
    title,
    currentLocale,
    creationDate,
    qrCode,
    walletNameLabel,
    walletName,
    isMainnet,
    networkLabel,
    networkName,
    filePath,
    author,
  });
};
