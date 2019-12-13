// @flow
import { defineMessages } from 'react-intl';

export default defineMessages({
  downloadLogsSuccess: {
    id: 'notification.downloadLogsSuccess',
    defaultMessage: '!!!Logs successfully downloaded',
    description:
      'Notification for download logs in the Loading and Settings pages.',
  },
  downloadLogsProgress: {
    id: 'notification.downloadLogsProgress',
    defaultMessage: '!!!Preparing logs for download',
    description:
      'Notification for download logs in progress in the Loading and Settings pages.',
  },
  copyAddress: {
    id: 'notification.copyAddress',
    defaultMessage:
      '!!!Address: <strong>{walletAddress}</strong> copied to clipboard',
    description:
      'Notification for the wallet address copy success in the Wallet Receive page.',
  },
  copyStateDirectoryPath: {
    id: 'notification.copyStateDirectoryPath',
    defaultMessage: '!!!Daedalus state directory copied to clipboard',
    description:
      'Notification for the state directory copy success in the Diagnostics page.',
  },
});
