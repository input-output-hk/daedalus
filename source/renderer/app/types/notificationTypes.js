// @flow
import Action from '../actions/lib/Action';

export type NotificationId =
  | 'copyPublicKey'
  | 'copyAddress'
  | 'copyStateDirectoryPath'
  | 'downloadAddressPDFSuccess'
  | 'downloadLogsProgress'
  | 'downloadLogsSuccess'
  | 'downloadQRCodeImageSuccess'
  | 'downloadRewardsCSVSuccess'
  | 'downloadTransactionsCSVSuccess';

export type NotificationConfig = {
  id: NotificationId,
  duration?: number,
  actionToListenAndOpen: Action<any>,
  actionToListenAndClose?: Action<any>,
};
