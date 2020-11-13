// @flow
import Action from '../actions/lib/Action';

export type NotificationId =
  | 'downloadLogsProgress'
  | 'downloadLogsSuccess'
  | 'downloadRewardsCSVSuccess'
  | 'downloadTransactionsCSVSuccess'
  | 'copyAddress'
  | 'copyStateDirectoryPath';

export type NotificationConfig = {
  id: NotificationId,
  duration?: number,
  actionToListenAndOpen: Action<any>,
  actionToListenAndClose?: Action<any>,
};
