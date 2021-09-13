// @flow
import Action from '../actions/lib/Action';

export type NotificationId =
  | 'copyWalletPublicKey'
  | 'copyICOPublicKey'
  | 'copyAddress'
  | 'copyAssetItem'
  | 'copyStateDirectoryPath'
  | 'downloadAddressPDFSuccess'
  | 'downloadLogsProgress'
  | 'downloadLogsSuccess'
  | 'downloadQRCodeImageSuccess'
  | 'downloadVotingPDFSuccess'
  | 'downloadRewardsCSVSuccess'
  | 'downloadTransactionsCSVSuccess';

export type NotificationConfig = {
  id: NotificationId,
  duration?: number,
  actionToListenAndOpen: Action<any>,
  actionToListenAndClose?: Action<any>,
};
