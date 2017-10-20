// @flow
import WalletsActions from './wallets-actions';

export type EtcActionsMap = {
  wallets: WalletsActions,
};

const etcActionsMap: EtcActionsMap = {
  wallets: new WalletsActions(),
};

export default etcActionsMap;
