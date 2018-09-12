// @flow
import { IpcChannel } from './lib/IpcChannel';
import { CARDANO_NODE_STATE_CHANGE_CHANNEL } from '../../../common/ipc-api';
import type { CardanoNodeState } from '../../../common/types/cardanoNodeTypes';

export const cardanoNodeStateChangeChannel: IpcChannel<void, CardanoNodeState> = (
  new IpcChannel(CARDANO_NODE_STATE_CHANGE_CHANNEL)
);
