// @flow
import { IpcChannel } from './lib/IpcChannel';
import { AWAIT_UPDATE_CHANNEL } from '../../../common/ipc-api';

export const awaitUpdateChannel: IpcChannel<void, void> = (
  new IpcChannel(AWAIT_UPDATE_CHANNEL)
);
