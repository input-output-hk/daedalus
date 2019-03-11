// @flow
import rimraf from 'rimraf';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { DeleteKeyFileChannelName } from '../../common/ipc/api';
import type {
  DeleteKeyFileRendererRequest,
  DeleteKeyFileMainResponse,
} from '../../common/ipc/api';

export const deleteKeyFileChannel: (
  // IpcChannel<Incoming, Outgoing>
  MainIpcChannel<DeleteKeyFileRendererRequest, DeleteKeyFileMainResponse>
) = (
  new MainIpcChannel(DeleteKeyFileChannelName)
);

export const handleDeleteKeyFileRequests = () => {
  deleteKeyFileChannel.onReceive((request: DeleteKeyFileRendererRequest) => (
    new Promise((resolve, reject) => {
      const { filePath } = request;
      const lockFilePath = `${filePath}.lock`; // We don't want to leave .lock file behind!
      try {
        rimraf.sync(filePath);
        rimraf.sync(lockFilePath);
        resolve();
      } catch (error) {
        reject(error);
      }
    })
  ));
};
