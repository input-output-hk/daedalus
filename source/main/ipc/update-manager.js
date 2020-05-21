// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  UPDATE_MANAGER_INIT,
  UPDATE_MANAGER_STATUS,
  UPDATE_MANAGER_REQUEST_DOWNLOAD,
} from '../../common/ipc/api';
import type { UpdateManagerStatusResponse } from '../../common/types/update-manager.types';

const updateManagerInit = async (): Promise<UpdateManagerStatusResponse> => {
  const isUpdateAvailable = await checkisUpdateAvailable();
  const isDownloadingUpdate = await checkIsDownloadingUpdate();
  const updateDownloadProgress = await checkUpdateDownloadProgress();
  const haspendingUpdateDownload = await checkHaspendingUpdateDownload();
  return {
    isUpdateAvailable,
    isDownloadingUpdate,
    updateDownloadProgress,
    haspendingUpdateDownload,
  };
};

const updateManagerStatus = async (): Promise<UpdateManagerStatusResponse> => {
  const isUpdateAvailable = await checkisUpdateAvailable();
  const isDownloadingUpdate = await checkIsDownloadingUpdate();
  const updateDownloadProgress = await checkUpdateDownloadProgress();
  const haspendingUpdateDownload = await checkHaspendingUpdateDownload();
  return {
    isUpdateAvailable,
    isDownloadingUpdate,
    updateDownloadProgress,
    haspendingUpdateDownload,
  };
};

const updateManagerRequestDownload = async (): Promise<UpdateManagerStatusResponse> => {
  const isUpdateAvailable = await checkisUpdateAvailable();
  const isDownloadingUpdate = await checkIsDownloadingUpdate();
  const updateDownloadProgress = await checkUpdateDownloadProgress();
  const haspendingUpdateDownload = await checkHaspendingUpdateDownload();
  return {
    isUpdateAvailable,
    isDownloadingUpdate,
    updateDownloadProgress,
    haspendingUpdateDownload,
  };
};

const checkisUpdateAvailable = async (): Promise<boolean> => false;
const checkIsDownloadingUpdate = async (): Promise<boolean> => false;
const checkUpdateDownloadProgress = async (): Promise<number> => 0;
const checkHaspendingUpdateDownload = async (): Promise<boolean> => false;

const updateManagerInitChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<void, UpdateManagerStatusResponse> = new MainIpcChannel(
  UPDATE_MANAGER_INIT
);

const updateManagerStatusChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<void, UpdateManagerStatusResponse> = new MainIpcChannel(
  UPDATE_MANAGER_STATUS
);

const updateManagerRequestDownloadChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<void, UpdateManagerStatusResponse> = new MainIpcChannel(
  UPDATE_MANAGER_REQUEST_DOWNLOAD
);

export default () => {
  updateManagerInitChannel.onReceive(updateManagerInit);
  updateManagerStatusChannel.onReceive(updateManagerStatus);
  updateManagerRequestDownloadChannel.onReceive(updateManagerRequestDownload);
};
