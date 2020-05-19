// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  UPDATE_MANAGER_INIT,
  UPDATE_MANAGER_STATUS,
  UPDATE_MANAGER_REQUEST_DOWNLOAD,
} from '../../common/ipc/api';
import type { UpdateManagerStatusResponse } from '../../common/types/update-manager.types';

const updateManagerInit = async (): Promise<UpdateManagerStatusResponse> => {
  const hasUpdateAvailable = await checkHasUpdateAvailable();
  const isDownloadingUpdate = await checkIsDownloadingUpdate();
  const updateDownloadProgress = await checkUpdateDownloadProgress();
  return {
    hasUpdateAvailable,
    isDownloadingUpdate,
    updateDownloadProgress,
  };
};

const updateManagerStatus = async (): Promise<UpdateManagerStatusResponse> => {
  return {};
};

const updateManagerRequestDownload = async (): Promise<UpdateManagerStatusResponse> => {
  return {};
};

const checkHasUpdateAvailable = async (): Promise<boolean> => false;
const checkIsDownloadingUpdate = async (): Promise<boolean> => false;
const checkUpdateDownloadProgress = async (): Promise<number> => 0;

const updateManagerInitChannel: MainIpcChannel<
  void,
  UpdateManagerStatusResponse
> = new MainIpcChannel(UPDATE_MANAGER_INIT);

const updateManagerStatusChannel: MainIpcChannel<
  void,
  UpdateManagerStatusResponse
> = new MainIpcChannel(UPDATE_MANAGER_STATUS);

const updateManagerRequestDownloadChannel: MainIpcChannel<
  void,
  UpdateManagerStatusResponse
> = new MainIpcChannel(UPDATE_MANAGER_REQUEST_DOWNLOAD);

export default () => {
  updateManagerInitChannel.onReceive(updateManagerInit);
  updateManagerStatusChannel.onReceive(updateManagerStatus);
  updateManagerRequestDownloadChannel.onReceive(updateManagerRequestDownload);
};
