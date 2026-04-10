import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  SET_CHAIN_STORAGE_DIRECTORY_CHANNEL,
  GET_CHAIN_STORAGE_DIRECTORY_CHANNEL,
  VALIDATE_CHAIN_STORAGE_DIRECTORY_CHANNEL,
} from '../../common/ipc/api';
import type {
  SetChainStorageDirectoryRendererRequest,
  SetChainStorageDirectoryMainResponse,
  GetChainStorageDirectoryRendererRequest,
  GetChainStorageDirectoryMainResponse,
  ValidateChainStorageDirectoryRendererRequest,
  ValidateChainStorageDirectoryMainResponse,
} from '../../common/ipc/api';
import { logger } from '../utils/logging';
import { chainStorageCoordinator } from '../utils/chainStorageCoordinator';

const setChainStorageDirectoryChannel: MainIpcChannel<
  SetChainStorageDirectoryRendererRequest,
  SetChainStorageDirectoryMainResponse
> = new MainIpcChannel(SET_CHAIN_STORAGE_DIRECTORY_CHANNEL);

const getChainStorageDirectoryChannel: MainIpcChannel<
  GetChainStorageDirectoryRendererRequest,
  GetChainStorageDirectoryMainResponse
> = new MainIpcChannel(GET_CHAIN_STORAGE_DIRECTORY_CHANNEL);

const validateChainStorageDirectoryChannel: MainIpcChannel<
  ValidateChainStorageDirectoryRendererRequest,
  ValidateChainStorageDirectoryMainResponse
> = new MainIpcChannel(VALIDATE_CHAIN_STORAGE_DIRECTORY_CHANNEL);

export const handleChainStorageRequests = () => {
  chainStorageCoordinator
    .verifySymlink()
    .then((verification) => {
      if (!verification.isValid && verification.path) {
        logger.warn('Chain storage symlink verification failed on startup', {
          verification,
        });
      }
    })
    .catch((error) => {
      logger.warn('Chain storage symlink startup verification failed', {
        error,
      });
    });

  setChainStorageDirectoryChannel.onRequest(async ({ path }) => {
    return chainStorageCoordinator.setDirectory(path);
  });

  getChainStorageDirectoryChannel.onRequest(async () =>
    chainStorageCoordinator.getConfig()
  );

  validateChainStorageDirectoryChannel.onRequest(async ({ path }) =>
    chainStorageCoordinator.validate(path)
  );
};
