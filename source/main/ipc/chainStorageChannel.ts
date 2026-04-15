import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  SET_CHAIN_STORAGE_DIRECTORY_CHANNEL,
  GET_CHAIN_STORAGE_DIRECTORY_CHANNEL,
  VALIDATE_CHAIN_STORAGE_DIRECTORY_CHANNEL,
  PREPARE_CHAIN_STORAGE_LOCATION_CHANGE_CHANNEL,
} from '../../common/ipc/api';
import type {
  SetChainStorageDirectoryRendererRequest,
  SetChainStorageDirectoryMainResponse,
  GetChainStorageDirectoryRendererRequest,
  GetChainStorageDirectoryMainResponse,
  ValidateChainStorageDirectoryRendererRequest,
  ValidateChainStorageDirectoryMainResponse,
  PrepareChainStorageLocationChangeRendererRequest,
  PrepareChainStorageLocationChangeMainResponse,
} from '../../common/ipc/api';
import { chainStorageCoordinator } from '../utils/chainStorageCoordinator';
import { getMithrilBootstrapNodeState } from './mithrilBootstrapChannel';

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

const prepareChainStorageLocationChangeChannel: MainIpcChannel<
  PrepareChainStorageLocationChangeRendererRequest,
  PrepareChainStorageLocationChangeMainResponse
> = new MainIpcChannel(PREPARE_CHAIN_STORAGE_LOCATION_CHANGE_CHANNEL);

let chainStorageRequestsInitialized = false;

export const handleChainStorageRequests = () => {
  if (chainStorageRequestsInitialized) return;
  chainStorageRequestsInitialized = true;
  setChainStorageDirectoryChannel.onRequest(async ({ path }) => {
    return chainStorageCoordinator.setDirectory(
      path,
      getMithrilBootstrapNodeState()
    );
  });

  getChainStorageDirectoryChannel.onRequest(async () =>
    chainStorageCoordinator.getConfig()
  );

  validateChainStorageDirectoryChannel.onRequest(async ({ path }) =>
    chainStorageCoordinator.validate(path)
  );

  prepareChainStorageLocationChangeChannel.onRequest(async () =>
    chainStorageCoordinator.prepareForLocationChange(
      getMithrilBootstrapNodeState()
    )
  );
};
