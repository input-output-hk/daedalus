import {
  DAPP_COLLATERAL_CHANNEL,
  DappCollateralMainResponse,
  DappCollateralRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const dappCollateralChannel = new RendererIpcChannel<
  DappCollateralMainResponse,
  DappCollateralRendererRequest
>(DAPP_COLLATERAL_CHANNEL);
