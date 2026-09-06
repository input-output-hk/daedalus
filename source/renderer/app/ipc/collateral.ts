import {
  DAPP_COLLATERAL_CHANNEL,
  DappCollateralMainResponse,
  DappCollateralRendererRequest,
  WALLET_INPUT_SELECTION_CHANNEL,
  WalletInputSelectionMainResponse,
  WalletInputSelectionRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const dappCollateralChannel = new RendererIpcChannel<
  DappCollateralMainResponse,
  DappCollateralRendererRequest
>(DAPP_COLLATERAL_CHANNEL);

export const walletInputSelectionChannel = new RendererIpcChannel<
  WalletInputSelectionMainResponse,
  WalletInputSelectionRendererRequest
>(WALLET_INPUT_SELECTION_CHANNEL);
