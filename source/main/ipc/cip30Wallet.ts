import {
  DAPP_CIP30_WALLET_CHANNEL,
  DappCip30WalletMainRequest,
  DappCip30WalletRendererResponse,
} from '../../common/ipc/api';
import {
  parseCip30WalletRequest,
  parseCip30WalletResponse,
} from '../../common/cip30/executor';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  awaitIpcResponse,
  currentWindowSender,
} from './lib/currentWindowSender';

const channel = new MainIpcChannel<
  DappCip30WalletRendererResponse,
  DappCip30WalletMainRequest
>(DAPP_CIP30_WALLET_CHANNEL);

export const executeCip30WalletRequest = async (
  requestValue: DappCip30WalletMainRequest
): Promise<DappCip30WalletRendererResponse> => {
  const request = parseCip30WalletRequest(requestValue);
  const response = await awaitIpcResponse(
    channel.send(request, currentWindowSender.sender)
  );
  if (response === undefined) throw new Error('CIP-30 wallet unavailable');
  return parseCip30WalletResponse(request, response);
};
