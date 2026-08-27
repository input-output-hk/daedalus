import {
  DAPP_CIP30_WALLET_CHANNEL,
  DappCip30WalletMainRequest,
  DappCip30WalletRendererResponse,
} from '../../../common/ipc/api';
import { parseCip30WalletRequest } from '../../../common/cip30/executor';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

const channel = new RendererIpcChannel<
  DappCip30WalletMainRequest,
  DappCip30WalletRendererResponse
>(DAPP_CIP30_WALLET_CHANNEL);

let handler:
  | ((
      request: DappCip30WalletMainRequest
    ) => Promise<DappCip30WalletRendererResponse>)
  | undefined;
let registered = false;

export const bindCip30WalletRenderer = (
  next: (
    request: DappCip30WalletMainRequest
  ) => Promise<DappCip30WalletRendererResponse>
): (() => void) => {
  handler = next;
  if (!registered) {
    registered = true;
    channel.onRequest(async (value) =>
      handler?.(parseCip30WalletRequest(value))
    );
  }
  return () => {
    if (handler === next) handler = undefined;
  };
};
