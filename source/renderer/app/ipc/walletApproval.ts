import { WALLET_APPROVAL_RENDER_CHANNEL } from '../../../common/ipc/api';
import type {
  WalletApprovalRenderMainRequest,
  WalletApprovalRenderRendererResponse,
} from '../../../common/ipc/api';
import { parseWalletApprovalRender } from '../../../common/ipc/walletApproval';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

const renderChannel = new RendererIpcChannel<
  WalletApprovalRenderMainRequest,
  WalletApprovalRenderRendererResponse
>(WALLET_APPROVAL_RENDER_CHANNEL);

let handler:
  | ((
      message: WalletApprovalRenderMainRequest
    ) => Promise<WalletApprovalRenderRendererResponse>)
  | undefined;
let registered = false;

export const bindWalletApprovalRenderer = (
  next: (
    message: WalletApprovalRenderMainRequest
  ) => Promise<WalletApprovalRenderRendererResponse>
): (() => void) => {
  handler = next;
  if (!registered) {
    registered = true;
    renderChannel.onRequest(async (value) =>
      handler?.(parseWalletApprovalRender(value))
    );
  }
  return () => {
    if (handler === next) handler = undefined;
  };
};
