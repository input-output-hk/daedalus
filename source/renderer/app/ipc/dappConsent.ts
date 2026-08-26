import { DAPP_CONSENT_RENDER_CHANNEL } from '../../../common/ipc/api';
import type {
  DappConsentRenderMainRequest,
  DappConsentRenderRendererResponse,
} from '../../../common/ipc/api';
import { parseDappConsentRender } from '../../../common/ipc/dapp';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

const renderChannel = new RendererIpcChannel<
  DappConsentRenderMainRequest,
  DappConsentRenderRendererResponse
>(DAPP_CONSENT_RENDER_CHANNEL);

let handler:
  | ((
      message: DappConsentRenderMainRequest
    ) => Promise<DappConsentRenderRendererResponse>)
  | undefined;
let registered = false;

export const bindDappConsentRenderer = (
  next: (
    message: DappConsentRenderMainRequest
  ) => Promise<DappConsentRenderRendererResponse>
): (() => void) => {
  handler = next;
  if (!registered) {
    registered = true;
    renderChannel.onRequest(async (value) =>
      handler?.(parseDappConsentRender(value))
    );
  }
  return () => {
    if (handler === next) handler = undefined;
  };
};
