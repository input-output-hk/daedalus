import type { ElectronStoreMessage } from '../../../common/ipc/api';
import { ELECTRON_STORE_CHANNEL } from '../../../common/ipc/api';
import { RendererIpcConversation } from './lib/RendererIpcConversation';
// RendererIpcConversation<Incoming, Outgoing>
export const electronStoreConversation: RendererIpcConversation<
  any,
  ElectronStoreMessage
> = new RendererIpcConversation(ELECTRON_STORE_CHANNEL);
