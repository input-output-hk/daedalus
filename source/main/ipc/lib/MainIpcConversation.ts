import { ipcMain } from 'electron';
import type {
  IpcEvent,
  IpcReceiver,
  IpcSender,
} from '../../../common/ipc/lib/IpcConversation';
import { IpcConversation } from '../../../common/ipc/lib/IpcConversation';
import {
  authorizeTrustedRenderer,
  isTrustedRendererEvent,
  onTrustedRendererInvalidated,
} from './trustedRendererIpcAuthority';

/**
 * Subclass of IpcChannel that uses ipcMain to receive messages.
 */
export class MainIpcConversation<Incoming, Outgoing> extends IpcConversation<
  Incoming,
  Outgoing
> {
  constructor(channelName: string) {
    super(channelName, {
      authorize: (event) => authorizeTrustedRenderer(event as any),
      authorizeResponse: (event) => isTrustedRendererEvent(event as any),
      onOutgoingInvalidated: onTrustedRendererInvalidated,
    });
  }

  async send(
    message: Outgoing,
    sender: IpcSender,
    receiver: IpcReceiver = ipcMain
  ): Promise<Incoming> {
    return super.request(message, sender.webContents || sender, receiver);
  }

  async request(
    message: Outgoing,
    sender: IpcSender,
    receiver: IpcReceiver = ipcMain
  ): Promise<Incoming> {
    return super.request(message, sender.webContents || sender, receiver);
  }

  onReceive(
    handler: (message: Incoming, event?: IpcEvent) => Promise<Outgoing>,
    receiver: IpcReceiver = ipcMain
  ): void {
    super.onRequest(handler, receiver);
  }

  onRequest(
    handler: (message: Incoming, event?: IpcEvent) => Promise<Outgoing>,
    receiver: IpcReceiver = ipcMain
  ): void {
    super.onRequest(handler, receiver);
  }
}
