import { ipcMain } from 'electron';
import { IpcChannel } from '../../../common/ipc/lib/IpcChannel';
import type {
  IpcEvent,
  IpcReceiver,
  IpcSender,
} from '../../../common/ipc/lib/IpcChannel';
import {
  authorizeTrustedRenderer,
  isTrustedRendererEvent,
  onTrustedRendererInvalidated,
} from './trustedRendererIpcAuthority';

/**
 * Subclass of IpcChannel that uses ipcMain to receive messages.
 */
export class MainIpcChannel<Incoming, Outgoing> extends IpcChannel<
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
    return super.send(message, sender.webContents || sender, receiver);
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
    super.onReceive(handler, receiver);
  }

  onRequest(
    handler: (message: Incoming, event?: IpcEvent) => Promise<Outgoing>,
    receiver: IpcReceiver = ipcMain
  ): void {
    super.onRequest(handler, receiver);
  }
}
