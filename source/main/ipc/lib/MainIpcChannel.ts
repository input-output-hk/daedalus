import { ipcMain } from 'electron';
import { IpcChannel } from '../../../common/ipc/lib/IpcChannel';
import type {
  IpcReceiver,
  IpcSender,
} from '../../../common/ipc/lib/IpcChannel';

/**
 * Subclass of IpcChannel that uses ipcMain to receive messages.
 */
export class MainIpcChannel<Incoming, Outgoing> extends IpcChannel<
  Incoming,
  Outgoing
> {
  async send(
    message: Outgoing,
    sender: IpcSender,
    receiver: IpcReceiver = ipcMain
  ): Promise<Incoming> {
    return super.send(message, sender, receiver);
  }

  async request(
    message: Outgoing,
    sender: IpcSender,
    receiver: IpcReceiver = ipcMain
  ): Promise<Incoming> {
    return super.request(message, sender, receiver);
  }

  onReceive(
    handler: (message: Incoming) => Promise<Outgoing>,
    receiver: IpcReceiver = ipcMain
  ): void {
    super.onReceive(handler, receiver);
  }

  onRequest(
    handler: (arg0: Incoming) => Promise<Outgoing>,
    receiver: IpcReceiver = ipcMain
  ): void {
    super.onRequest(handler, receiver);
  }
}
