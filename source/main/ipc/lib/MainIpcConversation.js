// @flow
import { ipcMain } from 'electron';
import type {
  IpcReceiver,
  IpcSender,
} from '../../../common/ipc/lib/IpcConversation';
import { IpcConversation } from '../../../common/ipc/lib/IpcConversation';

/**
 * Subclass of IpcChannel that uses ipcMain to receive messages.
 */
export class MainIpcConversation<Incoming, Outgoing> extends IpcConversation<
  Incoming,
  Outgoing
> {
  async send(
    message: Outgoing,
    sender: IpcSender,
    receiver: IpcReceiver = ipcMain
  ): Promise<Incoming> {
    return super.request(message, sender, receiver);
  }

  async request(
    message: Outgoing,
    sender: IpcSender,
    receiver: IpcReceiver = ipcMain
  ): Promise<Incoming> {
    console.log('MAIN REQUEST CONV');
    console.log('message', message);
    console.log('sender', sender);
    console.log('receiver', receiver);
    return super.request(message, sender, receiver);
  }

  onReceive(
    handler: (message: Incoming) => Promise<Outgoing>,
    receiver: IpcReceiver = ipcMain
  ): void {
    console.log('MAIN RECEIVE CONV');
    super.onRequest(handler, receiver);
  }

  onRequest(
    handler: Incoming => Promise<Outgoing>,
    receiver: IpcReceiver = ipcMain
  ): void {
    console.log('MAIN REQ CONV');
    // console.log('onRequest ------');
    // console.log('handler', handler);
    // console.log('receiver', receiver);
    super.onRequest(handler, receiver);
  }
}
