import { IpcChannel } from '../../../../common/ipc/lib/IpcChannel';
import type {
  IpcReceiver,
  IpcSender,
} from '../../../../common/ipc/lib/IpcChannel';

/**
 * Subclass of IpcChannel that uses ipcRenderer to send and receive messages.
 */
export class RendererIpcChannel<Incoming, Outgoing> extends IpcChannel<
  Incoming,
  Outgoing
> {
  async send(
    message: Outgoing,
    sender: IpcSender = global.ipcRenderer,
    receiver: IpcReceiver = global.ipcRenderer
  ): Promise<Incoming> {
    return super.send(message, sender, receiver);
  }

  async request(
    message: Outgoing,
    sender: IpcSender = global.ipcRenderer,
    receiver: IpcReceiver = global.ipcRenderer
  ): Promise<Incoming> {
    return super.request(message, sender, receiver);
  }

  onReceive(
    handler: (message: Incoming) => Promise<Outgoing>,
    receiver: IpcReceiver = global.ipcRenderer
  ): void {
    super.onReceive(handler, receiver);
  }

  onRequest(
    handler: (arg0: Incoming) => Promise<Outgoing>,
    receiver: IpcReceiver = global.ipcRenderer
  ): void {
    super.onRequest(handler, receiver);
  }
}
