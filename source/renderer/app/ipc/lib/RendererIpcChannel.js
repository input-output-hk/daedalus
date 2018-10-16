// @flow
import { ipcRenderer } from 'electron';
import { IpcChannel } from '../../../../common/ipc/lib/IpcChannel';
import type { IpcReceiver, IpcSender } from '../../../../common/ipc/lib/IpcChannel';

/**
 * Subclass of IpcChannel that uses ipcRenderer to send and receive messages.
 */
export class RendererIpcChannel<Incoming, Outgoing> extends IpcChannel<Incoming, Outgoing> {

  async send(
    message: Outgoing,
    sender: IpcSender = ipcRenderer,
    receiver: IpcReceiver = ipcRenderer
  ): Promise<Incoming> {
    return super.send(message, sender, receiver);
  }
  onReceive(
    handler: (message: Incoming) => Promise<Outgoing>,
    receiver: IpcReceiver = ipcRenderer
  ): void {
    super.onReceive(handler, receiver);
  }

}
