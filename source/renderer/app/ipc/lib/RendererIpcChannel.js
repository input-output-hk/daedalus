// @flow
import { ipcRenderer } from 'electron';
import { IpcChannel } from '../../../../common/ipc/lib/IpcChannel';
import type { IpcReceiver, IpcSender } from '../../../../common/ipc/lib/IpcChannel';

/**
 * Subclass of IpcChannel that uses ipcRenderer to send and receive messages.
 */
export class RendererIpcChannel<
  Request, AwaitedResponse, ReceivedRequest, Response
> extends IpcChannel<
  Request, AwaitedResponse, ReceivedRequest, Response
> {
  async send(
    request: Request,
    sender: IpcSender = ipcRenderer,
    receiver: IpcReceiver = ipcRenderer
  ): Promise<AwaitedResponse> {
    return super.send(request, sender, receiver);
  }
  onReceive(
    handler: (request: ReceivedRequest) => Promise<Response>,
    receiver: IpcReceiver = ipcRenderer
  ): void {
    super.onReceive(handler, receiver);
  }
}
