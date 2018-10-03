// @flow
import { ipcMain } from 'electron';
import { IpcChannel } from '../../../common/ipc/lib/IpcChannel';
import type { IpcReceiver, IpcSender } from '../../../common/ipc/lib/IpcChannel';

/**
 * Subclass of IpcChannel that uses ipcMain to receive messages.
 */
export class MainIpcChannel<
  Request, AwaitedResponse, ReceivedRequest, Response
> extends
  IpcChannel<
    Request, AwaitedResponse, ReceivedRequest, Response
> {

  async send(
    request: Request, sender: IpcSender, receiver: IpcReceiver = ipcMain
  ): Promise<AwaitedResponse> {
    return super.send(request, sender, receiver);
  }

  onReceive(
    handler: (request: ReceivedRequest) => Promise<Response>,
    receiver: IpcReceiver = ipcMain
  ): void {
    super.onReceive(handler, receiver);
  }
}

