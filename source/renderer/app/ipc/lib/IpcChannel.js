// @flow
import { isString } from 'lodash';
import { ipcRenderer } from 'electron';

/**
 * Provides a coherent, typed api for working with electron
 * ipc messages over named channels. Where possible it uses
 * promises to reduce the necessary boilerplate for request
 * and response cycles.
 */
export class IpcChannel<Request, Response> {

  /**
   * The ipc channel name
   * @private
   */
  channel: string;

  /**
   * Sets up the ipc channel and checks that its name is valid
   * @param channelName
   */
  constructor(channelName: string) {
    if (isString(channelName) && channelName !== '') {
      this.channel = channelName;
    } else {
      throw new Error(`Invalid channel name ${channelName} provided`);
    }
  }

  /**
   * Sends a request over ipc to the main process and waits for the
   * next response on the same channel. It returns a promise which
   * is resolved or rejected with the response depending on the
   * `isOk` flag set by the respondant.
   *
   * @param request
   * @returns {Promise<Response>}
   */
  async request(request: Request): Promise<Response> {
    return new Promise((resolve, reject) => {
      ipcRenderer.send(this.channel, request);
      ipcRenderer.once(this.channel, (event, isOk: boolean, response: Response) => {
        if (isOk) {
          resolve(response);
        } else {
          reject(response);
        }
      });
    });
  }

  /**
   * Sets up a permanent handler for receiving messages on this channel.
   * This should be used to receive messages that are broadcasted by the
   * main process.
   *
   * @param handler
   */
  receive(handler: Function): void {
    ipcRenderer.on(this.channel, (event, isOk: boolean, response: Response) => handler(response));
  }
}
