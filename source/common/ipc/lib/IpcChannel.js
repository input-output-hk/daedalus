// @flow
import { isString } from 'lodash';

export type IpcSender = {
  send: (channel: string, ...args: Array<any>) => void
};

export type IpcEvent = {
  sender: IpcSender,
}

export type IpcReceiver = {
  on: (channel: string, (event: IpcEvent, ...args: Array<any>) => Promise<any>) => void,
  once: (channel: string, (event: IpcEvent, isOk: boolean, ...args: Array<any>) => void) => void
};

/**
 * Provides a coherent, typed api for working with electron
 * ipc messages over named channels. Where possible it uses
 * promises to reduce the necessary boilerplate for request
 * and response cycles.
 */
export class IpcChannel<Request, AwaitedResponse, ReceivedRequest, Response> {

  /**
   * Each ipc channel should be a singleton (based on the channelName)
   * Here we track the created instances.
   */
  static _instances = {};
  /**
   * The public broadcast channel (any process will receive these messages)
   * @private
   */
  _broadcastChannel: string;
  /**
   * The response channel between a main and render process
   * @private
   */
  _responseChannel: string;
  /**
   * Sets up the ipc channel and checks that its name is valid.
   * Ensures that only one instance per channel name can exist.
   *
   * @param channelName {String}
   */
  constructor(channelName: string) {
    if (!isString(channelName) || channelName === '') {
      throw new Error(`Invalid channel name ${channelName} provided`);
    }
    // Enforce the singleton pattern based on the channel name
    const existingChannel = IpcChannel._instances[channelName];
    if (existingChannel) return existingChannel;
    IpcChannel._instances[channelName] = this;

    this._broadcastChannel = channelName + '-broadcast';
    this._responseChannel = channelName + '-response';
  }

  /**
   * Sends a request over ipc to the receiver and waits for the next response on the
   * same channel. It returns a promise which is resolved or rejected with the response
   * depending on the `isOk` flag set by the respondant.
   *
   * @param request {Request}
   * @param sender {IpcSender}
   * @param receiver {IpcReceiver}
   * @returns {Promise<AwaitedResponse>}
   */
  async send(request: Request, sender: IpcSender, receiver: IpcReceiver): Promise<AwaitedResponse> {
    return new Promise((resolve, reject) => {
      sender.send(this._broadcastChannel, request);
      // Handle response to the sent request once
      receiver.once(this._responseChannel, (event, isOk: boolean, response: AwaitedResponse) => {
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
   * This should be used to receive messages that are broadcasted by the other end of
   * the ipc channel and are not responses to requests sent by this party.
   *
   * @param receiver {IpcReceiver}
   * @param handler
   */
  onReceive(handler: (request: ReceivedRequest) => Promise<Response>, receiver: IpcReceiver): void {
    receiver.on(this._broadcastChannel, async (event: IpcEvent, request: ReceivedRequest) => {
      try {
        const response = await handler(request);
        event.sender.send(this._responseChannel, true, response);
      } catch (error) {
        event.sender.send(this._responseChannel, false, error);
      }
    });
  }
}
