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
   * The ipc channel name
   * @private
   */
  _channel: string;
  /**
   * The ipc interface that is used to send messages.
   * @private
   */
  _sender: IpcSender;
  /**
   * The ipc interfaces that is used to receive messages
   * @private
   */
  _receiver: IpcReceiver;
  /**
   * Flag that indicates if we are awaiting a reply from the receiver.
   * @private
   */
  _isAwaitingResponse: boolean;

  /**
   * Sets up the ipc channel and checks that its name is valid.
   * Ensures that only one instance per channel name can exist.
   *
   * @param channelName {String}
   * @param sender {IpcSender}
   * @param receiver {IpcReceiver}
   */
  constructor(channelName: string, sender: IpcSender, receiver: IpcReceiver) {
    if (!isString(channelName) || channelName === '') {
      throw new Error(`Invalid channel name ${channelName} provided`);
    }
    // Enforce the singleton pattern based on the channel name
    const existingChannel = IpcChannel._instances[channelName];
    if (existingChannel) return existingChannel;
    IpcChannel._instances[channelName] = this;

    this._channel = channelName;
    this._sender = sender;
    this._receiver = receiver;
    this._isAwaitingResponse = false;
  }

  /**
   * Sends a request over ipc to the receiver and waits for the next response on the
   * same channel. It returns a promise which is resolved or rejected with the response
   * depending on the `isOk` flag set by the respondant.
   *
   * @param request
   * @returns {Promise<AwaitedResponse>}
   */
  async send(request: Request): Promise<AwaitedResponse> {
    return new Promise((resolve, reject) => {
      this._sender.send(this._channel, request);
      this._isAwaitingResponse = true;
      // Handle response to the sent request once
      this._receiver.once(this._channel, (event, isOk: boolean, response: AwaitedResponse) => {
        if (isOk) {
          resolve(response);
        } else {
          reject(response);
        }
        this._isAwaitingResponse = false;
      });
    });
  }

  /**
   * Sets up a permanent handler for receiving messages on this channel.
   * This should be used to receive messages that are broadcasted by the other end of
   * the ipc channel and are not responses to requests sent by this party.
   *
   * @param handler
   */
  receive(handler: (request: ReceivedRequest) => Promise<Response>): void {
    this._receiver.on(this._channel, async (event: IpcEvent, request: ReceivedRequest) => {
      // Only handle messages if they are not yet part of a request / response cycle.
      if (this._isAwaitingResponse) return;
      try {
        const response = await handler(request);
        event.sender.send(this._channel, true, response);
      } catch (error) {
        event.sender.send(this._channel, false, error);
      }
    });
  }
}
