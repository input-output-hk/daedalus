import { isString } from 'lodash';

export type IpcSender = {
  send: (channel: string, ...args: Array<any>) => void;
};
export type IpcEvent = {
  sender: IpcSender;
};
export type IpcReceiver = {
  on: (
    channel: string,
    arg1: (event: IpcEvent, ...args: Array<any>) => Promise<any>
  ) => void;
  once: (
    channel: string,
    arg1: (event: IpcEvent, isOk: boolean, ...args: Array<any>) => void
  ) => void;
};

/**
 * Provides a coherent, typed api for working with electron
 * ipc messages over named channels. Where possible it uses
 * promises to reduce the necessary boilerplate for request
 * and response cycles.
 */
export class IpcChannel<Incoming, Outgoing> {
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
   * The public request channel (any process will receive these messages)
   * @private
   */
  _requestChannel: string;

  /**
   * The response channel between a main and render process
   * @private
   */
  _responseChannel: string;

  /**
   * Sets up the ipc channel and checks that its name is valid.
   * Ensures that only one instance per channel name can exist.
   */
  constructor(channelName: string) {
    if (!isString(channelName) || channelName === '') {
      throw new Error(`Invalid channel name ${channelName} provided`);
    }

    // Enforce the singleton pattern based on the channel name
    const existingChannel = IpcChannel._instances[channelName];
    if (existingChannel) return existingChannel;
    IpcChannel._instances[channelName] = this;
    this._broadcastChannel = `${channelName}-broadcast`;
    this._requestChannel = `${channelName}-request`;
    this._responseChannel = `${channelName}-response`;
  }

  /**
   * Sends a request over ipc to the receiver and waits for the next response on the
   * same channel. It returns a promise which is resolved or rejected with the response
   * depending on the `isOk` flag set by the respondent.
   */
  async send(
    message: Outgoing,
    sender: IpcSender,
    receiver: IpcReceiver
  ): Promise<Incoming> {
    return new Promise((resolve, reject) => {
      sender.send(this._broadcastChannel, message);
      // Handle response to the sent request once
      receiver.once(
        this._responseChannel,
        (event, isOk: boolean, response: Incoming) => {
          if (isOk) {
            resolve(response);
          } else {
            reject(response);
          }
        }
      );
    });
  }

  /**
   * Request a message from the other side.
   * Can be used to get the current state of some information.
   */
  async request(
    message: Outgoing,
    sender: IpcSender,
    receiver: IpcReceiver
  ): Promise<Incoming> {
    return new Promise((resolve, reject) => {
      sender.send(this._requestChannel, message);
      // Handle response to the sent request once
      receiver.once(
        this._responseChannel,
        (event, isOk: boolean, response: Incoming) => {
          if (isOk) {
            resolve(response);
          } else {
            reject(response);
          }
        }
      );
    });
  }

  /**
   * Sets up a permanent handler for receiving messages on this channel.
   * This should be used to receive messages that are broadcasted by the other end of
   * the ipc channel and are not responses to requests sent by this party.
   */
  onReceive(
    handler: (message: Incoming) => Promise<Outgoing>,
    receiver: IpcReceiver
  ): void {
    receiver.on(
      this._broadcastChannel,
      async (event: IpcEvent, message: Incoming) => {
        try {
          const response = await handler(message);
          event.sender.send(this._responseChannel, true, response);
        } catch (error) {
          event.sender.send(this._responseChannel, false, error);
        }
      }
    );
  }

  /**
   * Sets up a permanent handler for receiving request from the other side.
   */
  onRequest(
    handler: (arg0: Incoming) => Promise<Outgoing>,
    receiver: IpcReceiver
  ): void {
    receiver.on(
      this._requestChannel,
      async (event: IpcEvent, message: Incoming) => {
        try {
          const response = await handler(message);
          event.sender.send(this._responseChannel, true, response);
        } catch (error) {
          event.sender.send(this._responseChannel, false, error);
        }
      }
    );
  }
}
