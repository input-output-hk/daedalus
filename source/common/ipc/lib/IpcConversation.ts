import { isString } from 'lodash';
import uuid from 'uuid';

export type IpcSender = {
  send: (channel: string, conversationId: string, ...args: Array<any>) => void;
};
export type IpcEvent = {
  sender: IpcSender;
};
export type IpcReceiver = {
  on: (
    channel: string,
    arg1: (
      event: IpcEvent,
      conversationId: string,
      ...args: Array<any>
    ) => Promise<any> | void
  ) => void;
  removeListener: (
    channel: string,
    listener: (...args: Array<any>) => void
  ) => void;
};

/**
 * Provides a coherent, typed api for working with electron
 * ipc messages over named channels. Where possible it uses
 * promises to reduce the necessary boilerplate for request
 * and response cycles.
 */
export class IpcConversation<Incoming, Outgoing> {
  /**
   * Each ipc channel should be a singleton (based on the channelName)
   * Here we track the created instances.
   */
  static _instances = {};

  /**
   * The channel name
   * @private
   */
  _channelName: string;

  constructor(channelName: string) {
    if (!isString(channelName) || channelName === '') {
      throw new Error(`Invalid channel name ${channelName} provided`);
    }

    // Enforce the singleton pattern based on the channel name
    const existingChannel = IpcConversation._instances[channelName];

    if (existingChannel) {
      throw new Error(`IPC channel "${channelName}" already exists.`);
    }

    IpcConversation._instances[channelName] = this;
    this._channelName = channelName;
  }

  /**
   * Sends a request over ipc to the receiver and waits for the next response on the
   * same channel. It returns a promise which is resolved or rejected with the response
   * depending on the `isOk` flag set by the respondent.
   */
  async request(
    message: Outgoing,
    sender: IpcSender,
    receiver: IpcReceiver
  ): Promise<Incoming> {
    return new Promise((resolve, reject) => {
      const conversationId = uuid();

      const handler = (
        event,
        messageId: string,
        isOk: boolean,
        response: Incoming
      ) => {
        // Only handle messages with matching conversation id!
        if (messageId !== conversationId) return;

        // Simulate promise rejection over IPC (since it's not possible to throw over IPC)
        if (isOk) {
          resolve(response);
        } else {
          reject(response);
        }

        // Cleanup the lister once the request cycle is finished
        receiver.removeListener(this._channelName, handler);
      };

      receiver.on(this._channelName, handler);
      sender.send(this._channelName, conversationId, message);
    });
  }

  /**
   * Sets up a permanent handler for receiving and responding to requests
   * from the other side.
   */
  onRequest(
    handler: (arg0: Incoming) => Promise<Outgoing>,
    receiver: IpcReceiver
  ): void {
    receiver.on(
      this._channelName,
      async (event: IpcEvent, conversationId: string, message: Incoming) => {
        try {
          const response = await handler(message);
          event.sender.send(this._channelName, conversationId, true, response);
        } catch (error) {
          event.sender.send(this._channelName, conversationId, false, error);
        }
      }
    );
  }
}
