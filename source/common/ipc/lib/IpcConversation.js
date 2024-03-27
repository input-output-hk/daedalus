'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.IpcConversation = void 0;
const lodash_1 = require('lodash');
const uuid_1 = require('uuid');
/**
 * Provides a coherent, typed api for working with electron
 * ipc messages over named channels. Where possible it uses
 * promises to reduce the necessary boilerplate for request
 * and response cycles.
 */
class IpcConversation {
  /**
   * Each ipc channel should be a singleton (based on the channelName)
   * Here we track the created instances.
   */
  static _instances = {};
  /**
   * The channel name
   * @private
   */
  _channelName;
  constructor(channelName) {
    if (!(0, lodash_1.isString)(channelName) || channelName === '') {
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
  async request(message, sender, receiver) {
    return new Promise((resolve, reject) => {
      const conversationId = (0, uuid_1.v4)();
      const handler = (event, messageId, isOk, response) => {
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
  onRequest(handler, receiver) {
    receiver.on(this._channelName, async (event, conversationId, message) => {
      try {
        const response = await handler(message);
        event.sender.send(this._channelName, conversationId, true, response);
      } catch (error) {
        event.sender.send(this._channelName, conversationId, false, error);
      }
    });
  }
}
exports.IpcConversation = IpcConversation;
//# sourceMappingURL=IpcConversation.js.map
