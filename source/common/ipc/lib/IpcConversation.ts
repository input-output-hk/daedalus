import { isString } from 'lodash';
import { v4 as uuidv4 } from 'uuid';
import type {
  IpcAuthorization,
  IpcChannelSecurity,
  IpcEvent,
  IpcReceiver,
  IpcSender,
} from './IpcChannel';

export type { IpcEvent, IpcReceiver, IpcSender } from './IpcChannel';

type ConversationEnvelope<T> = {
  conversationId: string;
  isResponse: boolean;
  isOk?: boolean;
  message: T;
};

const isEnvelope = (value: any): value is ConversationEnvelope<any> =>
  value !== null &&
  typeof value === 'object' &&
  typeof value.conversationId === 'string' &&
  value.conversationId.length > 0 &&
  typeof value.isResponse === 'boolean' &&
  Object.prototype.hasOwnProperty.call(value, 'message');

export class IpcConversation<Incoming, Outgoing> {
  static _instances = {};
  _channelName: string;
  _security: IpcChannelSecurity;
  _registration?: {
    receiver: IpcReceiver;
    listener: (...args: Array<any>) => void;
  };

  constructor(channelName: string, security: IpcChannelSecurity = {}) {
    if (!isString(channelName) || channelName === '') {
      throw new Error(`Invalid channel name ${channelName} provided`);
    }
    const existingChannel = IpcConversation._instances[channelName];
    if (existingChannel)
      throw new Error(`IPC channel "${channelName}" already exists.`);
    IpcConversation._instances[channelName] = this;
    this._channelName = channelName;
    this._security = security;
  }

  request(
    message: Outgoing,
    sender: IpcSender,
    receiver: IpcReceiver
  ): Promise<Incoming> {
    return new Promise((resolve, reject) => {
      const conversationId = uuidv4();
      let settled = false;
      let unsubscribe = () => {};
      const settle = (isOk: boolean, response: Incoming) => {
        if (settled) return;
        settled = true;
        receiver.removeListener(this._channelName, listener);
        unsubscribe();
        if (isOk) resolve(response);
        else reject(response);
      };
      const listener = (
        event: IpcEvent,
        envelope: ConversationEnvelope<Incoming>
      ) => {
        if (
          !isEnvelope(envelope) ||
          !envelope.isResponse ||
          envelope.conversationId !== conversationId ||
          typeof envelope.isOk !== 'boolean' ||
          (this._security.authorizeResponse &&
            !this._security.authorizeResponse(event))
        )
          return;
        settle(envelope.isOk, envelope.message);
      };
      receiver.on(this._channelName, listener);
      unsubscribe =
        this._security.onOutgoingInvalidated?.(() =>
          settle(false, new Error('IPC request cancelled') as Incoming)
        ) || unsubscribe;
      if (settled) return;
      try {
        sender.send(this._channelName, {
          conversationId,
          isResponse: false,
          message,
        });
      } catch (error) {
        settle(false, error as Incoming);
      }
    });
  }

  onRequest(
    handler: (message: Incoming, event?: IpcEvent) => Promise<Outgoing>,
    receiver: IpcReceiver
  ): void {
    if (this._registration)
      this._registration.receiver.removeListener(
        this._channelName,
        this._registration.listener
      );
    const listener = async (
      event: IpcEvent,
      envelope: ConversationEnvelope<Incoming>
    ) => {
      if (!isEnvelope(envelope) || envelope.isResponse) return;
      const authorization: IpcAuthorization | null = this._security.authorize
        ? this._security.authorize(event)
        : { isCurrent: () => true };
      if (!authorization) return;
      let finished = false;
      const unsubscribe =
        authorization.onInvalidated?.(() => {
          if (finished || !event.reply) return;
          finished = true;
          try {
            event.reply(this._channelName, {
              conversationId: envelope.conversationId,
              isResponse: true,
              isOk: false,
              message: new Error('IPC request cancelled'),
            });
          } catch (_error) {
            // The caller frame may already be detached; cleanup still completes.
          }
        }) || (() => {});
      const reply = (isOk: boolean, message: unknown) => {
        if (finished || !authorization.isCurrent()) return;
        finished = true;
        unsubscribe();
        const response = {
          conversationId: envelope.conversationId,
          isResponse: true,
          isOk,
          message,
        };
        if (event.reply) event.reply(this._channelName, response);
        else event.sender.send(this._channelName, response);
      };
      try {
        reply(true, await handler(envelope.message, event));
      } catch (error) {
        reply(false, error);
      }
    };
    receiver.on(this._channelName, listener);
    this._registration = { receiver, listener };
  }
}
