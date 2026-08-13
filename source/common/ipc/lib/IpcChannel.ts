import { isString } from 'lodash';
import { v4 as uuidv4 } from 'uuid';

export type IpcSender = {
  send: (channel: string, ...args: Array<any>) => void;
  webContents?: IpcSender;
};
export type IpcEvent = {
  sender: IpcSender;
  reply?: (channel: string, ...args: Array<any>) => void;
};
export type IpcReceiver = {
  on: (channel: string, listener: (...args: Array<any>) => void) => void;
  removeListener: (
    channel: string,
    listener: (...args: Array<any>) => void
  ) => void;
};

type RequestEnvelope<T> = { requestId: string; message: T };
type ResponseEnvelope<T> = {
  requestId: string;
  isOk: boolean;
  response: T;
};

export type IpcAuthorization = {
  isCurrent: () => boolean;
  onInvalidated?: (listener: () => void) => () => void;
};

export type IpcChannelSecurity = {
  authorize?: (event: IpcEvent) => IpcAuthorization | null;
  authorizeResponse?: (event: IpcEvent) => boolean;
  onOutgoingInvalidated?: (listener: () => void) => () => void;
};

export interface Channel<Incoming, Outgoing> {
  send(
    message: Outgoing,
    sender: IpcSender,
    receiver?: IpcReceiver
  ): Promise<Incoming>;
  request(
    message: Outgoing,
    sender: IpcSender,
    receiver?: IpcReceiver
  ): Promise<Incoming>;
  onReceive(
    handler: (message: Incoming, event?: IpcEvent) => Promise<Outgoing>,
    receiver: IpcReceiver
  ): void;
  onRequest(
    handler: (message: Incoming, event?: IpcEvent) => Promise<Outgoing>,
    receiver?: IpcReceiver
  ): void;
}

const isEnvelope = (value: any): value is RequestEnvelope<any> =>
  value !== null &&
  typeof value === 'object' &&
  typeof value.requestId === 'string' &&
  value.requestId.length > 0 &&
  Object.prototype.hasOwnProperty.call(value, 'message');

const isResponseEnvelope = (value: any): value is ResponseEnvelope<any> =>
  value !== null &&
  typeof value === 'object' &&
  typeof value.requestId === 'string' &&
  typeof value.isOk === 'boolean' &&
  Object.prototype.hasOwnProperty.call(value, 'response');

export class IpcChannel<Incoming, Outgoing>
  implements Channel<Incoming, Outgoing> {
  static _instances = {};

  _broadcastChannel: string;
  _requestChannel: string;
  _responseChannel: string;
  _security: IpcChannelSecurity;
  _registrations = new Map<
    string,
    { receiver: IpcReceiver; listener: (...args: Array<any>) => void }
  >();

  constructor(channelName: string, security: IpcChannelSecurity = {}) {
    if (!isString(channelName) || channelName === '') {
      throw new Error(`Invalid channel name ${channelName} provided`);
    }
    const existingChannel = IpcChannel._instances[channelName];
    if (existingChannel)
      throw new Error(`Channel ${channelName} already exists`);
    IpcChannel._instances[channelName] = this;
    this._broadcastChannel = `${channelName}-broadcast`;
    this._requestChannel = `${channelName}-request`;
    this._responseChannel = `${channelName}-response`;
    this._security = security;
  }

  async send(
    message: Outgoing,
    sender: IpcSender,
    receiver?: IpcReceiver
  ): Promise<Incoming> {
    if (!receiver) throw new Error('IPC receiver is required');
    return this._send(this._broadcastChannel, message, sender, receiver);
  }

  async request(
    message: Outgoing,
    sender: IpcSender,
    receiver?: IpcReceiver
  ): Promise<Incoming> {
    if (!receiver) throw new Error('IPC receiver is required');
    return this._send(this._requestChannel, message, sender, receiver);
  }

  _send(
    channel: string,
    message: Outgoing,
    sender: IpcSender,
    receiver: IpcReceiver
  ): Promise<Incoming> {
    return new Promise((resolve, reject) => {
      const requestId = uuidv4();
      let settled = false;
      let unsubscribe = () => {};
      const settle = (isOk: boolean, response: Incoming) => {
        if (settled) return;
        settled = true;
        receiver.removeListener(this._responseChannel, listener);
        unsubscribe();
        if (isOk) resolve(response);
        else reject(response);
      };
      const listener = (
        event: IpcEvent,
        envelope: ResponseEnvelope<Incoming>
      ) => {
        if (
          !isResponseEnvelope(envelope) ||
          envelope.requestId !== requestId ||
          (this._security.authorizeResponse &&
            !this._security.authorizeResponse(event))
        )
          return;
        settle(envelope.isOk, envelope.response);
      };

      receiver.on(this._responseChannel, listener);
      unsubscribe =
        this._security.onOutgoingInvalidated?.(() =>
          settle(false, new Error('IPC request cancelled') as Incoming)
        ) || unsubscribe;
      if (settled) return;
      try {
        sender.send(channel, { requestId, message });
      } catch (error) {
        settle(false, error as Incoming);
      }
    });
  }

  onReceive(
    handler: (message: Incoming, event?: IpcEvent) => Promise<Outgoing>,
    receiver: IpcReceiver
  ): void {
    this._register(this._broadcastChannel, handler, receiver);
  }

  onRequest(
    handler: (message: Incoming, event?: IpcEvent) => Promise<Outgoing>,
    receiver?: IpcReceiver
  ): void {
    if (!receiver) throw new Error('IPC receiver is required');
    this._register(this._requestChannel, handler, receiver);
  }

  _register(
    channel: string,
    handler: (message: Incoming, event?: IpcEvent) => Promise<Outgoing>,
    receiver: IpcReceiver
  ): void {
    const previous = this._registrations.get(channel);
    if (previous) previous.receiver.removeListener(channel, previous.listener);

    const listener = async (
      event: IpcEvent,
      envelope: RequestEnvelope<Incoming>
    ) => {
      if (!isEnvelope(envelope)) return;
      const authorization = this._security.authorize
        ? this._security.authorize(event)
        : { isCurrent: () => true };
      if (!authorization) return;

      let finished = false;
      const unsubscribe =
        authorization.onInvalidated?.(() => {
          if (finished || !event.reply) return;
          finished = true;
          try {
            event.reply(this._responseChannel, {
              requestId: envelope.requestId,
              isOk: false,
              response: new Error('IPC request cancelled'),
            });
          } catch (_error) {
            // The caller frame may already be detached; cleanup still completes.
          }
        }) || (() => {});
      const reply = (isOk: boolean, response: unknown) => {
        if (finished || !authorization.isCurrent()) return;
        finished = true;
        unsubscribe();
        const responseEnvelope: ResponseEnvelope<unknown> = {
          requestId: envelope.requestId,
          isOk,
          response,
        };
        if (event.reply) event.reply(this._responseChannel, responseEnvelope);
        else event.sender.send(this._responseChannel, responseEnvelope);
      };
      try {
        reply(true, await handler(envelope.message, event));
      } catch (error) {
        reply(false, error);
      }
    };
    receiver.on(channel, listener);
    this._registrations.set(channel, { receiver, listener });
  }
}
