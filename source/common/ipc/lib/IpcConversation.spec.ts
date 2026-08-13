import { EventEmitter } from 'events';
import { IpcConversation } from './IpcConversation';

class Receiver extends EventEmitter {}

describe('IpcConversation', () => {
  beforeEach(() => {
    IpcConversation._instances = {};
  });

  it('correlates responses and installs the listener before send', async () => {
    const receiver = new Receiver();
    let sent: any;
    const sender = {
      send: (channel: string, envelope: any) => {
        expect(receiver.listenerCount(channel)).toBe(1);
        sent = envelope;
      },
    };
    const conversation = new IpcConversation<string, string>('conversation');
    const request = conversation.request('message', sender, receiver);
    receiver.emit(
      'conversation',
      { sender },
      {
        conversationId: sent.conversationId,
        isResponse: true,
        isOk: true,
        message: 'response',
      }
    );

    await expect(request).resolves.toBe('response');
    expect(receiver.listenerCount('conversation')).toBe(0);
  });

  it('replaces request registration and preserves caller reply targeting', async () => {
    const receiver = new Receiver();
    const reply = jest.fn();
    const conversation = new IpcConversation<string, string>('conversation');
    const first = jest.fn(async () => 'first');
    const second = jest.fn(async () => 'second');
    conversation.onRequest(first, receiver);
    conversation.onRequest(second, receiver);
    receiver.emit(
      'conversation',
      { sender: { send: jest.fn() }, reply },
      {
        conversationId: 'id',
        isResponse: false,
        message: 'message',
      }
    );
    await Promise.resolve();

    expect(first).not.toHaveBeenCalled();
    expect(second).toHaveBeenCalledTimes(1);
    expect(reply).toHaveBeenCalledWith('conversation', {
      conversationId: 'id',
      isResponse: true,
      isOk: true,
      message: 'second',
    });
  });
});
