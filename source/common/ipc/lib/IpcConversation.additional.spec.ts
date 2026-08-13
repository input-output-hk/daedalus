import { EventEmitter } from 'events';
import { IpcConversation } from './IpcConversation';

class Receiver extends EventEmitter {}

describe('IpcConversation failures', () => {
  beforeEach(() => {
    IpcConversation._instances = {};
  });

  it('ignores malformed and wrong-id responses until the match arrives', async () => {
    const receiver = new Receiver();
    const sender = { send: jest.fn() };
    const conversation = new IpcConversation<string, string>('conversation');
    const promise = conversation.request('message', sender, receiver);
    const id = sender.send.mock.calls[0][1].conversationId;
    receiver.emit('conversation', { sender }, { bad: true });
    receiver.emit(
      'conversation',
      { sender },
      {
        conversationId: 'wrong',
        isResponse: true,
        isOk: true,
        message: 'wrong',
      }
    );
    receiver.emit(
      'conversation',
      { sender },
      {
        conversationId: id,
        isResponse: true,
        isOk: false,
        message: 'failure',
      }
    );

    await expect(promise).rejects.toBe('failure');
    expect(receiver.listenerCount('conversation')).toBe(0);
  });

  it('cleans up when send throws', async () => {
    const receiver = new Receiver();
    const conversation = new IpcConversation<string, string>('conversation');
    const error = new Error('send failed');
    const promise = conversation.request(
      'message',
      {
        send: () => {
          throw error;
        },
      },
      receiver
    );

    await expect(promise).rejects.toBe(error);
    expect(receiver.listenerCount('conversation')).toBe(0);
  });

  it('does not send after synchronous outgoing invalidation', async () => {
    const receiver = new Receiver();
    const sender = { send: jest.fn() };
    const conversation = new IpcConversation<string, string>('conversation', {
      onOutgoingInvalidated: (listener) => {
        listener();
        return jest.fn();
      },
    });

    await expect(
      conversation.request('message', sender, receiver)
    ).rejects.toThrow('IPC request cancelled');
    expect(sender.send).not.toHaveBeenCalled();
  });
});
