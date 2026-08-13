import { EventEmitter } from 'events';
import { IpcChannel } from './IpcChannel';

class Receiver extends EventEmitter {
  on(channel: string, listener: (...args: Array<any>) => void): this {
    return super.on(channel, listener);
  }
}

describe('IpcChannel', () => {
  beforeEach(() => {
    IpcChannel._instances = {};
  });

  it('registers before send and correlates concurrent responses', async () => {
    const receiver = new Receiver();
    const sent: Array<any> = [];
    const sender = {
      send: (channel: string, envelope: any) => {
        expect(receiver.listenerCount('test-response')).toBeGreaterThan(0);
        sent.push({ channel, envelope });
      },
    };
    const channel = new IpcChannel<string, string>('test');

    const first = channel.request('first', sender, receiver);
    const second = channel.request('second', sender, receiver);
    receiver.emit(
      'test-response',
      { sender },
      {
        requestId: sent[1].envelope.requestId,
        isOk: true,
        response: 'second-result',
      }
    );
    receiver.emit(
      'test-response',
      { sender },
      {
        requestId: sent[0].envelope.requestId,
        isOk: true,
        response: 'first-result',
      }
    );

    await expect(first).resolves.toBe('first-result');
    await expect(second).resolves.toBe('second-result');
    expect(receiver.listenerCount('test-response')).toBe(0);
  });

  it('ignores unauthenticated responses and cleans up on invalidation', async () => {
    const receiver = new Receiver();
    let invalidate: () => void = () => {};
    const channel = new IpcChannel<string, string>('test', {
      authorizeResponse: (event) => event.sender === trustedSender,
      onOutgoingInvalidated: (listener) => {
        invalidate = listener;
        return () => {
          invalidate = () => {};
        };
      },
    });
    const trustedSender = { send: jest.fn() };
    const promise = channel.request('request', trustedSender, receiver);
    const envelope = trustedSender.send.mock.calls[0][1];
    receiver.emit(
      'test-response',
      { sender: { send: jest.fn() } },
      {
        requestId: envelope.requestId,
        isOk: true,
        response: 'spoofed',
      }
    );
    invalidate();

    await expect(promise).rejects.toThrow('IPC request cancelled');
    expect(receiver.listenerCount('test-response')).toBe(0);
  });

  it('does not send after synchronous outgoing invalidation', async () => {
    const receiver = new Receiver();
    const sender = { send: jest.fn() };
    const channel = new IpcChannel<string, string>('test', {
      onOutgoingInvalidated: (listener) => {
        listener();
        return jest.fn();
      },
    });

    await expect(channel.request('request', sender, receiver)).rejects.toThrow(
      'IPC request cancelled'
    );
    expect(sender.send).not.toHaveBeenCalled();
  });

  it('authenticates before handling and replies to the caller', async () => {
    const receiver = new Receiver();
    const reply = jest.fn();
    const trustedSender = { send: jest.fn() };
    const handler = jest.fn(async (message) => `${message}-result`);
    const channel = new IpcChannel<string, string>('test', {
      authorize: (event) =>
        event.sender === trustedSender ? { isCurrent: () => true } : null,
    });
    channel.onRequest(handler, receiver);

    receiver.emit(
      'test-request',
      { sender: { send: jest.fn() }, reply },
      {
        requestId: 'wrong',
        message: 'blocked',
      }
    );
    receiver.emit(
      'test-request',
      { sender: trustedSender, reply },
      {
        requestId: 'right',
        message: 'allowed',
      }
    );
    await Promise.resolve();

    expect(handler).toHaveBeenCalledTimes(1);
    expect(handler).toHaveBeenCalledWith('allowed', expect.anything());
    expect(reply).toHaveBeenCalledWith('test-response', {
      requestId: 'right',
      isOk: true,
      response: 'allowed-result',
    });
  });

  it('replaces an existing registration instead of duplicating handlers', async () => {
    const receiver = new Receiver();
    const channel = new IpcChannel<string, string>('test');
    const first = jest.fn(async () => 'first');
    const second = jest.fn(async () => 'second');
    channel.onRequest(first, receiver);
    channel.onRequest(second, receiver);

    receiver.emit(
      'test-request',
      { sender: { send: jest.fn() } },
      {
        requestId: 'request',
        message: 'value',
      }
    );
    await Promise.resolve();

    expect(first).not.toHaveBeenCalled();
    expect(second).toHaveBeenCalledTimes(1);
    expect(receiver.listenerCount('test-request')).toBe(1);
  });

  it('contains detached-frame reply failures during invalidation', () => {
    const receiver = new Receiver();
    let invalidate = () => {};
    const channel = new IpcChannel<string, string>('test', {
      authorize: () => ({
        isCurrent: () => false,
        onInvalidated: (listener) => {
          invalidate = listener;
          return jest.fn();
        },
      }),
    });
    channel.onRequest(() => new Promise(() => {}), receiver);
    receiver.emit(
      'test-request',
      {
        sender: { send: jest.fn() },
        reply: () => {
          throw new Error('detached');
        },
      },
      {
        requestId: 'request',
        message: 'value',
      }
    );

    expect(() => invalidate()).not.toThrow();
  });
});
