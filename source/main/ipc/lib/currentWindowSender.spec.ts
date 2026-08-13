import { createCurrentWindowSender } from './currentWindowSender';

describe('currentWindowSender', () => {
  it('routes existing callbacks to the latest bound window', () => {
    const first = {
      isDestroyed: () => false,
      webContents: { send: jest.fn() },
    } as any;
    const second = {
      isDestroyed: () => false,
      webContents: { send: jest.fn() },
    } as any;
    const current = createCurrentWindowSender();

    current.bind(first);
    const callbackSender = current.sender;
    current.bind(second);
    callbackSender.send('channel', 'message');

    expect(first.webContents.send).not.toHaveBeenCalled();
    expect(second.webContents.send).toHaveBeenCalledWith('channel', 'message');
  });
});
