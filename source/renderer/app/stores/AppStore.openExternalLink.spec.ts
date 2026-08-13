import { openExternalUrlChannel } from '../ipc/open-external-url';
import AppStore from './AppStore';

jest.mock('../ipc/open-external-url', () => ({
  openExternalUrlChannel: { send: jest.fn() },
}));

const send = openExternalUrlChannel.send as jest.Mock;

describe('AppStore.openExternalLink', () => {
  it('consumes rejection from its fire-and-forget IPC call', async () => {
    send.mockRejectedValue(new Error('private shell failure'));
    const event = { preventDefault: jest.fn() };
    const unhandled = jest.fn();
    process.on('unhandledRejection', unhandled);

    AppStore.prototype.openExternalLink.call(
      {} as AppStore,
      'https://example.test/',
      event as any
    );
    await Promise.resolve();
    await Promise.resolve();

    expect(event.preventDefault).toHaveBeenCalledTimes(1);
    expect(send).toHaveBeenCalledWith('https://example.test/');
    expect(unhandled).not.toHaveBeenCalled();
    process.removeListener('unhandledRejection', unhandled);
  });
});
