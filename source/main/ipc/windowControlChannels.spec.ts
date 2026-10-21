import { ipcMain } from 'electron';
import { handleWindowControlRequests } from './windowControlChannels';

jest.mock('electron', () => {
  const { EventEmitter } = jest.requireActual('events');
  return { ipcMain: new EventEmitter() };
});

const mockIpcMain = ipcMain as any;
const mockAuthority = {
  trusted: true,
  current: true,
};

jest.mock('./lib/trustedRendererIpcAuthority', () => ({
  authorizeTrustedRenderer: jest.fn(() =>
    mockAuthority.trusted
      ? {
          isCurrent: () => mockAuthority.current,
          onInvalidated: () => () => undefined,
        }
      : null
  ),
  isTrustedRendererEvent: jest.fn(() => mockAuthority.trusted),
  onTrustedRendererInvalidated: jest.fn(() => () => undefined),
}));

const createWindow = () => ({
  isDestroyed: () => false,
  setSize: jest.fn(),
  close: jest.fn(),
});

const emit = async (channel: string, message: unknown, reply = jest.fn()) => {
  mockIpcMain.emit(
    channel,
    { sender: { send: jest.fn() }, reply },
    { requestId: 'request', message }
  );
  await Promise.resolve();
  await Promise.resolve();
  return reply;
};

describe('windowControlChannels', () => {
  beforeEach(() => {
    jest.useFakeTimers();
    mockIpcMain.removeAllListeners();
    mockAuthority.trusted = true;
    mockAuthority.current = true;
  });

  afterEach(() => jest.useRealTimers());

  it('uses real wrapper registration and rebinds to the latest window', async () => {
    const first = createWindow();
    const second = createWindow();
    handleWindowControlRequests(first as any);
    handleWindowControlRequests(second as any);

    const reply = await emit('resize-window-broadcast', {
      width: 800,
      height: 600,
      animate: true,
    });

    expect(mockIpcMain.listenerCount('resize-window-broadcast')).toBe(1);
    expect(first.setSize).not.toHaveBeenCalled();
    expect(second.setSize).toHaveBeenCalledWith(800, 600, true);
    expect(reply).toHaveBeenCalledWith('resize-window-response', {
      requestId: 'request',
      isOk: true,
      response: undefined,
    });
  });

  it('rejects unauthenticated traffic before the window side effect', async () => {
    const window = createWindow();
    handleWindowControlRequests(window as any);
    mockAuthority.trusted = false;

    const reply = await emit('resize-window-broadcast', {
      width: 800,
      height: 600,
      animate: true,
    });

    expect(window.setSize).not.toHaveBeenCalled();
    expect(reply).not.toHaveBeenCalled();
  });

  it('emits the correlated close response before deferred teardown', async () => {
    const window = createWindow();
    handleWindowControlRequests(window as any);

    const reply = await emit('close-window-broadcast', undefined);
    expect(reply).toHaveBeenCalledWith('close-window-response', {
      requestId: 'request',
      isOk: true,
      response: undefined,
    });
    expect(window.close).not.toHaveBeenCalled();
    jest.runAllTimers();
    expect(window.close).toHaveBeenCalledTimes(1);
  });
});
