/**
 * @jest-environment node
 */
import { backendLifecycle } from '../BackendLifecycle';
import { handleChainStorageRequests } from './chainStorageChannel';
import { CONFIRM_CHAIN_STORAGE_CHANNEL } from '../../common/ipc/api';

const mockHandlers: Record<string, (request: any) => Promise<unknown>> = {};

jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn().mockImplementation((name: string) => ({
    onRequest: (handler: (request: any) => Promise<unknown>) => {
      mockHandlers[name] = handler;
    },
  })),
}));

jest.mock('../BackendLifecycle', () => ({
  backendLifecycle: {
    setCustomChainPath: jest.fn(() => Promise.resolve()),
    sendMithrilCommand: jest.fn(),
  },
}));

jest.mock('./electronStoreConversation', () => ({
  requestElectronStore: jest.fn(),
}));

jest.mock('../config', () => ({
  networkMagic: 1,
  stateDirectoryPath: '/state',
}));

jest.mock('../utils/chainStorageValidate', () => ({
  validatePath: jest.fn(),
}));

jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
    warn: jest.fn(),
  },
}));

describe('handleChainStorageRequests', () => {
  const setCustomChainPath = backendLifecycle.setCustomChainPath as jest.Mock;
  const sendMithrilCommand = backendLifecycle.sendMithrilCommand as jest.Mock;
  const confirm = (request: { customPath: string | null; rebuild?: boolean }) =>
    mockHandlers[CONFIRM_CHAIN_STORAGE_CHANNEL](request);

  beforeAll(() => {
    handleChainStorageRequests();
  });

  beforeEach(() => {
    setCustomChainPath.mockClear();
    sendMithrilCommand.mockClear();
  });

  it('applies a confirmed location and sends nothing else', async () => {
    await confirm({ customPath: '/new' });
    expect(setCustomChainPath).toHaveBeenCalledWith('/new');
    expect(sendMithrilCommand).not.toHaveBeenCalled();
  });

  it('sends the rebuild straight after a location confirmed together with it', async () => {
    await confirm({ customPath: '/new', rebuild: true });
    expect(setCustomChainPath).toHaveBeenCalledWith('/new');
    expect(sendMithrilCommand).toHaveBeenCalledWith({
      cmd: 'start_mithril',
      wipe_chain: true,
    });
    expect(setCustomChainPath.mock.invocationCallOrder[0]).toBeLessThan(
      sendMithrilCommand.mock.invocationCallOrder[0]
    );
  });

  it('rebuilds in the default location when that is the one confirmed', async () => {
    await confirm({ customPath: null, rebuild: true });
    expect(setCustomChainPath).toHaveBeenCalledWith(null);
    expect(sendMithrilCommand).toHaveBeenCalledTimes(1);
  });
});
