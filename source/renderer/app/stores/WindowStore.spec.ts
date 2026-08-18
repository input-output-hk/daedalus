import WindowStore from './WindowStore';
import {
  closeWindowChannel,
  resizeWindowChannel,
} from '../ipc/windowControlChannels';

jest.mock('../ipc/windowControlChannels', () => ({
  closeWindowChannel: { send: jest.fn() },
  resizeWindowChannel: { send: jest.fn() },
}));

const closeSend = closeWindowChannel.send as jest.Mock;
const resizeSend = resizeWindowChannel.send as jest.Mock;

describe('WindowStore window controls', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    closeSend.mockReturnValue({ catch: jest.fn() });
    resizeSend.mockReturnValue({ catch: jest.fn() });
  });

  it('keeps close synchronous and attaches terminal rejection ownership', () => {
    const store = new WindowStore(null as any, null as any, null as any);

    expect(store.closeWindow()).toBeUndefined();
    expect(closeSend).toHaveBeenCalledWith();
    expect(closeSend.mock.results[0].value.catch).toHaveBeenCalledTimes(1);
  });

  it('preserves resize payload and test animation behavior', () => {
    const store = new WindowStore(null as any, null as any, null as any);
    store._isTest = true;

    expect(store._resizeWindow({ width: 800, height: 600 })).toBeUndefined();
    expect(resizeSend).toHaveBeenCalledWith({
      width: 800,
      height: 600,
      animate: false,
    });
    expect(resizeSend.mock.results[0].value.catch).toHaveBeenCalledTimes(1);
  });
});
