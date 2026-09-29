/** @jest-environment node */
import { BrowserWindow } from 'electron';
import checkDiskSpace from 'check-disk-space';
import { handleDiskSpace } from './handleDiskSpace';
import { getDiskSpaceStatusChannel } from '../ipc/get-disk-space-status';

jest.mock('fs-extra', () => ({ realpath: async (value: string) => value }));
jest.mock('check-disk-space', () => jest.fn());
jest.mock('../ipc/get-disk-space-status', () => ({
  getDiskSpaceStatusChannel: { send: jest.fn(), onReceive: jest.fn() },
}));
jest.mock('./logging', () => ({
  logger: { info: jest.fn(), error: jest.fn() },
}));
jest.mock('../config', () => ({
  stateDirectoryPath: '/test',
  DISK_SPACE_REQUIRED: 10,
  DISK_SPACE_RECOMMENDED_PERCENTAGE: 10,
  DISK_SPACE_REQUIRED_MARGIN_PERCENTAGE: 10,
  DISK_SPACE_CHECK_TIMEOUT: 1000,
  DISK_SPACE_CHECK_LONG_INTERVAL: 10000,
}));

test('disk-space notifications resolve the recovered window without creating another poller', async () => {
  jest.useFakeTimers();
  try {
    const first = { webContents: { id: 1 } } as unknown as BrowserWindow;
    const second = { webContents: { id: 2 } } as unknown as BrowserWindow;
    let current = first;
    const check = handleDiskSpace(() => current);
    let finish: (value: { free: number; size: number }) => void;
    (checkDiskSpace as jest.Mock).mockImplementationOnce(
      () =>
        new Promise((resolve) => {
          finish = resolve;
        })
    );
    const pending = check();
    await Promise.resolve();
    current = second;
    finish({ free: 100, size: 1000 });
    await pending;
    expect(getDiskSpaceStatusChannel.send).toHaveBeenLastCalledWith(
      expect.anything(),
      second.webContents
    );
    (checkDiskSpace as jest.Mock).mockResolvedValue({ free: 100, size: 1000 });
    const receive = (getDiskSpaceStatusChannel.onReceive as jest.Mock).mock
      .calls[0][0];
    await receive();
    expect(getDiskSpaceStatusChannel.send).toHaveBeenLastCalledWith(
      expect.anything(),
      second.webContents
    );
    expect(getDiskSpaceStatusChannel.onReceive).toHaveBeenCalledTimes(1);
  } finally {
    jest.clearAllTimers();
    jest.useRealTimers();
  }
});
