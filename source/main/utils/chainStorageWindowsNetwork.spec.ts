import fs from 'fs-extra';
import { resolveMithrilWorkDir } from './chainStoragePathResolver';

jest.mock('fs-extra', () => ({
  lstat: jest.fn(),
  readlink: jest.fn(),
  realpath: jest.fn(),
  pathExists: jest.fn(),
}));

jest.mock('./logging', () => ({
  logger: {
    warn: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
  },
}));

describe('Windows mapped-drive chain storage', () => {
  beforeEach(() => jest.resetAllMocks());

  it('preserves the Z: junction target when realpath temporarily fails', async () => {
    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => true,
      isDirectory: () => false,
    });

    (fs.readlink as jest.Mock).mockResolvedValue('Z:\\DaedalusChain\\chain');

    ((fs.realpath as unknown) as jest.Mock).mockRejectedValue(
      Object.assign(new Error('mapped drive temporarily unavailable'), {
        code: 'ENOENT',
      })
    );

    const result = await resolveMithrilWorkDir(
      'C:\\Users\\test\\AppData\\Roaming\\Daedalus Mainnet'
    );

    expect(result).toBe('Z:\\DaedalusChain\\chain');
  });
});
