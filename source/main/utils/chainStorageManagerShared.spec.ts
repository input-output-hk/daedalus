import fs from 'fs-extra';
import { captureChainPathState } from './chainStorageManagerShared';

jest.mock('fs-extra', () => ({
  readlink: jest.fn(),
  realpath: jest.fn(),
}));

jest.mock('./logging', () => ({
  logger: {
    warn: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
  },
}));

describe('captureChainPathState', () => {
  const originalPlatform = process.platform;

  beforeEach(() => {
    jest.clearAllMocks();
    Object.defineProperty(process, 'platform', {
      value: originalPlatform,
      configurable: true,
    });
  });

  afterEach(() => {
    Object.defineProperty(process, 'platform', {
      value: originalPlatform,
      configurable: true,
    });
  });

  it('classifies Windows junction-backed chain paths as symlinks for layout detection', async () => {
    Object.defineProperty(process, 'platform', {
      value: 'win32',
      configurable: true,
    });
    (fs.readlink as jest.Mock).mockResolvedValue('/custom-parent/chain');
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(
      '/custom-parent/chain'
    );

    const result = await captureChainPathState({
      _chainPath: '/state/chain',
      _safeLstat: jest.fn().mockResolvedValue({
        isSymbolicLink: () => false,
        isDirectory: () => true,
      }),
    } as never);

    expect(result).toEqual({
      type: 'symlink',
      linkTargetPath: '/custom-parent/chain',
      resolvedPath: '/custom-parent/chain',
    });
  });
});
