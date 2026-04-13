import fs from 'fs-extra';
import { MithrilBootstrapService } from './MithrilBootstrapService';

jest.mock('fs-extra', () => ({
  pathExists: jest.fn(),
  realpath: jest.fn(),
  readdir: jest.fn(),
  move: jest.fn(),
  remove: jest.fn(),
  lstat: jest.fn(),
  emptyDir: jest.fn(),
}));

jest.mock('../config', () => ({
  stateDirectoryPath: '/tmp/state',
}));

jest.mock('../environment', () => ({
  environment: {
    network: 'mainnet',
    nodeVersion: '1.35.0',
  },
}));

jest.mock('../utils/logging', () => ({
  logger: {
    warn: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
  },
}));

describe('MithrilBootstrapService _installSnapshot', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('returns early when resolved db directory equals resolved chain directory', async () => {
    const service = new MithrilBootstrapService('/tmp/state');
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(
      '/mnt/custom-chain'
    );

    await service._installSnapshot('/mnt/custom-chain');

    expect(fs.move).not.toHaveBeenCalled();
    expect(fs.remove).not.toHaveBeenCalled();
  });

  it('moves db contents into resolved chain directory when db is nested in chain target', async () => {
    const service = new MithrilBootstrapService('/tmp/state');
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(
      '/mnt/custom-chain'
    );
    (fs.readdir as jest.Mock).mockResolvedValue(['immutable', 'ledger']);

    await service._installSnapshot('/mnt/custom-chain/db');

    expect(fs.move).toHaveBeenCalledWith(
      '/mnt/custom-chain/db/immutable',
      '/mnt/custom-chain/immutable',
      { overwrite: true }
    );
    expect(fs.move).toHaveBeenCalledWith(
      '/mnt/custom-chain/db/ledger',
      '/mnt/custom-chain/ledger',
      { overwrite: true }
    );
    expect(fs.remove).toHaveBeenCalledWith('/mnt/custom-chain/db');
  });

  it('preserves symlink and installs into resolved target for external storage', async () => {
    const service = new MithrilBootstrapService('/tmp/state');
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(
      '/mnt/custom-chain'
    );
    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => true,
    });
    (fs.readdir as jest.Mock).mockResolvedValue(['volatile']);

    await service._installSnapshot('/tmp/state/db');

    expect(fs.emptyDir).toHaveBeenCalledWith('/mnt/custom-chain');
    expect(fs.move).toHaveBeenCalledWith(
      '/tmp/state/db/volatile',
      '/mnt/custom-chain/volatile',
      { overwrite: true }
    );
    expect(fs.remove).toHaveBeenCalledWith('/tmp/state/db');
  });
});
