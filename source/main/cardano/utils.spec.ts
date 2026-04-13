import fs from 'fs-extra';
import { spawnSync } from 'child_process';
import { createSelfnodeConfig } from './utils';

const chainStorageManagerMock = {
  unlinkChainEntryPoint: jest.fn(),
  resetToDefault: jest.fn(),
};

jest.mock('fs-extra', () => ({
  pathExists: jest.fn(),
  readJson: jest.fn(),
  remove: jest.fn(),
  writeFile: jest.fn(),
  readFile: jest.fn(),
}));

jest.mock('electron', () => ({
  BrowserWindow: jest.fn(),
  dialog: {},
}));

jest.mock('child_process', () => ({
  spawnSync: jest.fn(),
}));

jest.mock('../utils/logging', () => ({
  logger: {
    info: jest.fn(),
  },
}));

jest.mock('../utils/getTranslation', () => ({
  getTranslation: jest.fn(),
}));

jest.mock('../utils/ensureDirectoryExists', () => jest.fn());

jest.mock('../utils/restoreKeystore', () => ({
  decodeKeystore: jest.fn(),
}));

jest.mock('../utils/chainStorageManager', () => ({
  ChainStorageManager: jest
    .fn()
    .mockImplementation(() => chainStorageManagerMock),
}));

describe('cardano utils', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    (fs.pathExists as jest.Mock)
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(true);
    (fs.readJson as jest.Mock).mockResolvedValue({
      systemStart: 'legacy-value',
    });
    (fs.readFile as jest.Mock).mockResolvedValue(
      Buffer.from(JSON.stringify({ GenesisFile: 'old-genesis.json' }))
    );
    (spawnSync as jest.Mock).mockReturnValue({
      stdout: Buffer.from('mock-genesis-hash\n'),
    });
    chainStorageManagerMock.unlinkChainEntryPoint.mockResolvedValue(undefined);
    chainStorageManagerMock.resetToDefault.mockResolvedValue({
      isValid: true,
      path: null,
    });
  });

  it('unlinks the chain entry point before resetting selfnode storage to default', async () => {
    const result = await createSelfnodeConfig(
      '/tmp/selfnode/input-config.json',
      '/tmp/selfnode/genesis-source.json',
      '/tmp/selfnode/state',
      '/tmp/bin/cardano-cli'
    );

    expect(chainStorageManagerMock.unlinkChainEntryPoint).toHaveBeenCalledTimes(
      1
    );
    expect(chainStorageManagerMock.resetToDefault).toHaveBeenCalledTimes(1);
    expect(
      chainStorageManagerMock.unlinkChainEntryPoint.mock.invocationCallOrder[0]
    ).toBeLessThan(
      chainStorageManagerMock.resetToDefault.mock.invocationCallOrder[0]
    );
    expect(fs.remove).toHaveBeenCalledWith('/tmp/selfnode/state/wallets');
    expect(result).toEqual({
      configPath: '/tmp/selfnode/state/config.yaml',
      genesisPath: '/tmp/selfnode/state/genesis.json',
      genesisHash: 'mock-genesis-hash',
    });
  });

  it('still clears chain storage before failing when the selfnode config file is missing', async () => {
    (fs.pathExists as jest.Mock).mockReset();
    (fs.pathExists as jest.Mock)
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(false);

    await expect(
      createSelfnodeConfig(
        '/tmp/selfnode/input-config.json',
        '/tmp/selfnode/genesis-source.json',
        '/tmp/selfnode/state',
        '/tmp/bin/cardano-cli'
      )
    ).rejects.toThrow('No config file found');

    expect(chainStorageManagerMock.unlinkChainEntryPoint).toHaveBeenCalledTimes(
      1
    );
    expect(chainStorageManagerMock.resetToDefault).toHaveBeenCalledTimes(1);
    expect(fs.remove).not.toHaveBeenCalledWith('/tmp/selfnode/state/wallets');
  });
});
