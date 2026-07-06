import fs from 'fs-extra';
import {
  writeMithrilPartialSyncMarker,
  readMithrilPartialSyncMarker,
} from './mithrilPartialSyncMarker';

jest.mock('fs-extra', () => ({
  ensureDir: jest.fn().mockResolvedValue(undefined),
  writeJson: jest.fn().mockResolvedValue(undefined),
  pathExists: jest.fn(),
  readJson: jest.fn(),
  remove: jest.fn().mockResolvedValue(undefined),
}));

jest.mock('../config', () => ({
  stateDirectoryPath: '/tmp/daedalus-state',
}));

describe('mithrilPartialSyncMarker', () => {
  const ensureDirMock = fs.ensureDir as jest.Mock;
  const writeJsonMock = fs.writeJson as jest.Mock;
  const pathExistsMock = fs.pathExists as jest.Mock;
  const readJsonMock = fs.readJson as jest.Mock;

  beforeEach(() => {
    jest.clearAllMocks();
    ensureDirMock.mockResolvedValue(undefined);
    writeJsonMock.mockResolvedValue(undefined);
  });

  describe('writeMithrilPartialSyncMarker', () => {
    it('writes a marker with stagingRootPath when provided', async () => {
      const marker = await writeMithrilPartialSyncMarker(
        'installed-awaiting-node-start',
        {
          managedChainPath: '/chain',
          stagingRootPath: '/vol/mithril-partial-sync',
        }
      );

      expect(marker.stagingRootPath).toBe('/vol/mithril-partial-sync');
      expect(marker.managedChainPath).toBe('/chain');
      expect(marker.state).toBe('installed-awaiting-node-start');

      expect(writeJsonMock).toHaveBeenCalledWith(
        expect.any(String),
        expect.objectContaining({
          state: 'installed-awaiting-node-start',
          stagingRootPath: '/vol/mithril-partial-sync',
          managedChainPath: '/chain',
        }),
        { spaces: 2 }
      );
    });

    it('omits stagingRootPath from the written marker when not provided (parity with managedChainPath)', async () => {
      const marker = await writeMithrilPartialSyncMarker(
        'cutover-in-progress',
        { managedChainPath: '/chain' }
      );

      expect(marker.stagingRootPath).toBeUndefined();
      expect(writeJsonMock).toHaveBeenCalledWith(
        expect.any(String),
        expect.not.objectContaining({ stagingRootPath: expect.anything() }),
        { spaces: 2 }
      );
    });

    it('omits both optional fields when options is empty', async () => {
      const marker = await writeMithrilPartialSyncMarker('cutover-in-progress');

      expect(marker.managedChainPath).toBeUndefined();
      expect(marker.stagingRootPath).toBeUndefined();
    });

    it('writes node-start-verified carrying both paths', async () => {
      const marker = await writeMithrilPartialSyncMarker(
        'node-start-verified',
        {
          managedChainPath: '/chain',
          stagingRootPath: '/vol/mithril-partial-sync',
        }
      );

      expect(marker.state).toBe('node-start-verified');
      expect(marker.stagingRootPath).toBe('/vol/mithril-partial-sync');
    });
  });

  describe('readMithrilPartialSyncMarker', () => {
    it('returns stagingRootPath verbatim when the marker is valid', async () => {
      pathExistsMock.mockResolvedValue(true);
      readJsonMock.mockResolvedValue({
        state: 'node-start-verified',
        updatedAt: '2026-06-01T00:00:00.000Z',
        managedChainPath: '/chain',
        stagingRootPath: '/vol/mithril-partial-sync',
      });

      const result = await readMithrilPartialSyncMarker();

      expect(result?.stagingRootPath).toBe('/vol/mithril-partial-sync');
      expect(result?.state).toBe('node-start-verified');
      expect(result?.managedChainPath).toBe('/chain');
    });

    it('returns null when the marker file does not exist', async () => {
      pathExistsMock.mockResolvedValue(false);

      const result = await readMithrilPartialSyncMarker();

      expect(result).toBeNull();
    });

    it('returns the unsafe-cutover fallback (no stagingRootPath) when the file is corrupt', async () => {
      pathExistsMock.mockResolvedValue(true);
      readJsonMock.mockRejectedValue(new Error('JSON parse error'));

      const result = await readMithrilPartialSyncMarker();

      expect(result?.state).toBe('cutover-in-progress');
      expect(result?.stagingRootPath).toBeUndefined();
    });

    it('round-trips stagingRootPath through write then read', async () => {
      let writtenValue: unknown;
      writeJsonMock.mockImplementation(async (_path: string, data: unknown) => {
        writtenValue = data;
      });
      pathExistsMock.mockResolvedValue(true);
      readJsonMock.mockImplementation(async () => writtenValue);

      await writeMithrilPartialSyncMarker('installed-awaiting-node-start', {
        managedChainPath: '/c',
        stagingRootPath: '/s',
      });

      const result = await readMithrilPartialSyncMarker();

      expect(result?.stagingRootPath).toBe('/s');
      expect(result?.managedChainPath).toBe('/c');
      expect(result?.state).toBe('installed-awaiting-node-start');
    });
  });
});
