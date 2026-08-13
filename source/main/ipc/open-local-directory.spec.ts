import { shell } from 'electron';
import { openLocalDirectory } from './open-local-directory';

jest.mock('electron', () => ({
  ipcMain: { on: jest.fn(), once: jest.fn() },
  shell: { openPath: jest.fn() },
}));

const mockOpenPath = shell.openPath as jest.Mock;

describe('local directory opening', () => {
  it('awaits and accepts an empty Electron error response', async () => {
    mockOpenPath.mockResolvedValue('');
    await expect(openLocalDirectory('/tmp/example')).resolves.toBeUndefined();
    expect(mockOpenPath).toHaveBeenCalledWith('/tmp/example');
  });

  it('rejects Electron error responses', async () => {
    mockOpenPath.mockResolvedValue('Unable to open directory');
    await expect(openLocalDirectory('/tmp/example')).rejects.toThrow(
      'Unable to open directory'
    );
  });
});
