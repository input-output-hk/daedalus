import fs from 'fs';
import os from 'os';
import path from 'path';
import ensureDirectoryExists from './ensureDirectoryExists';

jest.mock('./logging', () => ({
  logger: {
    error: jest.fn(),
  },
}));

describe('ensureDirectoryExists', () => {
  let tmpRoot: string;
  let exitSpy: jest.SpyInstance;

  beforeEach(() => {
    tmpRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'ensure-dir-'));
    exitSpy = jest
      .spyOn(process, 'exit')
      // Throwing keeps the test process alive while still proving exit was called.
      .mockImplementation(((code?: number) => {
        throw new Error(`process.exit(${code})`);
      }) as never);
  });

  afterEach(() => {
    exitSpy.mockRestore();
    fs.rmSync(tmpRoot, { recursive: true, force: true });
  });

  it('accepts an existing directory', () => {
    const dir = path.join(tmpRoot, 'existing');
    fs.mkdirSync(dir);

    expect(() => ensureDirectoryExists(dir)).not.toThrow();
    expect(exitSpy).not.toHaveBeenCalled();
  });

  it('creates the directory when it is missing', () => {
    const dir = path.join(tmpRoot, 'nested', 'missing');

    expect(() => ensureDirectoryExists(dir)).not.toThrow();
    expect(fs.statSync(dir).isDirectory()).toBe(true);
    expect(exitSpy).not.toHaveBeenCalled();
  });

  // A state or logs directory may be symlinked to another disk; lstat would
  // report the link itself and wrongly exit the whole app.
  //
  // The link type follows the platform for the same reason the product does
  // (chainStorageManagerShared.ts createSymlink): on Windows a 'dir' symlink
  // needs SeCreateSymbolicLinkPrivilege, whereas a junction needs none, so
  // testing with 'dir' here would assert against a link Daedalus never makes.
  it('accepts a symlink pointing at a directory', () => {
    const target = path.join(tmpRoot, 'target');
    const link = path.join(tmpRoot, 'link');
    fs.mkdirSync(target);
    fs.symlinkSync(
      target,
      link,
      process.platform === 'win32' ? 'junction' : 'dir'
    );

    expect(() => ensureDirectoryExists(link)).not.toThrow();
    expect(exitSpy).not.toHaveBeenCalled();
  });

  it('exits when the path exists but is a file', () => {
    const file = path.join(tmpRoot, 'a-file');
    fs.writeFileSync(file, 'not a directory');

    expect(() => ensureDirectoryExists(file)).toThrow('process.exit(1)');
    expect(exitSpy).toHaveBeenCalledWith(1);
  });
});
