import fs from 'fs';
import os from 'os';
import path from 'path';
import { validatePath } from './chainStorageValidate';

describe('validatePath', () => {
  let root: string;
  let stateDir: string;
  let folder: string;
  let chain: string;

  beforeEach(() => {
    root = fs.mkdtempSync(path.join(os.tmpdir(), 'chain-storage-validate-'));
    stateDir = path.join(root, 'state');
    folder = path.join(root, 'picked');
    chain = path.join(folder, 'chain');
    fs.mkdirSync(stateDir);
    fs.mkdirSync(folder);
  });

  afterEach(() => {
    fs.rmSync(root, { recursive: true, force: true });
  });

  const makeNodeDatabase = (dir: string) => {
    fs.mkdirSync(path.join(dir, 'immutable'), { recursive: true });
    fs.writeFileSync(path.join(dir, 'protocolMagicId'), '1');
  };

  it('accepts a folder without a chain subdirectory', () => {
    const result = validatePath(folder, stateDir);

    expect(result.isValid).toBe(true);
    expect(result.chainSubdirectoryStatus).toBe('will-create');
  });

  it('accepts an empty chain subdirectory', () => {
    fs.mkdirSync(chain);

    const result = validatePath(folder, stateDir);

    expect(result.isValid).toBe(true);
    expect(result.chainSubdirectoryStatus).toBe('existing-directory');
  });

  it('accepts a chain subdirectory holding a node database', () => {
    makeNodeDatabase(chain);

    expect(validatePath(folder, stateDir).isValid).toBe(true);
  });

  it('accepts a node database that also holds other files', () => {
    makeNodeDatabase(chain);
    fs.writeFileSync(path.join(chain, 'notes.txt'), 'user data');

    expect(validatePath(folder, stateDir).isValid).toBe(true);
  });

  it('accepts a chain subdirectory holding only database entries', () => {
    fs.mkdirSync(path.join(chain, 'volatile'), { recursive: true });
    fs.writeFileSync(path.join(chain, 'lock'), '');
    fs.writeFileSync(path.join(chain, '.DS_Store'), '');

    expect(validatePath(folder, stateDir).isValid).toBe(true);
  });

  it('refuses a chain subdirectory holding other files and leaves them', () => {
    fs.mkdirSync(chain);
    fs.writeFileSync(path.join(chain, 'photo.jpg'), 'user data');

    const result = validatePath(folder, stateDir);

    expect(result).toEqual({
      isValid: false,
      path: folder,
      reason: 'chain-subdirectory-not-database',
    });
    expect(fs.readFileSync(path.join(chain, 'photo.jpg'), 'utf8')).toBe(
      'user data'
    );
  });

  it('refuses a chain entry that is a file', () => {
    fs.writeFileSync(chain, 'user data');

    const result = validatePath(folder, stateDir);

    expect(result).toEqual({
      isValid: false,
      path: folder,
      reason: 'chain-entry-not-directory',
    });
  });
});
