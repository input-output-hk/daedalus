import fs from 'fs';
import os from 'os';
import path from 'path';
import { parseNetworkMagic, validatePath } from './chainStorageValidate';

// The running cluster's network magic, and the one makeNodeDatabase marks a
// database with
const NETWORK_MAGIC = 1;
const OTHER_NETWORK_MAGIC = '764824073';

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
    const result = validatePath(folder, stateDir, NETWORK_MAGIC);

    expect(result.isValid).toBe(true);
    expect(result.chainSubdirectoryStatus).toBe('will-create');
  });

  it('accepts an empty chain subdirectory', () => {
    fs.mkdirSync(chain);

    const result = validatePath(folder, stateDir, NETWORK_MAGIC);

    expect(result.isValid).toBe(true);
    expect(result.chainSubdirectoryStatus).toBe('existing-directory');
  });

  it('accepts a chain subdirectory holding a node database', () => {
    makeNodeDatabase(chain);

    expect(validatePath(folder, stateDir, NETWORK_MAGIC).isValid).toBe(true);
  });

  it('accepts a node database that also holds other files', () => {
    makeNodeDatabase(chain);
    fs.writeFileSync(path.join(chain, 'notes.txt'), 'user data');

    expect(validatePath(folder, stateDir, NETWORK_MAGIC).isValid).toBe(true);
  });

  it('accepts a chain subdirectory holding only database entries', () => {
    fs.mkdirSync(path.join(chain, 'volatile'), { recursive: true });
    fs.writeFileSync(path.join(chain, 'lock'), '');
    fs.writeFileSync(path.join(chain, '.DS_Store'), '');

    expect(validatePath(folder, stateDir, NETWORK_MAGIC).isValid).toBe(true);
  });

  it('refuses a chain subdirectory holding other files and leaves them', () => {
    fs.mkdirSync(chain);
    fs.writeFileSync(path.join(chain, 'photo.jpg'), 'user data');

    const result = validatePath(folder, stateDir, NETWORK_MAGIC);

    expect(result).toEqual({
      isValid: false,
      path: folder,
      reason: 'chain-subdirectory-not-database',
    });
    expect(fs.readFileSync(path.join(chain, 'photo.jpg'), 'utf8')).toBe(
      'user data'
    );
  });

  it("refuses a chain subdirectory holding another network's database and leaves it", () => {
    makeNodeDatabase(chain);
    fs.writeFileSync(path.join(chain, 'protocolMagicId'), OTHER_NETWORK_MAGIC);

    const result = validatePath(folder, stateDir, NETWORK_MAGIC);

    expect(result).toEqual({
      isValid: false,
      path: folder,
      reason: 'chain-subdirectory-other-network',
    });
    expect(fs.readFileSync(path.join(chain, 'protocolMagicId'), 'utf8')).toBe(
      OTHER_NETWORK_MAGIC
    );
    expect(fs.readdirSync(chain).sort()).toEqual([
      'immutable',
      'protocolMagicId',
    ]);
  });

  it("refuses another network's marker even without other database entries", () => {
    fs.mkdirSync(chain);
    fs.writeFileSync(
      path.join(chain, 'protocolMagicId'),
      `${OTHER_NETWORK_MAGIC}\n`
    );

    expect(validatePath(folder, stateDir, NETWORK_MAGIC).reason).toBe(
      'chain-subdirectory-other-network'
    );
  });

  it('accepts a database of any network when the network magic is unknown', () => {
    makeNodeDatabase(chain);
    fs.writeFileSync(path.join(chain, 'protocolMagicId'), OTHER_NETWORK_MAGIC);

    expect(validatePath(folder, stateDir, null).isValid).toBe(true);
  });

  it('refuses a chain entry that is a file', () => {
    fs.writeFileSync(chain, 'user data');

    const result = validatePath(folder, stateDir, NETWORK_MAGIC);

    expect(result).toEqual({
      isValid: false,
      path: folder,
      reason: 'chain-entry-not-directory',
    });
  });
});

describe('parseNetworkMagic', () => {
  it('reads a decimal network magic with surrounding whitespace', () => {
    expect(parseNetworkMagic('764824073')).toBe(764824073);
    expect(parseNetworkMagic(' 2\n')).toBe(2);
  });

  it('returns null for a missing, non-numeric or out-of-range value', () => {
    expect(parseNetworkMagic(undefined)).toBeNull();
    expect(parseNetworkMagic('')).toBeNull();
    expect(parseNetworkMagic('mainnet')).toBeNull();
    expect(parseNetworkMagic('-1')).toBeNull();
    expect(parseNetworkMagic('4294967296')).toBeNull();
  });
});
