import fs from 'fs';
import os from 'os';
import path from 'path';
import {
  writeMigrationJournal,
  readMigrationJournal,
} from './chainStorageManagerLayout';
import type { ChainStorageMigrationJournal } from './chainStorageManagerShared';

// Real filesystem, no `fs-extra` mock.
//
// The migration journal lives in <stateDir>/Logs, and a state or logs directory
// is allowed to be a symlink to another disk — see ensureDirectoryExists and the
// fix in #3352. These cases record what actually happens to journal I/O for each
// shape that directory can take, because the journal is what makes an
// interrupted chain migration recoverable.

jest.mock('./logging', () => ({
  logger: {
    warn: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    debug: jest.fn(),
  },
}));

const JOURNAL_FILE = 'chain-storage-migration-journal.json';

const makeJournal = (): ChainStorageMigrationJournal => ({
  state: 'start',
  customPath: '/mnt/custom-parent',
  legacyRootPath: '/mnt/custom-parent',
  managedChainPath: '/mnt/custom-parent/chain',
  movedEntries: [],
  ignoredEntries: [],
  backupEntryPointPath: '/tmp/state/chain.legacy-backup',
  tempEntryPointPath: '/tmp/state/chain.managed-next',
  createdAt: '2026-01-01T00:00:00.000Z',
  updatedAt: '2026-01-01T00:00:00.000Z',
});

describe('migration journal I/O against a real filesystem', () => {
  let tmpRoot: string;
  let stateDir: string;

  // Only the two fields journal I/O reads.
  const makeCtx = () => {
    const logsDirectoryPath = path.join(stateDir, 'Logs');
    return {
      _logsDirectoryPath: logsDirectoryPath,
      _migrationJournalPath: path.join(logsDirectoryPath, JOURNAL_FILE),
    } as never;
  };

  beforeEach(() => {
    tmpRoot = fs.realpathSync(
      fs.mkdtempSync(path.join(os.tmpdir(), 'migration-journal-'))
    );
    stateDir = path.join(tmpRoot, 'state');
    fs.mkdirSync(stateDir, { recursive: true });
  });

  afterEach(() => {
    fs.rmSync(tmpRoot, { recursive: true, force: true });
  });

  it('writes and reads the journal when Logs is a plain directory', async () => {
    const journal = makeJournal();

    await writeMigrationJournal(makeCtx(), journal);

    await expect(readMigrationJournal(makeCtx())).resolves.toEqual(journal);
  });

  // The field-reported case: a state or logs directory pointed at another disk.
  it('writes and reads the journal when Logs is a symlink to a directory', async () => {
    const target = path.join(tmpRoot, 'logs-on-another-disk');
    fs.mkdirSync(target);
    fs.symlinkSync(target, path.join(stateDir, 'Logs'), 'dir');
    const journal = makeJournal();

    await writeMigrationJournal(makeCtx(), journal);

    // The journal must land in the link target, not beside the link.
    expect(fs.existsSync(path.join(target, JOURNAL_FILE))).toBe(true);
    await expect(readMigrationJournal(makeCtx())).resolves.toEqual(journal);
  });

  it('writes and reads the journal when Logs is a relative symlink', async () => {
    const target = path.join(tmpRoot, 'relative-logs');
    fs.mkdirSync(target);
    fs.symlinkSync(
      path.join('..', 'relative-logs'),
      path.join(stateDir, 'Logs'),
      'dir'
    );
    const journal = makeJournal();

    await writeMigrationJournal(makeCtx(), journal);

    expect(fs.existsSync(path.join(target, JOURNAL_FILE))).toBe(true);
    await expect(readMigrationJournal(makeCtx())).resolves.toEqual(journal);
  });

  // A dangling Logs symlink makes fs.ensureDir raise ENOENT rather than creating
  // the directory. Recorded here so the behaviour is known rather than assumed:
  // callers must treat a journal write as able to fail.
  it('fails to write the journal when Logs is a dangling symlink', async () => {
    const target = path.join(tmpRoot, 'logs-target-removed');
    fs.mkdirSync(target);
    fs.symlinkSync(target, path.join(stateDir, 'Logs'), 'dir');
    fs.rmSync(target, { recursive: true });

    await expect(
      writeMigrationJournal(makeCtx(), makeJournal())
    ).rejects.toMatchObject({ code: 'ENOENT' });
  });

  it('reports no journal rather than throwing when Logs is a dangling symlink', async () => {
    const target = path.join(tmpRoot, 'logs-target-removed');
    fs.mkdirSync(target);
    fs.symlinkSync(target, path.join(stateDir, 'Logs'), 'dir');
    fs.rmSync(target, { recursive: true });

    await expect(readMigrationJournal(makeCtx())).resolves.toBeNull();
  });

  it('returns null rather than throwing on a corrupt journal', async () => {
    fs.mkdirSync(path.join(stateDir, 'Logs'));
    fs.writeFileSync(
      path.join(stateDir, 'Logs', JOURNAL_FILE),
      '{ this is not json'
    );

    await expect(readMigrationJournal(makeCtx())).resolves.toBeNull();
  });
});
