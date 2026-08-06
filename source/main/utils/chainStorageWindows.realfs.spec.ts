// Real NTFS reparse points, on a real Windows filesystem.
//
// Everything else covering Windows behaviour in this repository overrides
// `process.platform` and mocks `fs-extra`, which means the mock encodes what
// the author believed Windows does. If that belief is wrong the test still
// passes, because the mock is the only thing under test. These assertions
// cannot be made on any other platform and cannot be faked on this one.
//
// The specific question they settle: Daedalus creates a *junction* on Windows
// (`chainStorageManagerShared.ts` `createSymlink`), and two code paths branch on
// how the platform then reports it. `captureChainPathState` and
// `resolveMithrilWorkDir` each handle a junction twice — once under
// `stats.isSymbolicLink()`, once under `process.platform === 'win32' &&
// stats.isDirectory()`. Only one of those can be the live path.

import fs from 'fs-extra';
import os from 'os';
import path from 'path';
import { execFileSync } from 'child_process';
import { resolveMithrilWorkDir } from './chainStoragePathResolver';
import { createSymlink } from './chainStorageManagerShared';
import { isSamePath, isSubPath } from './chainStorageValidation';

jest.mock('../config', () => ({
  DISK_SPACE_REQUIRED: 0,
}));

jest.mock('./logging', () => ({
  logger: {
    warn: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
    debug: jest.fn(),
  },
}));

const onWindows = process.platform === 'win32' ? describe : describe.skip;

const CHAIN_DIRECTORY_NAME = 'chain';

onWindows('Windows reparse point handling', () => {
  let tmpRoot: string;

  beforeEach(() => {
    tmpRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'daedalus-win-'));
  });

  afterEach(() => {
    fs.removeSync(tmpRoot);
  });

  const makeStateDirWithJunction = (
    targetPath: string
  ): { stateDir: string; chainPath: string } => {
    const stateDir = path.join(tmpRoot, 'state');
    fs.ensureDirSync(stateDir);
    const chainPath = path.join(stateDir, CHAIN_DIRECTORY_NAME);
    fs.symlinkSync(targetPath, chainPath, 'junction');
    return { stateDir, chainPath };
  };

  describe('lstat', () => {
    // The result of this single assertion determines which of the two branches
    // in `captureChainPathState` and `resolveMithrilWorkDir` is reachable at
    // all. If it fails, the mocked expectations in
    // `chainStorageWindowsNetwork.spec.ts` are built on a false premise; if it
    // passes, the `win32 && isDirectory()` branches are dead code for
    // junctions and only run for a plain directory.
    it('reports a junction as a symbolic link, not as a directory', () => {
      const target = path.join(tmpRoot, 'target');
      fs.ensureDirSync(target);
      const { chainPath } = makeStateDirWithJunction(target);

      const stats = fs.lstatSync(chainPath);

      expect(stats.isSymbolicLink()).toBe(true);
      expect(stats.isDirectory()).toBe(false);
    });

    it('reports a plain directory as a directory', () => {
      const plain = path.join(tmpRoot, 'plain');
      fs.ensureDirSync(plain);

      const stats = fs.lstatSync(plain);

      expect(stats.isSymbolicLink()).toBe(false);
      expect(stats.isDirectory()).toBe(true);
    });
  });

  describe('createSymlink', () => {
    it('creates a junction that resolves to the target directory', async () => {
      const target = path.join(tmpRoot, 'target');
      fs.ensureDirSync(target);
      const link = path.join(tmpRoot, 'link');

      await createSymlink(target, link);

      expect(fs.lstatSync(link).isSymbolicLink()).toBe(true);
      expect(fs.realpathSync(link)).toBe(fs.realpathSync(target));
    });

    it('creates a junction that can be written through', async () => {
      const target = path.join(tmpRoot, 'target');
      fs.ensureDirSync(target);
      const link = path.join(tmpRoot, 'link');

      await createSymlink(target, link);
      fs.writeFileSync(
        path.join(link, 'probe.txt'),
        'written through junction'
      );

      expect(fs.readFileSync(path.join(target, 'probe.txt'), 'utf8')).toBe(
        'written through junction'
      );
    });

    // A junction is a local-volume reparse point and cannot target a UNC path.
    // The failure must surface rather than leaving a link that resolves at
    // creation and dangles later.
    it('fails rather than creating a junction to a UNC path', async () => {
      const link = path.join(tmpRoot, 'unc-link');

      await expect(
        createSymlink('\\\\localhost\\NoSuchShare\\chain', link)
      ).rejects.toThrow();
      expect(fs.existsSync(link)).toBe(false);
    });
  });

  describe('resolveMithrilWorkDir', () => {
    it('returns the junction target rather than the entry point', async () => {
      const target = path.join(tmpRoot, 'elsewhere', 'chain');
      fs.ensureDirSync(target);
      const { stateDir } = makeStateDirWithJunction(target);

      const resolved = await resolveMithrilWorkDir(stateDir);

      expect(fs.realpathSync(resolved)).toBe(fs.realpathSync(target));
    });

    it('returns an absolute path when the junction target has been deleted', async () => {
      const target = path.join(tmpRoot, 'doomed');
      fs.ensureDirSync(target);
      const { stateDir } = makeStateDirWithJunction(target);
      fs.removeSync(target);

      const resolved = await resolveMithrilWorkDir(stateDir);

      // The dangling case must not leak a relative path: it is handed to
      // MithrilBootstrapService.setWorkDir(), where a relative value would be
      // resolved against process.cwd().
      expect(path.win32.isAbsolute(resolved)).toBe(true);
    });

    it('returns the chain path itself when no junction is present', async () => {
      const stateDir = path.join(tmpRoot, 'state');
      const chainPath = path.join(stateDir, CHAIN_DIRECTORY_NAME);
      fs.ensureDirSync(chainPath);

      const resolved = await resolveMithrilWorkDir(stateDir);

      expect(fs.realpathSync(resolved)).toBe(fs.realpathSync(chainPath));
    });
  });

  // `subst` maps a drive letter to a local directory, giving a genuine
  // drive-letter path with no network involved — the closest reproduction of
  // the reported mapped-drive case that needs no share.
  describe('drive letters', () => {
    const findFreeDriveLetter = (): string | null => {
      for (const letter of 'YXWVUT'.split('')) {
        if (!fs.existsSync(`${letter}:\\`)) return letter;
      }
      return null;
    };

    it('resolves a junction whose target is a subst drive letter', async () => {
      const letter = findFreeDriveLetter();
      // Asserted rather than skipped: a silent skip here would report coverage
      // of the drive-letter case that does not exist.
      expect(letter).not.toBeNull();

      const backing = path.join(tmpRoot, 'backing');
      fs.ensureDirSync(backing);
      execFileSync('subst', [`${letter}:`, backing]);

      try {
        const target = `${letter}:\\chain`;
        fs.ensureDirSync(target);
        const { stateDir } = makeStateDirWithJunction(target);

        const resolved = await resolveMithrilWorkDir(stateDir);

        expect(path.win32.isAbsolute(resolved)).toBe(true);
        expect(fs.existsSync(resolved)).toBe(true);
      } finally {
        execFileSync('subst', [`${letter}:`, '/D']);
      }
    });
  });

  describe('isSamePath', () => {
    it('treats paths differing only in case as the same path', () => {
      expect(
        isSamePath('C:\\Users\\Test\\Chain', 'c:\\users\\test\\chain')
      ).toBe(true);
    });
  });

  describe('isSubPath', () => {
    it('treats a child differing only in case as nested', () => {
      expect(
        isSubPath('C:\\Daedalus\\State', 'c:\\daedalus\\state\\chain')
      ).toBe(true);
    });

    it('does not treat a sibling with a shared prefix as nested', () => {
      expect(isSubPath('C:\\Daedalus\\State', 'C:\\Daedalus\\State2')).toBe(
        false
      );
    });
  });
});
