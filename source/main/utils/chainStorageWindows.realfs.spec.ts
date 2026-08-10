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
import {
  isSamePath,
  isSubPath,
  validateChainStorageDirectory,
} from './chainStorageValidation';

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

/**
 * A drive letter not currently in use, chosen from `candidates`.
 *
 * Callers pass disjoint candidate sets so two tests cannot select the same
 * letter and interfere with each other.
 */
const findFreeDriveLetter = (candidates: string): string | null => {
  for (const letter of candidates.split('')) {
    if (!fs.existsSync(`${letter}:\\`)) return letter;
  }
  return null;
};

const CHAIN_DIRECTORY_NAME = 'chain';

// `checkDiskSpace` spawns a subprocess on Windows and a cold start costs
// seconds, so any case that reaches it needs more than Jest's default five.
const DISK_SPACE_TIMEOUT_MS = 30_000;

// The classic Windows limit. A path longer than this needs the `\\?\` prefix,
// or long path support enabled, or it does not work.
const MAX_PATH = 260;

const makeGetDefaultConfig = (defaultPath: string) =>
  jest.fn().mockResolvedValue({
    defaultPath,
    availableSpaceBytes: Number.MAX_SAFE_INTEGER,
    requiredSpaceBytes: 0,
  });

// `existsSync` follows the link, so it is false for a link that exists but
// does not resolve — which is the state under test.
const isLink = (linkPath: string): boolean => {
  try {
    return fs.lstatSync(linkPath).isSymbolicLink();
  } catch {
    return false;
  }
};

const resolvesToDirectory = (linkPath: string): boolean => {
  try {
    return fs.statSync(fs.realpathSync(linkPath)).isDirectory();
  } catch {
    return false;
  }
};

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

  // Chain storage on a network drive is a supported advanced setup, but the
  // application cannot set it up itself: a junction cannot target a mapped
  // drive, and creating a true symbolic link needs
  // SeCreateSymbolicLinkPrivilege, which Daedalus does not have and should not
  // request. The documented answer is for the user to create the link out of
  // band with `mklink /D`, elevated.
  //
  // The network target cannot be automated here without a share, but the link
  // *type* can: what follows is exactly the link `mklink /D` produces, and the
  // resolvers must follow it the same way they follow a junction. That is the
  // half of the supported setup this suite can actually verify.
  describe('user-created symbolic link', () => {
    it('resolves a real symbolic link, not only a junction', async () => {
      const target = path.join(tmpRoot, 'elsewhere', 'chain');
      fs.ensureDirSync(target);
      const stateDir = path.join(tmpRoot, 'state');
      fs.ensureDirSync(stateDir);
      const chainPath = path.join(stateDir, CHAIN_DIRECTORY_NAME);

      // 'dir' rather than 'junction': this is what mklink /D creates, and it
      // is the link type a user following the advanced setup will have.
      fs.symlinkSync(target, chainPath, 'dir');

      const resolved = await resolveMithrilWorkDir(stateDir);

      expect(fs.realpathSync(resolved)).toBe(fs.realpathSync(target));
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
    it('resolves a junction whose target is a subst drive letter', async () => {
      const letter = findFreeDriveLetter('YXWVUT');
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

  // A drive letter backed by a *share* behaves differently from one backed by
  // a local directory, which is why the `subst` case above is not a substitute
  // for this one. `subst` produces a volume-relative path a junction can
  // record; a mapped network drive is a per-session object-manager entry, so
  // the junction is accepted and then does not work — reported from the field
  // as chain storage selected on a NAS-mapped drive that was "created but
  // corrupt".
  //
  // No external NAS is needed: Windows can share a local directory with itself
  // and map it back over the loopback, which exercises the real network
  // redirector.
  describe('mapped network drive', () => {
    const SHARE_NAME = 'DaedalusChainTest';

    it('never leaves a corrupt link when the target is a mapped network drive', async () => {
      const letter = findFreeDriveLetter('NMLKJ');
      expect(letter).not.toBeNull();

      const backing = path.join(tmpRoot, 'share-backing');
      fs.ensureDirSync(path.join(backing, 'chain'));

      let shared = false;
      let mapped = false;
      try {
        execFileSync('net', [
          'share',
          `${SHARE_NAME}=${backing}`,
          '/GRANT:Everyone,FULL',
        ]);
        shared = true;
        execFileSync('net', [
          'use',
          `${letter}:`,
          `\\\\localhost\\${SHARE_NAME}`,
        ]);
        mapped = true;

        const stateDir = path.join(tmpRoot, 'state');
        fs.ensureDirSync(stateDir);
        const chainPath = path.join(stateDir, CHAIN_DIRECTORY_NAME);
        const target = `${letter}:\\chain`;

        // Succeeding with a working link and failing outright are both
        // acceptable. Leaving a link that exists but does not resolve is not:
        // that is the defect exactly — success reported, storage broken.
        await createSymlink(target, chainPath).catch(() => undefined);

        const linkExists = fs.existsSync(chainPath) || isLink(chainPath);
        const linkWorks = resolvesToDirectory(chainPath);

        expect(linkExists && !linkWorks).toBe(false);
      } finally {
        if (mapped) {
          execFileSync('net', ['use', `${letter}:`, '/DELETE', '/Y']);
        }
        if (shared) {
          execFileSync('net', ['share', SHARE_NAME, '/DELETE']);
        }
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

  // Path syntax with no POSIX equivalent, all of it reachable by a user who
  // types a location rather than browsing to one.
  describe('path syntax the platform treats specially', () => {
    // CON, PRN, NUL and AUX name devices in every directory, so a directory
    // cannot be created with one of those names anywhere. The path can still be
    // typed. What matters is that it comes back rejected with a reason the
    // renderer has copy for, rather than through the generic catch: either the
    // location does not exist, or it exists and is not a directory. Which of
    // the two depends on how the device answers the probe, and both are honest.
    it.each(['CON', 'PRN', 'NUL', 'AUX'])(
      'rejects a path named %s with a specific reason',
      async (reservedName) => {
        const stateDir = path.join(tmpRoot, 'state');
        fs.ensureDirSync(stateDir);
        const reservedPath = path.join(tmpRoot, reservedName);

        // Precondition: the platform genuinely refuses the name. If a future
        // Windows accepts it, this test is no longer about what it claims and
        // should fail rather than quietly pass.
        expect(() => fs.mkdirSync(reservedPath)).toThrow();

        const result = await validateChainStorageDirectory(
          reservedPath,
          stateDir,
          makeGetDefaultConfig(path.join(stateDir, CHAIN_DIRECTORY_NAME)),
          0
        );

        expect(result.isValid).toBe(false);
        expect(result.reason).not.toBe('unknown');
      },
      DISK_SPACE_TIMEOUT_MS
    );

    // Windows strips a trailing dot or space when it creates the directory, so
    // the name the user gave and the name on disk differ. Everything downstream
    // compares paths, so the question is whether validation hands back the name
    // that exists or the name that was typed.
    it.each([
      ['space', 'trailing-space '],
      ['dot', 'trailing-dot.'],
    ])(
      'resolves a directory created with a trailing %s to the name on disk',
      async (_label, spelledName) => {
        const spelledPath = path.join(tmpRoot, spelledName);
        fs.ensureDirSync(spelledPath);

        const strippedPath = path.join(tmpRoot, spelledName.slice(0, -1));

        // Precondition: the platform really did strip it. Without this the
        // assertions below would hold trivially on a filesystem that keeps the
        // name intact.
        expect(fs.existsSync(strippedPath)).toBe(true);
        expect(fs.realpathSync(spelledPath)).toBe(strippedPath);

        // The comparison the product uses does not strip, so the two spellings
        // are not the same path to it, while they are the same directory to the
        // filesystem. Recorded rather than worked around: it only matters once
        // a stored custom path is compared against a resolved one.
        expect(isSamePath(spelledPath, strippedPath)).toBe(false);
      }
    );

    // Whether a path past the classic limit can be created depends on whether
    // long path support is enabled on the machine, and both answers are
    // legitimate. So the assertion ties the verdict to the observable fact
    // rather than to a guess about the runner: validation accepts the location
    // exactly when the directory is there, and never falls through to the
    // generic reason either way.
    it(
      'agrees with the filesystem about a path beyond MAX_PATH',
      async () => {
        const stateDir = path.join(tmpRoot, 'state');
        fs.ensureDirSync(stateDir);

        const segment = 'x'.repeat(40);
        const segmentCount =
          Math.ceil((MAX_PATH - tmpRoot.length) / (segment.length + 1)) + 2;
        const longPath = path.join(
          tmpRoot,
          ...Array(segmentCount).fill(segment)
        );
        expect(longPath.length).toBeGreaterThan(MAX_PATH);

        try {
          fs.ensureDirSync(longPath);
        } catch {
          // Long paths are not available here. That is one of the two outcomes
          // under test, not a failure to set the fixture up.
        }
        const exists = fs.existsSync(longPath);

        // Pure string work, so it holds at any length and on either outcome.
        // The nesting checks depend on it and run on whatever the user picked.
        expect(isSubPath(tmpRoot, longPath)).toBe(true);

        const result = await validateChainStorageDirectory(
          longPath,
          stateDir,
          makeGetDefaultConfig(path.join(stateDir, CHAIN_DIRECTORY_NAME)),
          0
        );

        expect(result.isValid).toBe(exists);
        expect(result.reason).not.toBe('unknown');
      },
      DISK_SPACE_TIMEOUT_MS
    );
  });
});
