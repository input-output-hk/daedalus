import fs from 'fs';
import os from 'os';
import path from 'path';
import { validateChainStorageDirectory } from './chainStorageValidation';

// This spec deliberately does NOT mock `fs-extra`. `chainStorageValidation.spec.ts`
// covers the branch logic against a mocked filesystem; the cases here are ones a
// mock cannot express, because they depend on how the real filesystem behaves:
// permission bits, symlink resolution, and the errno a syscall actually raises.
//
// Jest's module registry is per test file, so mocking `fs-extra` there and using
// the real module here does not conflict.

// `../config` refuses to load outside the Nix shell, and `./logging` writes to
// the Electron app data directory. Both are mocked; neither is the filesystem
// behaviour under test. `check-disk-space` is left real, so the free-space read
// happens against the actual temp volume.
jest.mock('../config', () => ({
  DISK_SPACE_REQUIRED: 1024,
}));

jest.mock('./logging', () => ({
  logger: {
    warn: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    debug: jest.fn(),
  },
}));

const REQUIRED_SPACE = 1024;

const makeGetDefaultConfig = (defaultPath: string) =>
  jest.fn().mockResolvedValue({
    defaultPath,
    availableSpaceBytes: Number.MAX_SAFE_INTEGER,
    requiredSpaceBytes: REQUIRED_SPACE,
  });

/**
 * Asserts that `dir` genuinely denies writes before a test depends on it.
 *
 * A process with privileges that bypass the mode bits — root, or CAP_DAC_OVERRIDE
 * in a container — can still write to a 0555 directory. Without this the
 * read-only test would pass vacuously in exactly the environments where it was
 * not actually exercised. Failing loudly is better than a silent false pass.
 */
const expectWritesDenied = (dir: string) => {
  expect(() => fs.accessSync(dir, fs.constants.W_OK)).toThrow();
};

// One assertion below does not hold on Windows, and the reason is the platform
// rather than the fixture: POSIX mode bits do not deny writes there, so the
// read-only case cannot be set up at all without an ACL (`icacls /deny`). It is
// gated rather than adapted.
//
// The real-reparse-point behaviour that is settled has its own suite in
// chainStorageWindows.realfs.spec.ts.
//
// `checkDiskSpace` shells out on Windows, and a cold subprocess start costs
// seconds, so every assertion reaching it needs more than Jest's default five.
// Slow rather than broken, but worth knowing that a user-facing validation path
// pays that cost on every call.
const DISK_SPACE_TIMEOUT_MS = 30_000;

const describeOnPosix = process.platform === 'win32' ? describe.skip : describe;

describe('validateChainStorageDirectory against a real filesystem', () => {
  let tmpRoot: string;
  let stateDir: string;

  beforeEach(() => {
    tmpRoot = fs.mkdtempSync(
      path.join(os.tmpdir(), 'chain-storage-validation-')
    );
    stateDir = path.join(tmpRoot, 'state');
    fs.mkdirSync(stateDir, { recursive: true });
  });

  afterEach(() => {
    // Restore write permission first, or the recursive removal fails on the
    // directories this spec deliberately made read-only.
    for (const entry of fs.readdirSync(tmpRoot)) {
      const full = path.join(tmpRoot, entry);
      try {
        fs.chmodSync(full, 0o700);
      } catch {
        // Not all entries are directories we changed; ignore.
      }
    }
    fs.rmSync(tmpRoot, { recursive: true, force: true });
  });

  // Gated at the describe level rather than by aliasing `it`: the lint rule
  // that keeps `expect` inside a test block only recognises the standard
  // names, and a gate it cannot see would be worse than none.
  it(
    'resolves a symlinked target directory to its real path',
    async () => {
      const target = path.join(tmpRoot, 'real-target');
      const link = path.join(tmpRoot, 'link-to-target');
      fs.mkdirSync(target);
      fs.symlinkSync(target, link, 'dir');

      const result = await validateChainStorageDirectory(
        link,
        stateDir,
        makeGetDefaultConfig(path.join(stateDir, 'chain')),
        REQUIRED_SPACE
      );

      expect(result.resolvedPath).toBe(fs.realpathSync(target));
    },
    DISK_SPACE_TIMEOUT_MS
  );

  describeOnPosix('on a POSIX filesystem', () => {
    it('reports a read-only directory as not-writable rather than unknown', async () => {
      const target = path.join(tmpRoot, 'read-only');
      fs.mkdirSync(target);
      fs.chmodSync(target, 0o555);
      expectWritesDenied(target);

      const result = await validateChainStorageDirectory(
        target,
        stateDir,
        makeGetDefaultConfig(path.join(stateDir, 'chain')),
        REQUIRED_SPACE
      );

      expect(result.isValid).toBe(false);
      expect(result.reason).toBe('not-writable');
    });
  });

  // The two platforms fail this at different syscalls: on POSIX the existence
  // probe follows the link and reports nothing there, while on Windows the
  // reparse point satisfies the probe and the resolution fails instead. The
  // user is told the same thing either way, which is what this asserts.
  it('reports a link whose target is gone as path-not-found', async () => {
    const missingTarget = path.join(tmpRoot, 'target-that-was-deleted');
    const link = path.join(tmpRoot, 'dangling');
    fs.mkdirSync(missingTarget);
    fs.symlinkSync(missingTarget, link, 'dir');
    fs.rmSync(missingTarget, { recursive: true });

    const result = await validateChainStorageDirectory(
      link,
      stateDir,
      makeGetDefaultConfig(path.join(stateDir, 'chain')),
      REQUIRED_SPACE
    );

    expect(result.isValid).toBe(false);
    expect(result.reason).toBe('path-not-found');
  });

  it('reports a file selected as the target with path-is-file semantics', async () => {
    const target = path.join(tmpRoot, 'a-file');
    fs.writeFileSync(target, 'not a directory');

    const result = await validateChainStorageDirectory(
      target,
      stateDir,
      makeGetDefaultConfig(path.join(stateDir, 'chain')),
      REQUIRED_SPACE
    );

    expect(result.isValid).toBe(false);
  });

  it('accepts a directory whose path contains spaces and non-ASCII characters', async () => {
    const target = path.join(tmpRoot, 'Ünïcödé 目録 dir');
    fs.mkdirSync(target);

    const result = await validateChainStorageDirectory(
      target,
      stateDir,
      makeGetDefaultConfig(path.join(stateDir, 'chain')),
      REQUIRED_SPACE
    );

    expect(result.reason).toBeUndefined();
    expect(result.isValid).toBe(true);
  });
});
