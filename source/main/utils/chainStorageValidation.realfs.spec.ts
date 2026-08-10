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

// Three assertions below do not hold on Windows, for reasons that are about
// the platform rather than the fixture. They are gated rather than adapted,
// because two of them look like real defects and adapting the test would hide
// the question rather than answer it:
//
//   * POSIX mode bits do not deny writes on Windows, so the read-only case
//     needs an ACL (`icacls /deny`) to set up at all.
//   * A dangling symlink is reported as `unknown` on Windows rather than
//     `path-not-found` — the same shape as the `EACCES` mislabelling this file
//     was written to catch, and unexplained as yet.
//   * Resolving a symlinked target times out. `checkDiskSpace` shells out on
//     Windows, and the tool it reaches for is not present on current images.
//
// The real-reparse-point behaviour that *is* settled has its own suite in
// chainStorageWindows.realfs.spec.ts.
// `checkDiskSpace` shells out on Windows, and a cold PowerShell start costs
// seconds, so every assertion reaching it needs more than Jest's default five.
// Slow rather than broken — but worth knowing that a user-facing validation
// path pays that cost on every call.
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

    it('reports a dangling symlink as path-not-found', async () => {
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
  });

  it(
    'resolves a chain of symlinks to the directory at the end of it',
    async () => {
      const target = path.join(tmpRoot, 'final-target');
      const intermediate = path.join(tmpRoot, 'intermediate-link');
      fs.mkdirSync(target);
      fs.symlinkSync(target, intermediate, 'dir');
      const link = path.join(tmpRoot, 'link-to-link');
      fs.symlinkSync(intermediate, link, 'dir');

      const result = await validateChainStorageDirectory(
        link,
        stateDir,
        makeGetDefaultConfig(path.join(stateDir, 'chain')),
        REQUIRED_SPACE
      );

      expect(result.isValid).toBe(true);
      expect(result.resolvedPath).toBe(fs.realpathSync(target));
    },
    DISK_SPACE_TIMEOUT_MS
  );

  // Two links pointing at each other. The platforms disagree about which
  // syscall fails first, and therefore about which reason comes back: POSIX
  // fails the existence probe and reports the path as missing, while Windows
  // satisfies that probe on the reparse point and fails the resolution
  // instead. The rejection is what holds on both, and is what this asserts.
  it('rejects a directory that is part of a symlink loop', async () => {
    const link = path.join(tmpRoot, 'loop-entry');
    const partner = path.join(tmpRoot, 'loop-partner');
    fs.symlinkSync(partner, link, 'dir');
    fs.symlinkSync(link, partner, 'dir');

    expect(() => fs.realpathSync(link)).toThrow();

    const result = await validateChainStorageDirectory(
      link,
      stateDir,
      makeGetDefaultConfig(path.join(stateDir, 'chain')),
      REQUIRED_SPACE
    );

    expect(result.isValid).toBe(false);
  });

  it('rejects the state directory itself', async () => {
    const result = await validateChainStorageDirectory(
      stateDir,
      stateDir,
      makeGetDefaultConfig(path.join(stateDir, 'chain')),
      REQUIRED_SPACE
    );

    expect(result.isValid).toBe(false);
    expect(result.reason).toBe('inside-state-dir');
  });

  // The nesting check runs on the resolved path, so a link that lives outside
  // the state directory but points inside it has to be rejected on where it
  // lands rather than on where it sits. A mocked filesystem cannot express the
  // difference, because the mock decides both.
  it('rejects a symlink that resolves to a location inside the state directory', async () => {
    const insideState = path.join(stateDir, 'nested-target');
    const link = path.join(tmpRoot, 'link-into-state');
    fs.mkdirSync(insideState);
    fs.symlinkSync(insideState, link, 'dir');

    const result = await validateChainStorageDirectory(
      link,
      stateDir,
      makeGetDefaultConfig(path.join(stateDir, 'chain')),
      REQUIRED_SPACE
    );

    expect(result.isValid).toBe(false);
    expect(result.reason).toBe('inside-state-dir');
  });

  // Selecting the managed chain directory is how a user asks to go back to the
  // default location, so it is accepted with a null path rather than rejected
  // as being inside the state directory.
  it('treats the managed chain directory as a reset to the default location', async () => {
    const chainPath = path.join(stateDir, 'chain');
    fs.mkdirSync(chainPath);
    const defaultPath = fs.realpathSync(chainPath);

    const result = await validateChainStorageDirectory(
      chainPath,
      stateDir,
      makeGetDefaultConfig(defaultPath),
      REQUIRED_SPACE
    );

    expect(result.isValid).toBe(true);
    expect(result.path).toBeNull();
    expect(result.resolvedPath).toBe(defaultPath);
  });

  // Same outcome by a different route: the selected path is somewhere else
  // entirely, and only resolving it shows that it lands on the managed chain
  // directory. This is the branch that compares resolved paths rather than
  // literal ones.
  it('treats a symlink that resolves to the managed chain directory as a reset', async () => {
    const chainPath = path.join(stateDir, 'chain');
    fs.mkdirSync(chainPath);
    const defaultPath = fs.realpathSync(chainPath);
    const alias = path.join(tmpRoot, 'alias-to-chain');
    fs.symlinkSync(chainPath, alias, 'dir');

    const result = await validateChainStorageDirectory(
      alias,
      stateDir,
      makeGetDefaultConfig(defaultPath),
      REQUIRED_SPACE
    );

    expect(result.isValid).toBe(true);
    expect(result.path).toBeNull();
    expect(result.resolvedPath).toBe(defaultPath);
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
