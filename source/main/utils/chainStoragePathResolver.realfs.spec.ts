import fs from 'fs';
import os from 'os';
import path from 'path';
import { resolveMithrilWorkDir } from './chainStoragePathResolver';

// Real filesystem, no `fs-extra` mock. `chainStoragePathResolver.spec.ts` covers
// the branch logic against mocks; these cases depend on what the filesystem
// actually does — in particular that `realpath` fails on a dangling link while
// `readlink` still returns its raw target.

jest.mock('./logging', () => ({
  logger: {
    warn: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    debug: jest.fn(),
  },
}));

describe('resolveMithrilWorkDir against a real filesystem', () => {
  let tmpRoot: string;
  let stateDir: string;
  let chainPath: string;

  beforeEach(() => {
    tmpRoot = fs.realpathSync(
      fs.mkdtempSync(path.join(os.tmpdir(), 'chain-path-resolver-'))
    );
    stateDir = path.join(tmpRoot, 'state');
    fs.mkdirSync(stateDir, { recursive: true });
    chainPath = path.join(stateDir, 'chain');
  });

  afterEach(() => {
    fs.rmSync(tmpRoot, { recursive: true, force: true });
  });

  it('returns the chain directory itself when it is a plain directory', async () => {
    fs.mkdirSync(chainPath);

    await expect(resolveMithrilWorkDir(stateDir)).resolves.toBe(chainPath);
  });

  it('resolves a symlink with an absolute target to that target', async () => {
    const target = path.join(tmpRoot, 'elsewhere');
    fs.mkdirSync(target);
    fs.symlinkSync(target, chainPath, 'dir');

    await expect(resolveMithrilWorkDir(stateDir)).resolves.toBe(target);
  });

  it('resolves a symlink with a relative target to that target', async () => {
    const target = path.join(tmpRoot, 'elsewhere');
    fs.mkdirSync(target);
    fs.symlinkSync(path.join('..', 'elsewhere'), chainPath, 'dir');

    await expect(resolveMithrilWorkDir(stateDir)).resolves.toBe(target);
  });

  // A user who moves the chain directory twice leaves a link pointing at a link.
  // `realpath` walks the whole chain, so the resolver should report the directory
  // at the end of it rather than the intermediate link.
  it('resolves a chain of symlinks to the directory at the end of it', async () => {
    const target = path.join(tmpRoot, 'final-target');
    const intermediate = path.join(tmpRoot, 'intermediate-link');
    fs.mkdirSync(target);
    fs.symlinkSync(target, intermediate, 'dir');
    fs.symlinkSync(intermediate, chainPath, 'dir');

    await expect(resolveMithrilWorkDir(stateDir)).resolves.toBe(target);
  });

  // Two links pointing at each other. `realpath` raises ELOOP, so the resolver
  // takes the same fallback a dangling link takes and returns the raw target.
  // The point of the assertion is that it returns an absolute path instead of
  // throwing: the result is handed to MithrilBootstrapService.setWorkDir(),
  // which has no defence against either.
  it('returns an absolute path when the entry point is part of a symlink loop', async () => {
    const partner = path.join(tmpRoot, 'loop-partner');
    fs.symlinkSync(partner, chainPath, 'dir');
    fs.symlinkSync(chainPath, partner, 'dir');

    // Precondition, not decoration: if the loop resolved, the fallback under
    // test would never run and the assertions below would pass vacuously. The
    // errno is ELOOP on POSIX and unmeasured on Windows, so only the failure
    // itself is asserted here.
    expect(() => fs.realpathSync(chainPath)).toThrow();

    const result = await resolveMithrilWorkDir(stateDir);

    expect(path.isAbsolute(result)).toBe(true);
    expect(result).toBe(partner);
  });

  // The regression this file exists for. When `realpath` fails — a dangling
  // link locally, or a mapped network drive on Windows — the resolver falls back
  // to `readlink`, which returns the target exactly as it was recorded. For a
  // relative link that is a relative path, and the result is handed to
  // MithrilBootstrapService.setWorkDir(), where it would resolve against
  // process.cwd() rather than the state directory.
  it('returns an absolute path when a relative symlink target cannot be resolved', async () => {
    const missingTarget = path.join(tmpRoot, 'target-removed-later');
    fs.mkdirSync(missingTarget);
    fs.symlinkSync(path.join('..', 'target-removed-later'), chainPath, 'dir');
    fs.rmSync(missingTarget, { recursive: true });

    const result = await resolveMithrilWorkDir(stateDir);

    expect(path.isAbsolute(result)).toBe(true);
    expect(result).toBe(missingTarget);
  });

  it('returns an absolute path when an absolute symlink target cannot be resolved', async () => {
    const missingTarget = path.join(tmpRoot, 'absolute-target-removed');
    fs.mkdirSync(missingTarget);
    fs.symlinkSync(missingTarget, chainPath, 'dir');
    fs.rmSync(missingTarget, { recursive: true });

    const result = await resolveMithrilWorkDir(stateDir);

    expect(path.isAbsolute(result)).toBe(true);
    expect(result).toBe(missingTarget);
  });

  // A Windows drive-letter target is not absolute by POSIX rules, and `path` is
  // the POSIX implementation here. Without a win32 check the resolver would
  // treat 'Z:\...' as relative and resolve it into nonsense, which is precisely
  // the mapped network drive fallback chainStorageWindowsNetwork.spec.ts guards.
  it('leaves a Windows drive-letter target intact rather than resolving it', async () => {
    const windowsTarget = 'Z:\\DaedalusChain\\chain';
    fs.symlinkSync(windowsTarget, chainPath, 'dir');

    await expect(resolveMithrilWorkDir(stateDir)).resolves.toBe(windowsTarget);
  });

  it('falls back to the chain path itself when the entry point is missing', async () => {
    const result = await resolveMithrilWorkDir(stateDir);

    expect(path.isAbsolute(result)).toBe(true);
    expect(result).toBe(chainPath);
  });
});
