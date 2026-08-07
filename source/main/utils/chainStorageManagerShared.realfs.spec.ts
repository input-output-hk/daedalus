import fs from 'fs';
import os from 'os';
import path from 'path';
import { movePath } from './chainStorageManagerShared';

// Real filesystem, no `fs-extra` mock.
//
// `movePath` exists to survive a move across a device boundary: `fs.move` fails
// with EXDEV, and it falls back to copy-then-remove. Every migration spec stubs
// this function out with `jest.spyOn(manager, '_movePath')`, so until now the
// EXDEV branch had never executed in a test — despite being the code that moves
// a user's entire chain directory, potentially tens of gigabytes, to another
// disk.

jest.mock('./logging', () => ({
  logger: {
    warn: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    debug: jest.fn(),
  },
}));

/**
 * Returns a directory on a different device from `reference`, or null.
 *
 * `/dev/shm` is a tmpfs on Linux and is present and writable inside the Nix
 * build sandbox, which makes a genuine cross-device move testable without any
 * privileges or mounting.
 */
const findCrossDeviceRoot = (reference: string): string | null => {
  const referenceDev = fs.statSync(reference).dev;
  for (const candidate of ['/dev/shm', os.tmpdir(), '/var/tmp']) {
    try {
      if (fs.statSync(candidate).dev !== referenceDev) {
        // Confirm it is writable before claiming it, not just present.
        const probe = fs.mkdtempSync(path.join(candidate, 'xdev-probe-'));
        fs.rmSync(probe, { recursive: true, force: true });
        return candidate;
      }
    } catch {
      // Not present, not writable, or not a directory. Try the next.
    }
  }
  return null;
};

describe('movePath against a real filesystem', () => {
  let sameDeviceRoot: string;

  beforeEach(() => {
    sameDeviceRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'move-path-'));
  });

  afterEach(() => {
    fs.rmSync(sameDeviceRoot, { recursive: true, force: true });
  });

  const seedTree = (root: string) => {
    fs.mkdirSync(path.join(root, 'immutable'), { recursive: true });
    fs.writeFileSync(path.join(root, 'immutable', '00000.chunk'), 'chunk data');
    fs.writeFileSync(path.join(root, 'protocolMagicId'), '764824073');
  };

  const expectTreeAt = (root: string) => {
    expect(
      fs.readFileSync(path.join(root, 'immutable', '00000.chunk'), 'utf8')
    ).toBe('chunk data');
    expect(fs.readFileSync(path.join(root, 'protocolMagicId'), 'utf8')).toBe(
      '764824073'
    );
  };

  it('moves a directory tree within the same device', async () => {
    const source = path.join(sameDeviceRoot, 'source');
    const target = path.join(sameDeviceRoot, 'target');
    fs.mkdirSync(source);
    seedTree(source);

    await movePath(source, target);

    expectTreeAt(target);
    expect(fs.existsSync(source)).toBe(false);
  });

  it('moves a directory tree across a device boundary via the EXDEV fallback', async () => {
    const crossDeviceRoot = findCrossDeviceRoot(sameDeviceRoot);

    // Fail loudly rather than skipping. This is the only test that exercises the
    // EXDEV branch; a silent skip would report coverage that does not exist.
    expect(crossDeviceRoot).not.toBeNull();

    const otherDeviceDir = fs.mkdtempSync(
      path.join(crossDeviceRoot as string, 'move-path-xdev-')
    );

    try {
      const source = path.join(sameDeviceRoot, 'source');
      const target = path.join(otherDeviceDir, 'target');
      fs.mkdirSync(source);
      seedTree(source);

      // Precondition: the two roots must genuinely be on different devices, or
      // `fs.move` would succeed by rename and the fallback would never run.
      expect(fs.statSync(source).dev).not.toBe(fs.statSync(otherDeviceDir).dev);
      expect(() => fs.renameSync(source, target)).toThrow(
        expect.objectContaining({ code: 'EXDEV' })
      );

      await movePath(source, target);

      expectTreeAt(target);
      expect(fs.existsSync(source)).toBe(false);
    } finally {
      fs.rmSync(otherDeviceDir, { recursive: true, force: true });
    }
  });

  it('rethrows an error that is not EXDEV', async () => {
    const source = path.join(sameDeviceRoot, 'does-not-exist');
    const target = path.join(sameDeviceRoot, 'target');

    await expect(movePath(source, target)).rejects.toThrow();
    expect(fs.existsSync(target)).toBe(false);
  });
});
