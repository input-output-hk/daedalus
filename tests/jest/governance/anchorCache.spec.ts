/**
 * Filesystem cache for verified anchor bytes: hash-keyed file layout,
 * immutability of entries, path-traversal rejection, oldest-first eviction
 * and survival across a module reload.
 *
 * @jest-environment node
 */
import fs from 'fs';
import os from 'os';
import path from 'path';
import {
  ANCHOR_CACHE_MAX_ENTRIES,
  deleteVerifiedAnchorBytes,
  readVerifiedAnchorBytes,
  writeVerifiedAnchorBytes,
} from '../../../source/main/governance/anchorCache';
import { logger } from '../../../source/main/utils/logging';

// main/config boots launcher configuration and throws outside an Electron
// launcher, so the state directory is redirected to a temp dir instead.
jest.mock('../../../source/main/config', () => {
  const nodeOs = require('os');
  const nodePath = require('path');
  return {
    stateDirectoryPath: nodePath.join(nodeOs.tmpdir(), 'anchor-cache-spec'),
  };
});

// Mirrors the mocked config factory above.
const cacheDirectory = path.join(
  os.tmpdir(),
  'anchor-cache-spec',
  'DRep-anchor-cache'
);

const hashOf = (n: number) => n.toString(16).padStart(64, '0');

describe('anchorCache', () => {
  beforeEach(() => {
    jest.restoreAllMocks();
    fs.rmSync(cacheDirectory, { recursive: true, force: true });
  });

  it('returns the written bytes and stores them under the hash-named file', () => {
    const hash = hashOf(1);
    const bytes = Buffer.from('{"body":{"givenName":"Cache Entry"}}');
    writeVerifiedAnchorBytes(hash, bytes);
    expect(readVerifiedAnchorBytes(hash)).toEqual(bytes);
    expect(fs.existsSync(path.join(cacheDirectory, `${hash}.json`))).toBe(true);
  });

  it('keeps the original bytes when the same hash is written again', () => {
    const hash = hashOf(2);
    const original = Buffer.from('original bytes');
    writeVerifiedAnchorBytes(hash, original);
    const warnSpy = jest.spyOn(logger, 'warn');
    writeVerifiedAnchorBytes(hash, Buffer.from('replacement bytes'));
    expect(readVerifiedAnchorBytes(hash)).toEqual(original);
    expect(warnSpy).not.toHaveBeenCalled();
  });

  it('returns null for a hash with no cached entry', () => {
    expect(readVerifiedAnchorBytes(hashOf(3))).toBeNull();
  });

  it('rejects a path-traversal read before touching the filesystem', () => {
    const readSpy = jest.spyOn(fs, 'readFileSync');
    expect(readVerifiedAnchorBytes('../../../etc/passwd')).toBeNull();
    expect(readSpy).not.toHaveBeenCalled();
  });

  it('rejects a path-traversal write before touching the filesystem', () => {
    const writeSpy = jest.spyOn(fs, 'writeFileSync');
    const mkdirSpy = jest.spyOn(fs, 'mkdirSync');
    writeVerifiedAnchorBytes('../../../etc/passwd', Buffer.from('payload'));
    expect(writeSpy).not.toHaveBeenCalled();
    expect(mkdirSpy).not.toHaveBeenCalled();
  });

  it('rejects an uppercase hash in both primitives', () => {
    const upperHash = 'abcdef12'.repeat(8).toUpperCase();
    const writeSpy = jest.spyOn(fs, 'writeFileSync');
    writeVerifiedAnchorBytes(upperHash, Buffer.from('payload'));
    expect(writeSpy).not.toHaveBeenCalled();
    expect(readVerifiedAnchorBytes(upperHash)).toBeNull();
  });

  it('deletes an entry so a subsequent read returns null', () => {
    const hash = hashOf(7);
    writeVerifiedAnchorBytes(hash, Buffer.from('short-lived entry'));
    deleteVerifiedAnchorBytes(hash);
    expect(readVerifiedAnchorBytes(hash)).toBeNull();
  });

  it('evicts the oldest entries once the entry bound is exceeded', () => {
    fs.mkdirSync(cacheDirectory, { recursive: true });
    const baseTime = 1700000000;
    for (let i = 0; i <= ANCHOR_CACHE_MAX_ENTRIES; i += 1) {
      const filePath = path.join(cacheDirectory, `${hashOf(i)}.json`);
      fs.writeFileSync(filePath, Buffer.from(`entry ${i}`));
      fs.utimesSync(filePath, baseTime + i, baseTime + i);
    }
    const newHash = hashOf(4096);
    const newBytes = Buffer.from('newest entry');
    writeVerifiedAnchorBytes(newHash, newBytes);
    const remaining = fs.readdirSync(cacheDirectory);
    expect(remaining.length).toBeLessThanOrEqual(ANCHOR_CACHE_MAX_ENTRIES);
    expect(remaining).not.toContain(`${hashOf(0)}.json`);
    expect(readVerifiedAnchorBytes(newHash)).toEqual(newBytes);
  });

  it('serves an entry written before a module reload', () => {
    const hash = hashOf(9);
    const bytes = Buffer.from('survives restart');
    writeVerifiedAnchorBytes(hash, bytes);
    jest.resetModules();
    const reloaded = require('../../../source/main/governance/anchorCache');
    expect(reloaded.readVerifiedAnchorBytes(hash)).toEqual(bytes);
  });
});
