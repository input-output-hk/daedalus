/** @jest-environment node */

import fs from 'fs';
import os from 'os';
import path from 'path';
import { createHash } from 'crypto';
import { deflateRawSync } from 'zlib';

const { ec: EC } = require('elliptic');
const braces = require('braces');
const FormData = require('form-data');
const { sprintf } = require('sprintf-js');
const extract = require('extract-zip');
const vectors = require('./fixtures/elliptic-p521-regressions.json');

// Minimal ZIP writer so the regression fixtures can contain duplicate
// entry names and Unix symlinks. Ordinary ZIP writers often reject duplicates.
function archive(
  entries: Array<{
    name: string;
    contents: string;
    mode?: number;
    compress?: boolean;
  }>
) {
  const files: Buffer[] = [];
  const directory: Buffer[] = [];
  let offset = 0;
  for (const entry of entries) {
    const name = Buffer.from(entry.name);
    const contents = Buffer.from(entry.contents);
    const payload = entry.compress ? deflateRawSync(contents) : contents;
    const method = entry.compress ? 8 : 0;
    let crc = 0xffffffff;
    for (const byte of contents) {
      crc ^= byte;
      for (let bit = 0; bit < 8; bit++) {
        crc = (crc >>> 1) ^ (crc & 1 ? 0xedb88320 : 0);
      }
    }
    crc = (crc ^ 0xffffffff) >>> 0;
    const local = Buffer.alloc(30);
    local.writeUInt32LE(0x04034b50, 0);
    local.writeUInt16LE(20, 4);
    local.writeUInt16LE(method, 8);
    local.writeUInt32LE(crc, 14);
    local.writeUInt32LE(payload.length, 18);
    local.writeUInt32LE(contents.length, 22);
    local.writeUInt16LE(name.length, 26);
    const central = Buffer.alloc(46);
    central.writeUInt32LE(0x02014b50, 0);
    central.writeUInt16LE((3 << 8) | 20, 4);
    central.writeUInt16LE(20, 6);
    central.writeUInt16LE(method, 10);
    central.writeUInt32LE(crc, 16);
    central.writeUInt32LE(payload.length, 20);
    central.writeUInt32LE(contents.length, 24);
    central.writeUInt16LE(name.length, 28);
    central.writeUInt32LE(((entry.mode || 0o100644) << 16) >>> 0, 38);
    central.writeUInt32LE(offset, 42);
    files.push(local, name, payload);
    directory.push(central, name);
    offset += local.length + name.length + payload.length;
  }
  const central = Buffer.concat(directory);
  const end = Buffer.alloc(22);
  end.writeUInt32LE(0x06054b50, 0);
  end.writeUInt16LE(entries.length, 8);
  end.writeUInt16LE(entries.length, 10);
  end.writeUInt32LE(central.length, 12);
  end.writeUInt32LE(offset, 16);
  return Buffer.concat([...files, central, end]);
}

describe('dependency security patches', () => {
  // Reporter-provided known-answer vectors: these both fail in elliptic 6.6.1.
  // https://github.com/bleichenbacher-daniel/Rooterberg/blob/main/test_vectors/ecdsa/ecdsa_rfc6979_secp521r1_sha_512.json
  test.each(vectors)(
    'RFC 6979 P-521 leading-zero nonce, vector $tcId',
    (vector) => {
      const ec = new EC('p521');
      const digest = createHash('sha512')
        .update(Buffer.from(vector.msg, 'hex'))
        .digest();
      const signature = ec.sign(digest, vector.privateKey, 'hex');
      expect(signature.toDER('hex')).toBe(vector.sig);
      expect(
        ec.verify(
          digest,
          vector.sig,
          ec.keyFromPrivate(vector.privateKey, 'hex')
        )
      ).toBe(true);
    }
  );

  test.each(['compile', 'expand', 'stringify'])(
    'braces.%s bounds nested patterns',
    (operation) => {
      const pattern = '{'.repeat(20000) + 'a,b' + '}'.repeat(20000);
      expect(() => braces[operation](pattern)).toThrow(SyntaxError);
      expect(braces.expand('wallet/{mainnet,preview}/{1..2}')).toEqual([
        'wallet/mainnet/1',
        'wallet/mainnet/2',
        'wallet/preview/1',
        'wallet/preview/2',
      ]);
    }
  );

  test.each(['compile', 'expand', 'stringify'])(
    'braces.%s bounds supplied AST depth',
    (operation) => {
      const ast = { type: 'root', nodes: [] as any[] };
      let parent: any = ast;
      for (let i = 0; i < 1000; i++) {
        const child = { type: 'paren', nodes: [], parent };
        parent.nodes.push(child);
        parent = child;
      }
      expect(() => braces[operation](ast)).toThrow(SyntaxError);
    }
  );

  test.each(['e', 'f', 'g'])(
    'sprintf bounds excessive %s precision',
    (format) => {
      expect(() => sprintf(`%.100000${format}`, 1.25)).not.toThrow();
      expect(sprintf(`%.100000${format}`, 1.25).length).toBeLessThanOrEqual(
        110
      );
      expect(() => sprintf(`%.0${format}`, 1.25)).not.toThrow();
      expect(sprintf('%.2f', 1.25)).toBe('1.25');
    }
  );

  test('form-data escapes line breaks in field names and filenames', () => {
    const form = new FormData();
    form.append('email"\r\nX-Injected: true\r\nfake="', 'value', {
      filename: 'a"\r\nX-Injected: true.txt',
    });
    const body = form.getBuffer().toString();
    expect(
      body.split('\r\n').filter((l) => l.startsWith('X-Injected'))
    ).toEqual([]);
  });
});

describe('ZIP extraction containment', () => {
  let temporary: string;
  let dir: string;
  let zip: string;
  beforeEach(() => {
    temporary = fs.mkdtempSync(
      path.join(os.tmpdir(), 'daedalus-zip-security-')
    );
    dir = path.join(temporary, 'destination');
    zip = path.join(temporary, 'archive.zip');
    fs.mkdirSync(dir);
  });
  afterEach(() => fs.rmSync(temporary, { recursive: true, force: true }));

  test('finishes extracting large deflated entries on Node 26', async () => {
    // yauzl 2.x stalls at EOF here, leaving installer packaging unfinished.
    const contents = 'wallet'.repeat(256 * 1024);
    fs.writeFileSync(
      zip,
      archive([{ name: 'electron.bin', contents, compress: true }])
    );
    await extract(zip, { dir });
    expect(fs.readFileSync(path.join(dir, 'electron.bin'), 'utf8')).toBe(
      contents
    );
  });

  test('extracts regular files through both Promise and legacy callback APIs', async () => {
    fs.writeFileSync(
      zip,
      archive([{ name: 'nested/file', contents: 'wallet' }])
    );
    await extract(zip, { dir });
    expect(fs.readFileSync(path.join(dir, 'nested/file'), 'utf8')).toBe(
      'wallet'
    );
    await new Promise<void>((resolve, reject) => {
      extract(zip, { dir }, (error: Error) =>
        error ? reject(error) : resolve()
      );
    });
  });

  test('rejects an escaping symlink before it can overwrite an outside file', async () => {
    const victim = path.join(temporary, 'victim');
    fs.writeFileSync(victim, 'unchanged');
    fs.writeFileSync(
      zip,
      archive([
        { name: 'link', contents: '../victim', mode: 0o120777 },
        { name: 'link', contents: 'overwritten' },
      ])
    );
    await expect(extract(zip, { dir })).rejects.toThrow(/Out of bound/);
    expect(fs.readFileSync(victim, 'utf8')).toBe('unchanged');
    expect(fs.existsSync(path.join(dir, 'link'))).toBe(false);
  });

  const symlinkTest = process.platform === 'win32' ? test.skip : test;

  symlinkTest(
    'preserves valid in-root forward-reference symlinks',
    async () => {
      fs.writeFileSync(
        zip,
        archive([
          { name: 'link', contents: 'nested/file', mode: 0o120777 },
          { name: 'nested/file', contents: 'wallet' },
        ])
      );
      await extract(zip, { dir });
      expect(fs.readFileSync(path.join(dir, 'link'), 'utf8')).toBe('wallet');
    }
  );

  symlinkTest(
    'never writes a regular entry through a planted final-component symlink',
    async () => {
      const victim = path.join(temporary, 'victim');
      fs.writeFileSync(victim, 'unchanged');
      fs.symlinkSync(victim, path.join(dir, 'entry'));
      fs.writeFileSync(
        zip,
        archive([{ name: 'entry', contents: 'overwritten' }])
      );
      await expect(extract(zip, { dir })).rejects.toThrow(/overwrite symlink/);
      expect(fs.readFileSync(victim, 'utf8')).toBe('unchanged');
    }
  );

  symlinkTest(
    'rejects a repeated archive entry that would overwrite an in-root symlink',
    async () => {
      fs.writeFileSync(
        zip,
        archive([
          { name: 'target', contents: 'unchanged' },
          { name: 'link', contents: 'target', mode: 0o120777 },
          { name: 'link', contents: 'overwritten' },
        ])
      );
      await expect(extract(zip, { dir })).rejects.toThrow(/overwrite symlink/);
      expect(fs.readFileSync(path.join(dir, 'target'), 'utf8')).toBe(
        'unchanged'
      );
    }
  );

  symlinkTest(
    'rejects dangling chains that lead outside the destination',
    async () => {
      fs.symlinkSync('../missing-outside', path.join(dir, 'seed'));
      fs.writeFileSync(
        zip,
        archive([{ name: 'link', contents: 'seed', mode: 0o120777 }])
      );
      await expect(extract(zip, { dir })).rejects.toThrow(/Out of bound/);
    }
  );

  symlinkTest(
    'checks existing parent symlinks before creating directories',
    async () => {
      const outside = path.join(temporary, 'outside');
      fs.mkdirSync(outside);
      fs.symlinkSync(outside, path.join(dir, 'seed'));
      fs.writeFileSync(
        zip,
        archive([{ name: 'seed/new/file', contents: 'overwritten' }])
      );
      await expect(extract(zip, { dir })).rejects.toThrow(/Out of bound/);
      expect(fs.existsSync(path.join(outside, 'new'))).toBe(false);
    }
  );
});
