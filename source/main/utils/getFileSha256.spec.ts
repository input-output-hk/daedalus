/**
 * @jest-environment node
 */
import fs from 'fs';
import os from 'os';
import path from 'path';
import { createHash } from 'crypto';
import { getFileSha256 } from './getFileSha256';

describe('getFileSha256', () => {
  let directory: string;

  beforeEach(() => {
    directory = fs.mkdtempSync(path.join(os.tmpdir(), 'sha256-spec-'));
  });

  afterEach(() => {
    fs.rmSync(directory, { recursive: true, force: true });
  });

  it('resolves the hex SHA-256 of a file larger than one read chunk', async () => {
    const content = Buffer.alloc(300 * 1024, 'daedalus');
    const filePath = path.join(directory, 'installer.bin');
    fs.writeFileSync(filePath, content);

    await expect(getFileSha256(filePath)).resolves.toBe(
      createHash('sha256').update(content).digest('hex')
    );
  });

  it('rejects when the file does not exist', async () => {
    await expect(
      getFileSha256(path.join(directory, 'missing.bin'))
    ).rejects.toThrow();
  });
});
