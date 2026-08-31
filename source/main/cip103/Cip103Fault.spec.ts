import { spawnSync } from 'child_process';
import fs from 'fs';
import os from 'os';
import path from 'path';

const child = path.join(
  process.cwd(),
  'tests/dapps/cip103/process-exit-child.ts'
);

describe('CIP-103 process boundaries', () => {
  it('leaves later items unattempted when the process exits between submissions', () => {
    const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'cip103-exit-'));
    const log = path.join(directory, 'attempts.log');
    try {
      const result = spawnSync(
        process.execPath,
        ['-r', '@swc-node/register', child, log],
        { cwd: process.cwd(), encoding: 'utf8' }
      );

      expect(result.status).toBe(73);
      expect(fs.readFileSync(log, 'utf8')).toBe('0\n');
    } finally {
      fs.rmSync(directory, { recursive: true, force: true });
    }
  });
});
