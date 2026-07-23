import fs from 'fs';
import os from 'os';
import path from 'path';
import { logDRepStateSnapshot } from '../../../source/main/utils/setupLogging';
import type { DRepListQueryPayload } from '../../../source/common/types/governance.types';

// main/config boots launcher configuration and throws outside an Electron
// launcher, so the log folder is redirected to a temp dir instead. jest.mock
// calls hoist above the imports, so the factories apply before setupLogging
// resolves its dependencies (same pattern as GovernanceStore.spec.ts).
jest.mock('../../../source/main/config', () => {
  const nodeOs = require('os');
  const nodePath = require('path');
  const base = nodePath.join(nodeOs.tmpdir(), 'drep-snapshot-spec');
  return {
    appLogsFolderPath: base,
    pubLogsFolderPath: nodePath.join(base, 'pub'),
  };
});

jest.mock('../../../source/main/environment', () => ({
  environment: {
    network: 'mainnet',
    os: 'linux',
    platformVersion: '0',
    version: '0.0.0',
  },
}));

jest.mock('electron-log-daedalus', () => ({
  transports: { console: {}, file: {}, rendererConsole: {} },
}));

// Mirrors the mocked config factory above.
const pubLogsFolderPath = path.join(os.tmpdir(), 'drep-snapshot-spec', 'pub');
const SNAPSHOT_PATH = path.join(pubLogsFolderPath, 'DRep-state-snapshot.json');

const publicPayload: DRepListQueryPayload = {
  dreps: [
    {
      anchor: {
        hash: 'a'.repeat(64),
        url: 'https://example.org/drep.jsonld',
      },
      drepActivity: 12,
      drepId: 'drep1yg7s8vuv87f8a8f5d0m9yk4p5xqw6r4s3t2u1v9w8x7y6z5a4b',
      status: 'active',
      votingPower: null,
    },
  ],
  epoch: 512,
  fetchedAt: 1_750_000_000_000,
};

describe('logDRepStateSnapshot', () => {
  beforeEach(() => {
    fs.mkdirSync(pubLogsFolderPath, { recursive: true });
    if (fs.existsSync(SNAPSHOT_PATH)) {
      fs.unlinkSync(SNAPSHOT_PATH);
    }
  });

  it('writes the public directory payload with drepIds retained', () => {
    logDRepStateSnapshot(publicPayload);

    const written = fs.readFileSync(SNAPSHOT_PATH, 'utf-8');
    const parsed = JSON.parse(written);
    // The filterLogData bypass is the point: public drepIds must survive.
    expect(written).toContain(publicPayload.dreps[0].drepId);
    expect(parsed.data.dreps).toHaveLength(1);
    expect(parsed.data.epoch).toBe(512);
    expect(parsed.msg).toBe('Updating DRep-state-snapshot.json file');
  });

  it('overwrites the previous snapshot on each successful fetch', () => {
    logDRepStateSnapshot(publicPayload);
    logDRepStateSnapshot({
      ...publicPayload,
      epoch: 513,
      fetchedAt: 1_750_000_100_000,
    });

    const parsed = JSON.parse(fs.readFileSync(SNAPSHOT_PATH, 'utf-8'));
    expect(parsed.data.epoch).toBe(513);
  });

  it('never contains user vote or delegation fields', () => {
    logDRepStateSnapshot(publicPayload);

    const written = fs.readFileSync(SNAPSHOT_PATH, 'utf-8');
    // The payload type carries no wallet/vote state; pin the wire keys that
    // would betray a leak if the writer were ever fed the wrong payload.
    expect(written).not.toContain('voteKind');
    expect(written).not.toContain('chosenOption');
    expect(written).not.toContain('delegation');
  });

  it('registers the snapshot filename in ALLOWED_LOGS (source-text check)', () => {
    // Importing the real main/config throws outside an Electron launcher, so
    // membership is asserted at source level; the end-to-end bundle proof
    // stays with the release verification pass.
    const configSource = fs.readFileSync(
      path.resolve(__dirname, '../../../source/main/config.ts'),
      'utf-8'
    );
    expect(configSource).toContain("'DRep-state-snapshot.json'");
  });
});
