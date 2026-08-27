import fs from 'fs';
import os from 'os';
import path from 'path';
import { CollateralPreferenceStore } from './CollateralPreferenceStore';

const walletId = 'ab'.repeat(20);
const networkGenesis = 'cd'.repeat(32);
const value = {
  walletId,
  networkGenesis,
  targetLovelace: '5000000',
  preferredInputs: [{ transactionId: 'ef'.repeat(32), index: 0 }],
  generation: 1,
};

describe('CollateralPreferenceStore', () => {
  let directory: string;
  let filePath: string;

  beforeEach(() => {
    directory = fs.mkdtempSync(path.join(os.tmpdir(), 'collateral-store-'));
    filePath = path.join(directory, 'preferences.json');
  });
  afterEach(() => fs.rmSync(directory, { recursive: true, force: true }));

  it('atomically persists only validated wallet and network preferences', () => {
    const store = new CollateralPreferenceStore(filePath);
    store.put(value);

    expect(
      new CollateralPreferenceStore(filePath).get(walletId, networkGenesis)
    ).toMatchObject(value);
    expect(fs.statSync(filePath).mode & 0o777).toBe(0o600);
    expect(fs.readFileSync(filePath, 'utf8')).not.toContain('state');
  });

  it('fails closed on corruption until explicit metadata repair', () => {
    fs.writeFileSync(filePath, '{bad');
    const store = new CollateralPreferenceStore(filePath);

    expect(store.isCorrupt).toBe(true);
    expect(store.get(walletId, networkGenesis)).toBeUndefined();
    expect(() => store.put(value)).toThrow('requires repair');
    store.repair();
    expect(store.isCorrupt).toBe(false);
  });
});
