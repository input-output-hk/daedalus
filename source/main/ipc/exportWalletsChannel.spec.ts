/**
 * @jest-environment node
 */
import fs from 'fs-extra';
import os from 'os';
import path from 'path';

jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest
    .fn()
    .mockImplementation(() => ({ onRequest: jest.fn() })),
}));

jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
    warn: jest.fn(),
  },
}));

const { exportWallets } = require('./exportWalletsChannel');

const FIXTURE_DIR = path.resolve(
  __dirname,
  '../../../tests/wallets/e2e/documents/import-files'
);
const SECRET_KEY = path.join(FIXTURE_DIR, 'Secrets-1.0', 'secret.key');
const WALLET_ID_PREFIXES_WITHOUT_PASSWORD = [
  '656e7fd4',
  'f3007ab2',
  '8ea78091',
];
const WALLET_ID_PREFIXES_WITH_PASSWORD = ['fec0c934', 'da4e2784', 'a94d44b7'];

describe('exportWallets', () => {
  it('exports every wallet in a key file', async () => {
    const { wallets, errors } = await exportWallets({
      exportSourcePath: SECRET_KEY,
      locale: 'en-US',
    });
    expect(errors).toBe('');
    expect(wallets).toHaveLength(6);
    wallets.forEach((wallet) => {
      expect(wallet.id).toMatch(/^[0-9a-f]{40}$/);
      expect(wallet.name).toBeNull();
      expect(wallet.encrypted_root_private_key).toMatch(/^[0-9a-f]{256}$/);
      expect(wallet.passphrase_hash).toMatch(/^[0-9a-f]+$/);
    });
  });

  it('flags wallets without a spending password', async () => {
    const { wallets } = await exportWallets({
      exportSourcePath: SECRET_KEY,
      locale: 'en-US',
    });
    const emptyIds = wallets
      .filter((w) => w.isEmptyPassphrase)
      .map((w) => w.id.slice(0, 8));
    const passwordIds = wallets
      .filter((w) => !w.isEmptyPassphrase)
      .map((w) => w.id.slice(0, 8));
    expect(emptyIds.sort()).toEqual(
      [...WALLET_ID_PREFIXES_WITHOUT_PASSWORD].sort()
    );
    expect(passwordIds.sort()).toEqual(
      [...WALLET_ID_PREFIXES_WITH_PASSWORD].sort()
    );
  });

  it('derives the wallet id from the public half of the exported key', async () => {
    const { wallets } = await exportWallets({
      exportSourcePath: SECRET_KEY,
      locale: 'en-US',
    });
    const blake2b = require('blake2b');
    wallets.forEach((wallet) => {
      const xprv = Buffer.from(wallet.encrypted_root_private_key, 'hex');
      expect(blake2b(20).update(xprv.slice(64)).digest('hex')).toBe(wallet.id);
    });
  });

  it('exports the key file found in a Daedalus state directory', async () => {
    const { wallets, errors } = await exportWallets({
      exportSourcePath: FIXTURE_DIR,
      locale: 'en-US',
    });
    expect(errors).toBe('');
    expect(wallets).toHaveLength(6);
  });

  it('exports a Linux-layout state directory', async () => {
    const dir = await fs.mkdtemp(path.join(os.tmpdir(), 'export-wallets-'));
    try {
      await fs.copy(SECRET_KEY, path.join(dir, 'Secrets', 'secret.key'));
      const { wallets, errors } = await exportWallets({
        exportSourcePath: dir,
        locale: 'en-US',
      });
      expect(errors).toBe('');
      expect(wallets).toHaveLength(6);
    } finally {
      await fs.remove(dir);
    }
  });

  it('returns an error instead of throwing when the source is missing', async () => {
    const { wallets, errors } = await exportWallets({
      exportSourcePath: path.join(FIXTURE_DIR, 'does-not-exist'),
      locale: 'en-US',
    });
    expect(wallets).toEqual([]);
    expect(errors).not.toBe('');
  });

  it('returns an error when a state directory has no key file', async () => {
    const { wallets, errors } = await exportWallets({
      exportSourcePath: path.join(FIXTURE_DIR, 'Wallet-1.0-acid'),
      locale: 'en-US',
    });
    expect(wallets).toEqual([]);
    expect(errors).toContain('No secret.key found');
  });

  it('returns an error when the file is not a key file', async () => {
    const dir = await fs.mkdtemp(path.join(os.tmpdir(), 'export-wallets-'));
    try {
      const file = path.join(dir, 'secret.key');
      await fs.writeFile(file, 'not cbor');
      const { wallets, errors } = await exportWallets({
        exportSourcePath: file,
        locale: 'en-US',
      });
      expect(wallets).toEqual([]);
      expect(errors).not.toBe('');
    } finally {
      await fs.remove(dir);
    }
  });
});
