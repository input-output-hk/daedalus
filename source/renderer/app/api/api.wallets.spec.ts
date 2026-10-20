import AdaApi, { _createWalletFromServerData } from './api';
import { request } from './utils/request';
import { request as requestV0 } from './utils/requestV0';
import { updateWallet } from './wallets/requests/updateWallet';
import type { AdaWallet } from './wallets/types';
import { WalletUnits } from '../domains/Wallet';

jest.mock('./utils/request', () => ({ request: jest.fn() }));
jest.mock('./utils/requestV0', () => ({ request: jest.fn() }));
jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

const upstreamWallet = (): AdaWallet => ({
  id: 'ab'.repeat(20),
  name: 'Upstream wallet',
  address_pool_gap: 20,
  balance: {
    available: { quantity: 3000000, unit: WalletUnits.LOVELACE },
    total: { quantity: 3000000, unit: WalletUnits.LOVELACE },
    reward: { quantity: 0, unit: WalletUnits.LOVELACE },
  },
  assets: { available: [], total: [] },
  delegation: { active: { status: 'not_delegating' } },
  state: { status: 'ready' },
  discovery: 'sequential',
  isLegacy: false,
});

const config = {
  hostname: 'localhost',
  port: 8090,
  ca: new Uint8Array(),
  cert: new Uint8Array(),
  key: new Uint8Array(),
};

describe('upstream wallet response and request contracts', () => {
  beforeEach(() => {
    global.cip30Enabled = false;
    jest.clearAllMocks();
    (request as jest.Mock).mockResolvedValue(upstreamWallet());
    (requestV0 as jest.Mock).mockResolvedValue(upstreamWallet());
  });

  it('loads upstream and ignores saved fork state rather than interpreting it', () => {
    expect(
      _createWalletFromServerData(upstreamWallet()).singleAddressMode
    ).toBe(false);
    expect(
      _createWalletFromServerData({
        ...upstreamWallet(),
        single_address_mode: true,
      }).singleAddressMode
    ).toBe(false);
  });

  it.each([undefined, null, 'true', 1])(
    'rejects a malformed pilot field: %s',
    async (value) => {
      global.cip30Enabled = true;
      await expect(
        Promise.resolve().then(() =>
          _createWalletFromServerData({
            ...upstreamWallet(),
            single_address_mode: value,
          } as unknown as AdaWallet)
        )
      ).rejects.toMatchObject({ code: 'wallet_internal_error' });
    }
  );

  it.each([true, false])(
    'retains validated pilot mode %s',
    (single_address_mode) => {
      global.cip30Enabled = true;
      expect(
        _createWalletFromServerData({
          ...upstreamWallet(),
          single_address_mode,
        }).singleAddressMode
      ).toBe(single_address_mode);
    }
  );

  it('lists upstream wallets through the real serializers', async () => {
    (request as jest.Mock)
      .mockResolvedValueOnce([upstreamWallet()])
      .mockResolvedValueOnce([]);
    const previous = Object.getOwnPropertyDescriptor(global, 'daedalus');
    Object.defineProperty(global, 'daedalus', {
      configurable: true,
      value: {
        api: {
          localStorage: {
            getHardwareWalletsLocalData: jest.fn().mockResolvedValue({}),
          },
        },
      },
    });
    try {
      const wallets = await new AdaApi(false, config).getWallets();
      expect(wallets).toHaveLength(1);
      expect(wallets[0].singleAddressMode).toBe(false);
      expect(request).toHaveBeenNthCalledWith(
        1,
        expect.objectContaining({ method: 'GET', path: '/v2/wallets' })
      );
    } finally {
      if (previous) Object.defineProperty(global, 'daedalus', previous);
      else Reflect.deleteProperty(global, 'daedalus');
    }
  });

  it('creates, restores, imports, pairs and renames without a fork response field', async () => {
    const api = new AdaApi(false, config);
    const wallets = [
      await api.createWallet({
        name: 'New',
        mnemonic: ['word'],
        spendingPassword: 'password',
      }),
      await api.restoreWallet({
        walletName: 'Restored',
        recoveryPhrase: ['word'],
        spendingPassword: 'password',
      }),
      await api.importWalletFromKey({
        filePath: '/wallet.key',
        spendingPassword: 'password',
      }),
      await api.importWalletFromFile({
        filePath: '/wallet.json',
        walletName: 'Imported',
        spendingPassword: 'password',
      }),
      await api.createHardwareWallet({
        walletName: 'Hardware',
        accountPublicKey: 'ab',
      }),
      await api.updateWallet({
        walletId: upstreamWallet().id,
        isLegacy: false,
        name: 'Renamed',
      }),
    ];
    expect(wallets.every((wallet) => !wallet.singleAddressMode)).toBe(true);
    expect(request).toHaveBeenLastCalledWith(
      expect.objectContaining({
        method: 'PUT',
        path: `/v2/wallets/${upstreamWallet().id}`,
      }),
      {},
      { name: 'Renamed' }
    );
    for (const [, , body] of (request as jest.Mock).mock.calls) {
      expect(body).not.toHaveProperty('single_address_mode');
    }
  });

  it('rejects direct mode updates before transport, while pilot updates preserve false', async () => {
    await expect(
      updateWallet(config, { walletId: 'wallet', singleAddressMode: true })
    ).rejects.toThrow('Single-address mode is unavailable');
    await expect(
      new AdaApi(false, config).updateWallet({
        walletId: 'wallet',
        isLegacy: false,
        singleAddressMode: false,
      })
    ).rejects.toBeDefined();
    expect(request).not.toHaveBeenCalled();
    global.cip30Enabled = true;
    await updateWallet(config, {
      walletId: 'wallet',
      singleAddressMode: false,
    });
    expect(request).toHaveBeenCalledWith(
      expect.anything(),
      {},
      { single_address_mode: false }
    );
  });
});
