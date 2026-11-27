import BigNumber from 'bignumber.js';
import AdaApi from './api';
import { request } from './utils/request';
import type { RequestConfig } from './common/types';

jest.mock('./utils/request', () => ({ request: jest.fn() }));
jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));
jest.mock('../ipc/collateral', () => ({
  walletInputSelectionChannel: { request: jest.fn(async () => ({})) },
}));

const requestMock = request as unknown as jest.Mock;
const CONSTRUCTED = 'c0';
const SIGNED = 's1';
const TRANSACTION_ID = 'ab'.repeat(32);
const DREP = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';

type Step = 'construct' | 'sign' | 'submit' | 'oneShot' | 'byron';
const stepOf = (path: string): Step => {
  if (path.endsWith('/transactions-construct')) return 'construct';
  if (path.endsWith('/transactions-sign')) return 'sign';
  if (path.endsWith('/transactions-submit')) return 'submit';
  if (path.startsWith('/v2/byron-wallets/')) return 'byron';
  return 'oneShot';
};

const serverTransaction = {
  id: TRANSACTION_ID,
  amount: { quantity: 1000000, unit: 'lovelace' },
  fee: { quantity: 200000, unit: 'lovelace' },
  deposit_taken: { quantity: 0, unit: 'lovelace' },
  deposit_returned: { quantity: 0, unit: 'lovelace' },
  inserted_at: {
    time: new Date('2026-01-01T00:00:00.000Z'),
    block: { slot_number: 1, epoch_number: 2 },
  },
  depth: { quantity: 3, unit: 'block' },
  direction: 'outgoing',
  inputs: [],
  outputs: [],
  withdrawals: [],
  status: 'pending',
};

const nonNullRewards = {
  code: 'non_null_rewards',
  message: 'Make sure to withdraw your rewards first.',
};
const shortPassphrase = {
  code: 'bad_request',
  message: 'passphrase is too short',
};

const refusal = {
  code: 'created_invalid_transaction',
  message:
    'The submitted transaction was rejected by the local node. Here is the error: ConwayWdrlNotDelegatedToDRep',
};

const fail = (step: Step, body: unknown) => {
  requestMock.mockImplementation(async (options: { path: string }) => {
    const current = stepOf(options.path);
    if (current === step) throw body;
    if (current === 'construct') return { transaction: CONSTRUCTED };
    if (current === 'sign') return { transaction: SIGNED };
    if (current === 'oneShot' || current === 'byron') return serverTransaction;
    return { id: TRANSACTION_ID };
  });
};

const calls = () =>
  requestMock.mock.calls.map(([options, , body]) => ({
    step: stepOf(options.path),
    method: options.method,
    path: options.path,
    body,
  }));

const caught = async (work: () => Promise<unknown>) => {
  try {
    await work();
  } catch (error) {
    return error as { id: string; defaultMessage: string };
  }
  throw new Error('Expected the call to fail');
};

describe('AdaApi software transaction submission', () => {
  const originalCapability = global.cip30Enabled;
  const api = new AdaApi(false, {} as RequestConfig);
  const payment = {
    walletId: 'wallet',
    address: 'addr_test1',
    amount: 1000000,
    passphrase: 'password',
    isLegacy: false,
    assets: null as null,
    hasAssetsRemainingAfterTransaction: false,
  };

  beforeEach(() => {
    global.cip30Enabled = false;
    requestMock.mockReset();
    fail('none' as Step, null);
  });
  afterEach(() => {
    global.cip30Enabled = originalCapability;
  });

  describe('createTransaction', () => {
    it('constructs, signs with the passphrase and submits a Shelley payment', async () => {
      await expect(api.createTransaction(payment)).resolves.toEqual({
        id: TRANSACTION_ID,
      });
      const [construct, sign, submit, ...rest] = calls();
      expect(rest).toEqual([]);
      expect([construct.step, sign.step, submit.step]).toEqual([
        'construct',
        'sign',
        'submit',
      ]);
      expect(construct.path).toBe('/v2/wallets/wallet/transactions-construct');
      expect(construct.body).toEqual({
        encoding: 'base16',
        payments: [
          {
            address: 'addr_test1',
            amount: { quantity: 1000000, unit: 'lovelace' },
          },
        ],
        withdrawal: 'self',
      });
      expect(sign.body).toEqual({
        transaction: CONSTRUCTED,
        passphrase: 'password',
        encoding: 'base16',
      });
      expect(submit.body).toEqual({ transaction: SIGNED });
    });

    it('carries native assets into the constructed payment', async () => {
      const assets = [
        {
          policy_id: 'p'.repeat(56),
          asset_name: '41',
          quantity: new BigNumber(3),
        },
      ];
      await api.createTransaction({ ...payment, assets } as never);
      expect(calls()[0].body.payments[0].assets).toEqual(assets);
    });

    it('keeps Byron wallets on the one-shot route', async () => {
      await api.createTransaction({ ...payment, isLegacy: true });
      expect(calls().map(({ step }) => step)).toEqual(['byron']);
    });

    it('keeps the CIP-30 build on the one-shot route', async () => {
      global.cip30Enabled = true;
      await api.createTransaction(payment);
      expect(calls().map(({ step }) => step)).toEqual(['oneShot']);
    });

    it('stops at a refused submission without any further call', async () => {
      fail('submit', refusal);
      await caught(() => api.createTransaction(payment));
      expect(calls().map(({ step }) => step)).toEqual([
        'construct',
        'sign',
        'submit',
      ]);
    });

    it.each([
      ['a node refusal from submit', 'submit', refusal],
      [
        'a wrong passphrase from sign',
        'sign',
        {
          code: 'wrong_encryption_passphrase',
          message: 'The given encryption passphrase doesn’t match.',
        },
      ],
      [
        'a short passphrase from sign',
        'sign',
        { code: 'bad_request', message: 'passphrase is too short' },
      ],
      [
        'not enough money from construct',
        'construct',
        { code: 'not_enough_money', message: 'Not enough money' },
      ],
      [
        'an oversized transaction from construct',
        'construct',
        { code: 'transaction_is_too_big', message: 'too big' },
      ],
      [
        'a network failure from submit',
        'submit',
        { code: 'network_unreachable', message: 'unreachable' },
      ],
    ])(
      'maps %s to the message the one-shot call produced',
      async (_name, step, body) => {
        fail('byron', body);
        const oneShot = await caught(() =>
          api.createTransaction({ ...payment, isLegacy: true })
        );
        // The Byron route shares the catch chain of the one-shot Shelley call.
        requestMock.mockReset();
        fail(step as Step, body);
        const split = await caught(() => api.createTransaction(payment));
        expect(split.id).toBe(oneShot.id);
        expect(split.defaultMessage).toBe(oneShot.defaultMessage);
      }
    );
  });

  describe('joinStakePool', () => {
    it('constructs, signs and submits a pool join', async () => {
      await expect(
        api.joinStakePool({
          walletId: 'wallet',
          stakePoolId: 'pool1',
          passphrase: 'password',
        })
      ).resolves.toEqual({ id: TRANSACTION_ID });
      const [construct, sign, submit] = calls();
      expect(calls().map(({ step }) => step)).toEqual([
        'construct',
        'sign',
        'submit',
      ]);
      expect(construct.body).toEqual({
        encoding: 'base16',
        delegations: [{ join: { pool: 'pool1', stake_key_index: '0H' } }],
      });
      expect(sign.body.passphrase).toBe('password');
      expect(submit.body).toEqual({ transaction: SIGNED });
    });

    it('adds an Abstain vote when the wallet is not voting', async () => {
      await api.joinStakePool({
        walletId: 'wallet',
        stakePoolId: 'pool1',
        passphrase: 'password',
        voteAbstain: true,
      });
      expect(calls()[0].body).toEqual({
        encoding: 'base16',
        delegations: [{ join: { pool: 'pool1', stake_key_index: '0H' } }],
        vote: 'abstain',
      });
    });

    it('maps a wrong passphrase as before', async () => {
      fail('sign', {
        code: 'bad_request',
        message: 'passphrase is too short',
      });
      const error = await caught(() =>
        api.joinStakePool({
          walletId: 'wallet',
          stakePoolId: 'pool1',
          passphrase: 'x',
        })
      );
      expect(error.id).toBe('api.errors.IncorrectPasswordError');
    });

    it('surfaces a node refusal as an error', async () => {
      fail('submit', refusal);
      await caught(() =>
        api.joinStakePool({
          walletId: 'wallet',
          stakePoolId: 'pool1',
          passphrase: 'password',
        })
      );
      expect(calls().map(({ step }) => step)).toEqual([
        'construct',
        'sign',
        'submit',
      ]);
    });
  });

  describe('quitStakePool', () => {
    it('constructs, signs and submits a pool quit with the reward withdrawal', async () => {
      await expect(
        api.quitStakePool({ walletId: 'wallet', passphrase: 'password' })
      ).resolves.toEqual({ id: TRANSACTION_ID });
      expect(calls().map(({ step }) => step)).toEqual([
        'construct',
        'sign',
        'submit',
      ]);
      expect(calls()[0].body).toEqual({
        encoding: 'base16',
        delegations: [{ quit: { stake_key_index: '0H' } }],
        withdrawal: 'self',
      });
    });

    it('retries once with the quit route when construct refuses for non-null rewards', async () => {
      fail('construct', nonNullRewards);
      const result = await api.quitStakePool({
        walletId: 'wallet',
        passphrase: 'password',
      });
      expect(result.id).toBe(TRANSACTION_ID);
      expect(
        calls().map(({ step, method, path }) => [step, method, path])
      ).toEqual([
        ['construct', 'POST', '/v2/wallets/wallet/transactions-construct'],
        ['oneShot', 'DELETE', '/v2/stake-pools/*/wallets/wallet'],
      ]);
      expect(calls()[1].body).toEqual({ passphrase: 'password' });
    });

    it('maps an error of the quit route like the earlier quit', async () => {
      requestMock.mockImplementation(async (options: { path: string }) => {
        if (stepOf(options.path) === 'construct') throw nonNullRewards;
        throw shortPassphrase;
      });
      const error = await caught(() =>
        api.quitStakePool({ walletId: 'wallet', passphrase: 'x' })
      );
      expect(error.id).toBe('api.errors.IncorrectPasswordError');
    });

    it.each([
      ['not_delegating_to', 'not delegating'],
      ['wrong_encryption_passphrase', 'wrong passphrase'],
      ['not_enough_money', 'not enough'],
    ])(
      'does not use the quit route when construct refuses with %s',
      async (code, message) => {
        fail('construct', { code, message });
        await caught(() =>
          api.quitStakePool({ walletId: 'wallet', passphrase: 'password' })
        );
        expect(calls().map(({ step }) => step)).toEqual(['construct']);
      }
    );
  });

  describe('delegateVotes', () => {
    it('constructs the vote without a withdrawal, then signs and submits', async () => {
      await expect(
        api.delegateVotes({
          dRepId: DREP,
          walletId: 'wallet',
          passphrase: 'password',
        })
      ).resolves.toEqual({ id: TRANSACTION_ID });
      expect(calls().map(({ step }) => step)).toEqual([
        'construct',
        'sign',
        'submit',
      ]);
      expect(calls()[0].body).toEqual({ encoding: 'base16', vote: DREP });
      expect(calls()[1].body.passphrase).toBe('password');
    });

    it('surfaces a node refusal as an error', async () => {
      fail('submit', refusal);
      await caught(() =>
        api.delegateVotes({
          dRepId: 'abstain',
          walletId: 'wallet',
          passphrase: 'password',
        })
      );
    });
  });

  describe('createVotingRegistrationTransaction', () => {
    const registration = {
      walletId: 'wallet',
      address: 'addr_test1',
      addressHex: 'aa',
      amount: 1000000,
      passphrase: 'password',
      votingKey: 'bb',
      stakeKey: 'cc',
      signature: 'dd',
      absoluteSlotNumber: 7,
    };

    it('constructs the registration with its metadata, then signs and submits', async () => {
      await expect(
        api.createVotingRegistrationTransaction(registration)
      ).resolves.toEqual({ id: TRANSACTION_ID });
      const [construct, sign] = calls();
      expect(calls().map(({ step }) => step)).toEqual([
        'construct',
        'sign',
        'submit',
      ]);
      expect(construct.body.encoding).toBe('base16');
      expect(Object.keys(construct.body.metadata)).toEqual(['61284', '61285']);
      expect(construct.body.passphrase).toBeUndefined();
      expect(sign.body.passphrase).toBe('password');
    });

    it('keeps the CIP-30 build on the one-shot route', async () => {
      global.cip30Enabled = true;
      await api.createVotingRegistrationTransaction(registration);
      expect(calls().map(({ step }) => step)).toEqual(['oneShot']);
    });
  });

  describe('submitHardwareTransaction', () => {
    it('submits the device-signed transaction to the wallet submit route', async () => {
      await expect(
        api.submitHardwareTransaction({
          walletId: 'wallet',
          signedTransaction: '84a0',
        })
      ).resolves.toEqual({ id: TRANSACTION_ID });
      expect(calls()).toEqual([
        {
          step: 'submit',
          method: 'POST',
          path: '/v2/wallets/wallet/transactions-submit',
          body: { transaction: '84a0' },
        },
      ]);
    });
  });
});
