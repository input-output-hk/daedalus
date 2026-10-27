import BigNumber from 'bignumber.js';
import { configure, observable, runInAction } from 'mobx';
import WalletsStore from './WalletsStore';
import type { UndelegationVoteStep } from './WalletsStore';
import type Wallet from '../domains/Wallet';
import type { LedgerVoteDelegation } from '../api/wallets/types';

const deepStub: any = new Proxy(() => undefined, {
  get: () => deepStub,
  apply: () => deepStub,
});

const FEE = new BigNumber('0.18');

type Setup = {
  store: WalletsStore;
  wallet: any;
  voting: { initializeVPDelegationTx: jest.Mock; delegateVotes: jest.Mock };
  recent: { transactions: Array<{ id: string; state: string }> };
};

const setup = ({
  isHardwareWallet = false,
  ledgerVote = { status: 'not_delegated' } as LedgerVoteDelegation,
} = {}): Setup => {
  const store = new WalletsStore(deepStub, deepStub, deepStub);
  const wallet = observable({
    id: 'wallet-a',
    isLegacy: false,
    isHardwareWallet,
    reward: new BigNumber(5),
    ledgerVote,
  });
  const voting = {
    initializeVPDelegationTx: jest.fn(async () => ({
      success: true as const,
      fees: FEE,
    })),
    delegateVotes: jest.fn(async () => ({
      success: true as const,
      transactionId: 'vote-tx',
    })),
  };
  const recent = observable({ transactions: [] });
  store.configure({
    app: { currentRoute: '/wallets' },
    voting,
    transactions: {
      _getTransactionsRecentRequest: () => ({ result: recent }),
    },
  } as any);
  store.walletsRequest.result = [wallet as unknown as Wallet];
  // Start only the reaction under test, through the registration setup makes.
  jest.useFakeTimers();
  store.setup();
  jest.useRealTimers();
  store._reactions
    .filter(({ reaction }) => reaction === store._advanceUndelegationVoteSteps)
    .forEach((reaction) => reaction.start());
  return { store, wallet, voting, recent: recent as Setup['recent'] };
};

describe('WalletsStore undelegation vote step', () => {
  const originalCapability = global.cip30Enabled;
  let started: Array<WalletsStore> = [];

  beforeAll(() => {
    configure({ enforceActions: 'observed' });
  });
  afterEach(() => {
    global.cip30Enabled = originalCapability;
    started.forEach((store) => store.teardown());
    started = [];
  });

  const create = (options?: Parameters<typeof setup>[0]) => {
    const result = setup(options);
    started.push(result.store);
    return result;
  };

  it('registers the advance reaction at setup', () => {
    const { store } = create();
    expect(
      store._reactions.some(
        ({ reaction }) => reaction === store._advanceUndelegationVoteSteps
      )
    ).toBe(true);
  });

  it('holds no step for a wallet nothing was prepared for', () => {
    const { store } = create();
    expect(store.undelegationVoteStepFor('wallet-a')).toBeNull();
  });

  it('prepares a software mainnet step with the Abstain fee', async () => {
    global.cip30Enabled = false;
    const { store, wallet, voting } = create();
    const pending = store.prepareUndelegationVote('wallet-a');
    expect(store.undelegationVoteStepFor('wallet-a')).toEqual({
      status: 'estimating',
    });
    await pending;
    expect(voting.initializeVPDelegationTx).toHaveBeenCalledWith({
      chosenOption: 'abstain',
      wallet,
    });
    expect(store.undelegationVoteStepFor('wallet-a')).toEqual({
      status: 'ready',
      fee: FEE,
    });
  });

  it('submits a software step with the password and waits for the ledger', async () => {
    global.cip30Enabled = false;
    const { store, wallet, voting } = create();
    await store.prepareUndelegationVote('wallet-a');
    await store.submitUndelegationVote('wallet-a', 'secret');
    expect(voting.delegateVotes).toHaveBeenCalledWith({
      chosenOption: 'abstain',
      wallet,
      passphrase: 'secret',
    });
    expect(store.undelegationVoteStepFor('wallet-a')).toEqual({
      status: 'awaiting_ledger',
      transactionId: 'vote-tx',
    });
    runInAction('test', () => {
      wallet.ledgerVote = { status: 'delegated', voting: { abstain: true } };
    });
    expect(store.undelegationVoteStepFor('wallet-a')).toBeNull();
  });

  it('runs a hardware mainnet step without a password', async () => {
    global.cip30Enabled = false;
    const { store, wallet, voting } = create({ isHardwareWallet: true });
    await store.prepareUndelegationVote('wallet-a');
    await store.submitUndelegationVote('wallet-a');
    expect(voting.initializeVPDelegationTx).toHaveBeenCalledWith({
      chosenOption: 'abstain',
      wallet,
    });
    expect(voting.delegateVotes).toHaveBeenCalledWith({
      chosenOption: 'abstain',
      wallet,
      passphrase: undefined,
    });
    expect(store.undelegationVoteStepFor('wallet-a')?.status).toBe(
      'awaiting_ledger'
    );
  });

  it('prepares a cip30 step with no fee and prices nothing', async () => {
    global.cip30Enabled = true;
    const { store, voting } = create();
    await store.prepareUndelegationVote('wallet-a');
    expect(voting.initializeVPDelegationTx).not.toHaveBeenCalled();
    expect(store.undelegationVoteStepFor('wallet-a')).toEqual({
      status: 'ready',
      fee: null,
    });
  });

  it.each(['failed', 'expired'])(
    'fails the wait with vote_not_confirmed when the transaction is %s',
    async (state) => {
      global.cip30Enabled = false;
      const { store, recent } = create();
      await store.prepareUndelegationVote('wallet-a');
      await store.submitUndelegationVote('wallet-a', 'secret');
      runInAction('test', () => {
        recent.transactions.push({ id: 'other', state: 'failed' });
      });
      expect(store.undelegationVoteStepFor('wallet-a')?.status).toBe(
        'awaiting_ledger'
      );
      runInAction('test', () => {
        recent.transactions.push({ id: 'vote-tx', state });
      });
      expect(store.undelegationVoteStepFor('wallet-a')).toEqual({
        status: 'failed',
        errorCode: 'vote_not_confirmed',
      });
    }
  );

  it('keeps waiting while the transaction is pending', async () => {
    global.cip30Enabled = false;
    const { store, recent } = create();
    await store.prepareUndelegationVote('wallet-a');
    await store.submitUndelegationVote('wallet-a', 'secret');
    runInAction('test', () => {
      recent.transactions.push({ id: 'vote-tx', state: 'pending' });
    });
    expect(store.undelegationVoteStepFor('wallet-a')?.status).toBe(
      'awaiting_ledger'
    );
  });

  it.each(['same_vote', 'no_utxos_available', 'not_enough_money', 'generic'])(
    'maps the estimate failure %s onto the step',
    async (errorCode) => {
      global.cip30Enabled = false;
      const { store, voting } = create();
      voting.initializeVPDelegationTx.mockResolvedValueOnce({
        success: false,
        errorCode,
      } as any);
      await store.prepareUndelegationVote('wallet-a');
      expect(store.undelegationVoteStepFor('wallet-a')).toEqual({
        status: 'failed',
        errorCode,
      });
    }
  );

  it.each(['wrong_encryption_passphrase', 'generic'])(
    'maps the submission failure %s onto the step',
    async (errorCode) => {
      global.cip30Enabled = false;
      const { store, voting } = create();
      voting.delegateVotes.mockResolvedValueOnce({
        success: false,
        errorCode,
      } as any);
      await store.prepareUndelegationVote('wallet-a');
      await store.submitUndelegationVote('wallet-a', 'secret');
      expect(store.undelegationVoteStepFor('wallet-a')).toEqual({
        status: 'failed',
        errorCode,
      });
    }
  );

  it('submits only from a ready step', async () => {
    const { store, voting } = create();
    await store.submitUndelegationVote('wallet-a', 'secret');
    expect(voting.delegateVotes).not.toHaveBeenCalled();
    expect(store.undelegationVoteStepFor('wallet-a')).toBeNull();
  });

  it('cancels a ready step but not a waiting one', async () => {
    global.cip30Enabled = false;
    const { store } = create();
    await store.prepareUndelegationVote('wallet-a');
    store.cancelUndelegationVote('wallet-a');
    expect(store.undelegationVoteStepFor('wallet-a')).toBeNull();

    await store.prepareUndelegationVote('wallet-a');
    await store.submitUndelegationVote('wallet-a', 'secret');
    store.cancelUndelegationVote('wallet-a');
    expect(store.undelegationVoteStepFor('wallet-a')?.status).toBe(
      'awaiting_ledger'
    );
  });

  it('drops an estimate that finishes after a cancel', async () => {
    global.cip30Enabled = false;
    const { store } = create();
    const pending = store.prepareUndelegationVote('wallet-a');
    store.cancelUndelegationVote('wallet-a');
    await pending;
    expect(store.undelegationVoteStepFor('wallet-a')).toBeNull();
  });

  it('does not restart a step that is submitting or waiting', async () => {
    global.cip30Enabled = false;
    const { store, voting } = create();
    await store.prepareUndelegationVote('wallet-a');
    await store.submitUndelegationVote('wallet-a', 'secret');
    const before: UndelegationVoteStep | null =
      store.undelegationVoteStepFor('wallet-a');
    await store.prepareUndelegationVote('wallet-a');
    expect(voting.initializeVPDelegationTx).toHaveBeenCalledTimes(1);
    expect(store.undelegationVoteStepFor('wallet-a')).toEqual(before);
  });
});
