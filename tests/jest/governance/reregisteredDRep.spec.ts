import type AdaApiClass from '../../../source/renderer/app/api/api';
import type GovernanceStoreClass from '../../../source/renderer/app/stores/GovernanceStore';
import { isSameDRep } from '../../../source/renderer/app/utils/governance/isSameDRep';
import {
  effectiveCurrentVote,
  voteDelegationState,
} from '../../../source/renderer/app/utils/governance/voteDelegationState';
import {
  loadRecordedExchange,
  provideRequestGlobals,
  startWalletReplay,
} from '../support/walletReplay';
import type { WalletReplay } from '../support/walletReplay';

jest.mock('../../../source/renderer/app/utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

provideRequestGlobals();
const api = require('../../../source/renderer/app/api/api');

const AdaApi = api.default as typeof AdaApiClass;
const { _createWalletFromServerData } = api;
const GovernanceStore =
  require('../../../source/renderer/app/stores/GovernanceStore')
    .default as typeof GovernanceStoreClass;

// A wallet delegated to DRep A, which retired and registered again. The wallet
// that reads the ledger's delegation reports none; the one that does not still
// names A as the vote it sent.
const registered = loadRecordedExchange('ledger-vote', 'drep-registered-again');
const withField = loadRecordedExchange(
  'ledger-vote',
  'wallet-reregistered-drep'
);
const withoutField = loadRecordedExchange(
  'upstream',
  'wallet-reregistered-drep'
);

const walletFrom = (exchange: typeof withField) => {
  global.cip30Enabled = false;
  return _createWalletFromServerData(JSON.parse(exchange.response.body));
};

describe('A DRep that retired and registered again', () => {
  let replay: WalletReplay | null = null;
  afterEach(async () => {
    await replay?.close();
    replay = null;
  });

  const lookedUp = async () => {
    replay = await startWalletReplay([
      {
        method: 'GET',
        path: /^\/v2\/dreps\/drep1[a-z0-9]+$/,
        reply: registered,
      },
    ]);
    const store = new GovernanceStore(
      {
        ada: new AdaApi(false, replay.config),
        localStorage: { getDRepFavorites: jest.fn().mockResolvedValue([]) },
      } as any,
      {} as any,
      {} as any
    );
    store.configure({
      networkStatus: { localTip: { epoch: 0 }, isNodeInSync: true },
      router: { location: { pathname: '/governance' } },
    } as any);
    return store;
  };

  it('ends the delegation and says a new registration did not restore it', async () => {
    const wallet = walletFrom(withField);
    expect(wallet.ledgerVote).toEqual({ status: 'not_delegated' });
    const store = await lookedUp();
    const state = voteDelegationState(wallet, (id) => store.drepLookup(id));
    expect(state).toMatchObject({ kind: 'ended', registeredAgain: false });
    if (state.kind !== 'ended') throw new Error('expected an ended delegation');

    await store.resolveDRep(state.drep.cip129 ?? state.drep.raw);

    expect(
      voteDelegationState(wallet, (id) => store.drepLookup(id))
    ).toMatchObject({ kind: 'ended', registeredAgain: true });
  });

  it('is not the current vote, so the DRep can be chosen again', async () => {
    const wallet = walletFrom(withField);
    const store = await lookedUp();
    const state = voteDelegationState(wallet, (id) => store.drepLookup(id));
    if (state.kind !== 'ended') throw new Error('expected an ended delegation');

    const current = effectiveCurrentVote(state);

    expect(current).toBeNull();
    expect(isSameDRep(state.drep.cip129 ?? state.drep.raw, current)).toBe(
      false
    );
  });

  it('reads as the current vote where the wallet cannot report the ledger delegation', () => {
    const wallet = walletFrom(withoutField);
    expect(wallet.ledgerVote).toEqual({ status: 'unknown' });
    const state = voteDelegationState(wallet, () => ({ status: 'idle' }));
    expect(state).toMatchObject({ kind: 'current', source: 'history' });
  });
});
