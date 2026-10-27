import { _createWalletFromServerData } from './api';
import { loadRecordedExchange } from '../../../../tests/jest/support/walletReplay';
import { logger } from '../utils/logging';
import type { AdaWallet } from './wallets/types';

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

const recordedWallet = (
  backend: 'upstream' | 'ledger-vote',
  name: string
): AdaWallet => ({
  ...JSON.parse(loadRecordedExchange(backend, name).response.body),
  isLegacy: false,
});

// Bodies below are constructed from the wire shape of delegation.ledger_vote;
// they are not recorded responses.
const DREP_ID = 'drep1ytnsnlmkfuc73jdhydgnud54nnmc06c0ky653pxghspg8dgf9dlan';
const withLedgerVote = (ledgerVote: unknown, isLegacy = false): AdaWallet => {
  const wallet = recordedWallet('ledger-vote', 'wallet-held-rewards');
  return {
    ...wallet,
    isLegacy,
    delegation: { ...wallet.delegation, ledger_vote: ledgerVote as never },
  };
};

describe('_createWalletFromServerData ledger vote', () => {
  beforeEach(() => {
    global.cip30Enabled = false;
    jest.clearAllMocks();
  });

  it('reads unknown when the wallet sends no field', () => {
    expect(
      _createWalletFromServerData(
        recordedWallet('ledger-vote', 'wallet-retired-drep')
      ).ledgerVote
    ).toEqual({ status: 'unknown' });
    expect(
      _createWalletFromServerData(
        recordedWallet('upstream', 'wallet-retired-drep')
      ).ledgerVote
    ).toEqual({ status: 'unknown' });
  });

  it('reads not delegated beside a reward above zero', () => {
    const wallet = _createWalletFromServerData(
      recordedWallet('ledger-vote', 'wallet-held-rewards')
    );
    expect(wallet.ledgerVote).toEqual({ status: 'not_delegated' });
    expect(wallet.reward.gt(0)).toBe(true);
    expect(wallet.hasHeldRewards).toBe(true);
  });

  it('keeps the history target apart from the ledger value', () => {
    const wallet = _createWalletFromServerData(
      recordedWallet('ledger-vote', 'wallet-held-rewards')
    );
    expect(wallet.currentDRep).toMatchObject({
      kind: 'drep',
      drep: { raw: expect.stringMatching(/^drep1/) },
      source: 'onchain',
    });
  });

  it('reads a DRep labelled as the ledger value', () => {
    const { ledgerVote } = _createWalletFromServerData(
      recordedWallet('ledger-vote', 'wallet-drep')
    );
    expect(ledgerVote).toMatchObject({
      status: 'delegated',
      voting: { kind: 'drep', source: 'ledger' },
    });
  });

  it('reads Abstain', () => {
    expect(
      _createWalletFromServerData(
        recordedWallet('ledger-vote', 'wallet-abstain')
      ).ledgerVote
    ).toEqual({ status: 'delegated', voting: { kind: 'abstain' } });
  });

  it('reads No Confidence from a constructed body', () => {
    expect(
      _createWalletFromServerData(
        withLedgerVote({ status: 'delegated', voting: 'no_confidence' })
      ).ledgerVote
    ).toEqual({ status: 'delegated', voting: { kind: 'no_confidence' } });
  });

  it.each([
    ['null', null],
    ['a string', 'delegated'],
    ['an array', []],
  ])('reads unknown for %s without a warning', (_label, body) => {
    expect(
      _createWalletFromServerData(withLedgerVote(body)).ledgerVote
    ).toEqual({ status: 'unknown' });
    expect(logger.warn).not.toHaveBeenCalled();
  });

  it.each([
    ['delegated without voting', { status: 'delegated' }],
    [
      'delegated with an unparseable target',
      { status: 'delegated', voting: 'x' },
    ],
    [
      'delegated with a non-string target',
      { status: 'delegated', voting: { kind: 'abstain' } },
    ],
    [
      'not delegated with a target',
      { status: 'not_delegated', voting: 'abstain' },
    ],
    [
      'delegated with an extra key',
      { status: 'delegated', voting: 'abstain', x: 1 },
    ],
    ['an unrecognised status', { status: 'pending' }],
    ['no status', { voting: 'abstain' }],
  ])('reads unknown and warns once for %s', (_label, body) => {
    expect(
      _createWalletFromServerData(withLedgerVote(body)).ledgerVote
    ).toEqual({ status: 'unknown' });
    const ledgerWarnings = (logger.warn as jest.Mock).mock.calls.filter(
      ([message]) => message.includes('parseLedgerVote')
    );
    expect(ledgerWarnings).toHaveLength(1);
  });

  it('reads unknown for a legacy wallet whatever it carries', () => {
    expect(
      _createWalletFromServerData(
        withLedgerVote({ status: 'not_delegated' }, true)
      ).ledgerVote
    ).toEqual({ status: 'unknown' });
  });

  it('never puts the target or an unbounded status in a warning', () => {
    _createWalletFromServerData(
      withLedgerVote({ status: 'delegated', voting: DREP_ID.slice(0, -1) })
    );
    _createWalletFromServerData(
      withLedgerVote({ status: `drep1leak${DREP_ID}`, voting: DREP_ID })
    );
    const logged = JSON.stringify((logger.warn as jest.Mock).mock.calls);
    expect(logged).not.toContain(DREP_ID.slice(5, 30));
    expect(logged).not.toContain('drep1leak');
  });
});
