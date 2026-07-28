import path from 'path';
import { toJS } from 'mobx';
import { _createWalletFromServerData } from '../../../source/renderer/app/api/api';
import { logger } from '../../../source/renderer/app/utils/logging';
import type { AdaWallet } from '../../../source/renderer/app/api/wallets/types';

// The real renderer logger writes through global.electronLog, which does not
// exist in the Jest environment; the mock also records the sanitized warning.
jest.mock('../../../source/renderer/app/utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
    warn: jest.fn(),
  },
}));

// request.ts constructs a global.https.Agent at module scope; the Electron
// preload provides that global, the Jest environment does not. Mapper tests
// never issue requests, so the module is replaced wholesale.
jest.mock('../../../source/renderer/app/api/utils/request', () => ({
  request: jest.fn(),
}));

const mockedWarn = logger.warn as jest.Mock;

// Checksum-verified vector set shared with the cv-1 fixtures.
const KEY_CIP129 = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const KEY_CIP105 =
  'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';
const KEY_CREDENTIAL_HEX =
  'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';
const POOL_ID = 'pool1qvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsx6m90l2';

const loadFixture = (name: string): AdaWallet =>
  require(
    path.join(__dirname, '../../mocks/wallets', name)
  ) as unknown as AdaWallet;

const withDelegation = (delegation: unknown): AdaWallet =>
  ({
    ...(loadFixture('wallet-voting-drep.json') as Record<string, unknown>),
    delegation,
  }) as unknown as AdaWallet;

describe('_createWalletFromServerData voting mapping', () => {
  beforeEach(() => {
    mockedWarn.mockClear();
  });

  it('maps a voting-only DRep wallet: votingTarget populated, pool id null', () => {
    const wallet = _createWalletFromServerData(
      loadFixture('wallet-voting-drep.json')
    );
    expect(wallet.delegatedStakePoolId).toBeNull();
    expect(wallet.delegationStakePoolStatus).toBe('voting');
    expect(toJS(wallet.votingTarget)).toEqual({
      kind: 'drep',
      drep: {
        raw: KEY_CIP129,
        cip129: KEY_CIP129,
        cip105: KEY_CIP105,
        credentialHex: KEY_CREDENTIAL_HEX,
        credentialType: 'key',
      },
      source: 'onchain',
    });
    expect(mockedWarn).not.toHaveBeenCalled();
  });

  it('maps delegating_and_voting: pool target AND votingTarget populated', () => {
    const wallet = _createWalletFromServerData(
      loadFixture('wallet-delegating-and-voting.json')
    );
    expect(wallet.delegatedStakePoolId).toBe(POOL_ID);
    expect(wallet.delegationStakePoolStatus).toBe('delegating_and_voting');
    expect(toJS(wallet.votingTarget)).toEqual({
      kind: 'drep',
      drep: {
        raw: KEY_CIP105,
        cip129: KEY_CIP129,
        cip105: KEY_CIP105,
        credentialHex: KEY_CREDENTIAL_HEX,
        credentialType: 'key',
      },
      source: 'onchain',
    });
    expect(mockedWarn).not.toHaveBeenCalled();
  });

  it('maps the abstain sentinel with no pool id', () => {
    const wallet = _createWalletFromServerData(
      loadFixture('wallet-voting-abstain.json')
    );
    expect(wallet.delegatedStakePoolId).toBeNull();
    expect(toJS(wallet.votingTarget)).toEqual({ kind: 'abstain' });
    expect(mockedWarn).not.toHaveBeenCalled();
  });

  it('maps the no_confidence sentinel with no pool id', () => {
    const wallet = _createWalletFromServerData(
      loadFixture('wallet-voting-no-confidence.json')
    );
    expect(wallet.delegatedStakePoolId).toBeNull();
    expect(toJS(wallet.votingTarget)).toEqual({ kind: 'no_confidence' });
    expect(mockedWarn).not.toHaveBeenCalled();
  });

  it('keeps the pending next delegation intact alongside the vote target', () => {
    const wallet = _createWalletFromServerData(
      loadFixture('wallet-delegating-and-voting.json')
    );
    expect(wallet.pendingDelegations).toHaveLength(1);
    expect(wallet.lastDelegationStakePoolStatus).toBe('delegating');
    expect(wallet.lastDelegatedStakePoolId).toBe(POOL_ID);
  });

  it('yields votingTarget null for status voting without active.voting and never parses active.target', () => {
    const wallet = _createWalletFromServerData(
      withDelegation({
        active: { status: 'voting', target: POOL_ID },
        next: [],
      })
    );
    expect(wallet.votingTarget).toBeNull();
    expect(wallet.delegatedStakePoolId).toBeNull();
    expect(mockedWarn).not.toHaveBeenCalled();
  });

  it('degrades an unknown HRP to null with a sanitized HRP-only warning', () => {
    const wallet = _createWalletFromServerData(
      withDelegation({
        active: { status: 'voting', voting: POOL_ID },
        next: [],
      })
    );
    expect(wallet.votingTarget).toBeNull();
    expect(mockedWarn).toHaveBeenCalledTimes(1);
    const [message, data] = mockedWarn.mock.calls[0];
    expect(message).toBe('AdaApi::parseVoting unrecognized voting target');
    expect(data).toEqual({ hrp: 'pool' });
    expect(JSON.stringify(mockedWarn.mock.calls)).not.toContain(POOL_ID);
  });

  it('collapses a malformed voting value to the fixed invalid token', () => {
    const wallet = _createWalletFromServerData(
      withDelegation({
        active: { status: 'voting', voting: 'not-a-bech32-string' },
        next: [],
      })
    );
    expect(wallet.votingTarget).toBeNull();
    const [, data] = mockedWarn.mock.calls[0];
    expect(data).toEqual({ hrp: 'invalid' });
    expect(JSON.stringify(mockedWarn.mock.calls)).not.toContain(
      'not-a-bech32-string'
    );
  });

  it('maps delegating and not_delegating byte-identically to today', () => {
    const delegating = _createWalletFromServerData(
      withDelegation({
        active: { status: 'delegating', target: POOL_ID },
        next: [],
      })
    );
    expect(delegating.delegatedStakePoolId).toBe(POOL_ID);
    expect(delegating.votingTarget).toBeNull();
    const notDelegating = _createWalletFromServerData(
      withDelegation({ active: { status: 'not_delegating' }, next: [] })
    );
    expect(notDelegating.delegatedStakePoolId).toBeNull();
    expect(notDelegating.votingTarget).toBeNull();
  });
});
