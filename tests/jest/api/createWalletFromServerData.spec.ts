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

  it('maps voting_and_delegating: pool target AND votingTarget populated', () => {
    const wallet = _createWalletFromServerData(
      loadFixture('wallet-delegating-and-voting.json')
    );
    expect(wallet.delegatedStakePoolId).toBe(POOL_ID);
    expect(wallet.delegationStakePoolStatus).toBe('voting_and_delegating');
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

// A second pool id, distinct from POOL_ID, for pending-change scenarios.
const POOL_ID_NEXT = 'pool1qvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsx7n01m3';

const atEpoch = (epochNumber: number) => ({
  epoch_number: epochNumber,
  epoch_start_time: '2026-08-10T21:44:51Z',
});

describe('_createWalletFromServerData pending delegation mapping', () => {
  beforeEach(() => {
    mockedWarn.mockClear();
  });

  it('keeps a pool-delegated wallet delegating when the only pending change is a vote', () => {
    // PR #3355: a DRep-only certificate made a pool-delegated wallet read as
    // undelegated, because the voting-only pending entry carries no target and
    // Wallet.isDelegating prefers lastDelegationStakePoolStatus over the
    // active status.
    const wallet = _createWalletFromServerData(
      withDelegation({
        active: { status: 'delegating', target: POOL_ID },
        next: [
          { status: 'voting', voting: KEY_CIP105, changes_at: atEpoch(412) },
        ],
      })
    );
    expect(wallet.delegatedStakePoolId).toBe(POOL_ID);
    expect(wallet.lastDelegatedStakePoolId).toBeNull();
    expect(wallet.lastDelegationStakePoolStatus).toBeNull();
    expect(wallet.isDelegating).toBe(true);
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
  });

  it('surfaces a pending pool change and a pending vote change together', () => {
    const wallet = _createWalletFromServerData(
      withDelegation({
        active: { status: 'not_delegating' },
        next: [
          {
            status: 'delegating',
            target: POOL_ID_NEXT,
            changes_at: atEpoch(412),
          },
          { status: 'voting', voting: KEY_CIP105, changes_at: atEpoch(412) },
        ],
      })
    );
    expect(wallet.lastDelegatedStakePoolId).toBe(POOL_ID_NEXT);
    expect(wallet.lastDelegationStakePoolStatus).toBe('delegating');
    expect(wallet.isDelegating).toBe(true);
    expect(toJS(wallet.votingTarget)).not.toBeNull();
  });

  it('reads the pending pool change from the latest epoch, not the array tail', () => {
    const wallet = _createWalletFromServerData(
      withDelegation({
        active: { status: 'delegating', target: POOL_ID },
        next: [
          {
            status: 'delegating',
            target: POOL_ID_NEXT,
            changes_at: atEpoch(413),
          },
          { status: 'delegating', target: POOL_ID, changes_at: atEpoch(412) },
        ],
      })
    );
    expect(wallet.lastDelegatedStakePoolId).toBe(POOL_ID_NEXT);
  });

  it('lets a pending not_delegating entry clear the pool, unlike a voting entry', () => {
    const wallet = _createWalletFromServerData(
      withDelegation({
        active: { status: 'delegating', target: POOL_ID },
        next: [{ status: 'not_delegating', changes_at: atEpoch(412) }],
      })
    );
    expect(wallet.lastDelegationStakePoolStatus).toBe('not_delegating');
    expect(wallet.lastDelegatedStakePoolId).toBeNull();
    expect(wallet.isDelegating).toBe(false);
  });

  it('keeps the pending pool change when a later vote entry follows it', () => {
    // PR #3355: a combined certificate appends both entries; reading only the
    // final one dropped the stake delegation from the Delegation Center.
    const wallet = _createWalletFromServerData(
      withDelegation({
        active: { status: 'not_delegating' },
        next: [
          {
            status: 'delegating',
            target: POOL_ID_NEXT,
            changes_at: atEpoch(412),
          },
          { status: 'voting', voting: 'abstain', changes_at: atEpoch(412) },
        ],
      })
    );
    expect(wallet.lastDelegatedStakePoolId).toBe(POOL_ID_NEXT);
    expect(wallet.isDelegating).toBe(true);
    expect(toJS(wallet.votingTarget)).toEqual({ kind: 'abstain' });
  });

  it('ignores an empty next array', () => {
    const wallet = _createWalletFromServerData(
      withDelegation({
        active: { status: 'delegating', target: POOL_ID },
        next: [],
      })
    );
    expect(wallet.lastDelegatedStakePoolId).toBeNull();
    expect(wallet.lastDelegationStakePoolStatus).toBeNull();
    expect(wallet.isDelegating).toBe(true);
  });
});
