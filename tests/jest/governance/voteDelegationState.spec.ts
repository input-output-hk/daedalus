import { normalizeDRepIdentity } from '../../../source/renderer/app/utils/governance/normalizeDRepIdentity';
import {
  drepLookupKey,
  effectiveCurrentVote,
  voteDelegationState,
} from '../../../source/renderer/app/utils/governance/voteDelegationState';
import type { DRepLookup } from '../../../source/renderer/app/utils/governance/voteDelegationState';
import type {
  DRepDelegation,
  LedgerVoteDelegation,
} from '../../../source/renderer/app/api/wallets/types';

const X = normalizeDRepIdentity(
  'drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras'
)!;
const Y = normalizeDRepIdentity(
  'drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n'
)!;
const dRepX: DRepDelegation = { kind: 'drep', drep: X, source: 'onchain' };
const dRepY: DRepDelegation = { kind: 'drep', drep: Y, source: 'ledger' };
const abstain: DRepDelegation = { kind: 'abstain' };

const entry = {
  drepId: drepLookupKey(X),
  votingPower: null,
  status: 'active' as const,
  drepActivity: null,
  anchor: null,
  verifiedName: null,
  doNotList: false,
};
const idle = (): DRepLookup => ({ status: 'idle' });
const found = (): DRepLookup => ({ status: 'found', entry });
const status =
  (value: 'pending' | 'not_found' | 'failed') => (): DRepLookup => ({
    status: value,
  });

const wallet = (
  ledgerVote: LedgerVoteDelegation,
  votingTarget: DRepDelegation | null,
  isLegacy = false
) => ({ isLegacy, votingTarget, ledgerVote });

describe('voteDelegationState', () => {
  it('reports no delegation for a legacy wallet', () => {
    expect(
      voteDelegationState(
        wallet({ status: 'delegated', voting: dRepY }, dRepX, true),
        found
      )
    ).toEqual({ kind: 'none' });
  });

  it('takes the ledger delegation as current whatever the history says', () => {
    expect(
      voteDelegationState(
        wallet({ status: 'delegated', voting: dRepY }, dRepX),
        status('not_found')
      )
    ).toEqual({ kind: 'current', voting: dRepY, source: 'ledger' });
    expect(
      voteDelegationState(
        wallet({ status: 'delegated', voting: abstain }, null),
        idle
      )
    ).toEqual({ kind: 'current', voting: abstain, source: 'ledger' });
  });

  it.each([
    ['found', found, true],
    ['idle', idle, false],
    ['pending', status('pending'), false],
    ['not_found', status('not_found'), false],
    ['failed', status('failed'), false],
  ])(
    'reports an ended delegation when the ledger has none and the history names a DRep (lookup %s)',
    (_name, lookup, registeredAgain) => {
      expect(
        voteDelegationState(wallet({ status: 'not_delegated' }, dRepX), lookup)
      ).toEqual({ kind: 'ended', drep: X, registeredAgain });
    }
  );

  it('reports none when the ledger has no delegation and the history names no DRep', () => {
    expect(
      voteDelegationState(wallet({ status: 'not_delegated' }, abstain), idle)
    ).toEqual({ kind: 'none' });
    expect(
      voteDelegationState(wallet({ status: 'not_delegated' }, null), idle)
    ).toEqual({ kind: 'none' });
    expect(
      voteDelegationState(
        wallet({ status: 'not_delegated' }, { kind: 'no_confidence' }),
        found
      )
    ).toEqual({ kind: 'none' });
  });

  it('reports a DRep missing from the registry when the ledger field is unknown', () => {
    expect(
      voteDelegationState(
        wallet({ status: 'unknown' }, dRepX),
        status('not_found')
      )
    ).toEqual({ kind: 'not_in_registry', drep: X });
  });

  it.each([
    ['idle', idle],
    ['pending', status('pending')],
    ['failed', status('failed')],
    ['found', found],
  ])(
    'takes the history as current when the ledger field is unknown (lookup %s)',
    (_name, lookup) => {
      expect(
        voteDelegationState(wallet({ status: 'unknown' }, dRepX), lookup)
      ).toEqual({ kind: 'current', voting: dRepX, source: 'history' });
    }
  );

  it('takes a history that is not a DRep as current when the ledger field is unknown', () => {
    expect(
      voteDelegationState(
        wallet({ status: 'unknown' }, abstain),
        status('not_found')
      )
    ).toEqual({ kind: 'current', voting: abstain, source: 'history' });
  });

  it('reports none when the ledger field is unknown and there is no history', () => {
    expect(
      voteDelegationState(wallet({ status: 'unknown' }, null), found)
    ).toEqual({ kind: 'none' });
  });

  it('treats a wallet without the field as unknown', () => {
    expect(
      voteDelegationState({ isLegacy: false, votingTarget: dRepX } as any, idle)
    ).toEqual({ kind: 'current', voting: dRepX, source: 'history' });
  });
});

describe('effectiveCurrentVote', () => {
  it('returns the vote of a current delegation and null otherwise', () => {
    expect(
      effectiveCurrentVote({ kind: 'current', voting: dRepX, source: 'ledger' })
    ).toBe(dRepX);
    expect(effectiveCurrentVote({ kind: 'none' })).toBeNull();
    expect(
      effectiveCurrentVote({ kind: 'ended', drep: X, registeredAgain: false })
    ).toBeNull();
    expect(
      effectiveCurrentVote({ kind: 'not_in_registry', drep: X })
    ).toBeNull();
  });
});
