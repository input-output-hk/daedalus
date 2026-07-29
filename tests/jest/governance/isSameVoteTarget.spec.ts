import { isSameVoteTarget } from '../../../source/renderer/app/utils/governance/isSameVoteTarget';
import { normalizeDRepIdentity } from '../../../source/renderer/app/utils/governance/normalizeDRepIdentity';
import type { WalletVotingTarget } from '../../../source/renderer/app/api/wallets/types';

const KEY_CIP129 = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const KEY_CIP105 =
  'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';
// These two carry the same 28 credential bytes under a 0x22 key header and a
// 0x23 script header, so only credentialType separates them.
const OTHER_KEY_CIP129 =
  'drep1yg83utfufddxj7y8j6jmfs7ju8cq783d839456tcs7t2tdq5ah2yv';
const SCRIPT_CIP129 =
  'drep1yv83utfufddxj7y8j6jmfs7ju8cq783d839456tcs7t2tdq508myt';
// HRP `drep` over a bare 28-byte credential: the form gate accepts it, the
// decoder rejects it.
const UNDECODABLE_DREP =
  'drep15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94cymwqu9';

const drepVote = (id: string): WalletVotingTarget => ({
  kind: 'drep',
  drep: normalizeDRepIdentity(id),
  source: 'onchain',
});

describe('isSameVoteTarget', () => {
  it('is false for every choice when the wallet has no current vote', () => {
    expect(isSameVoteTarget(KEY_CIP129, null)).toBe(false);
    expect(isSameVoteTarget('abstain', null)).toBe(false);
    expect(isSameVoteTarget('no_confidence', null)).toBe(false);
  });

  it('matches a sentinel only against the same sentinel', () => {
    expect(isSameVoteTarget('abstain', { kind: 'abstain' })).toBe(true);
    expect(isSameVoteTarget('abstain', { kind: 'no_confidence' })).toBe(false);
    expect(isSameVoteTarget('abstain', drepVote(KEY_CIP129))).toBe(false);
    expect(isSameVoteTarget('no_confidence', { kind: 'no_confidence' })).toBe(
      true
    );
    expect(isSameVoteTarget('no_confidence', { kind: 'abstain' })).toBe(false);
  });

  it('is false when a DRep id is compared against a sentinel vote', () => {
    expect(isSameVoteTarget(KEY_CIP129, { kind: 'abstain' })).toBe(false);
    expect(isSameVoteTarget(KEY_CIP129, { kind: 'no_confidence' })).toBe(false);
  });

  it('matches the same DRep across CIP-129 and CIP-105 encodings', () => {
    expect(isSameVoteTarget(KEY_CIP129, drepVote(KEY_CIP129))).toBe(true);
    expect(isSameVoteTarget(KEY_CIP105, drepVote(KEY_CIP129))).toBe(true);
    expect(isSameVoteTarget(KEY_CIP129, drepVote(KEY_CIP105))).toBe(true);
  });

  it('is false for a different DRep', () => {
    expect(isSameVoteTarget(OTHER_KEY_CIP129, drepVote(KEY_CIP129))).toBe(
      false
    );
  });

  it('never equates a key DRep and a script DRep sharing credential bytes', () => {
    expect(isSameVoteTarget(OTHER_KEY_CIP129, drepVote(SCRIPT_CIP129))).toBe(
      false
    );
    expect(isSameVoteTarget(SCRIPT_CIP129, drepVote(OTHER_KEY_CIP129))).toBe(
      false
    );
    expect(isSameVoteTarget(SCRIPT_CIP129, drepVote(SCRIPT_CIP129))).toBe(true);
  });

  it('is false when the stored identity carries no credential hex', () => {
    const withoutHex: WalletVotingTarget = {
      kind: 'drep',
      drep: { raw: KEY_CIP129, credentialType: 'key' },
      source: 'onchain',
    };
    expect(isSameVoteTarget(KEY_CIP129, withoutHex)).toBe(false);
    expect(isSameVoteTarget(KEY_CIP105, withoutHex)).toBe(false);
  });

  it('is false, and does not throw, when the choice cannot be decoded', () => {
    expect(isSameVoteTarget('', drepVote(KEY_CIP129))).toBe(false);
    expect(isSameVoteTarget(UNDECODABLE_DREP, drepVote(KEY_CIP129))).toBe(
      false
    );
    expect(isSameVoteTarget('not-a-bech32-string', drepVote(KEY_CIP129))).toBe(
      false
    );
  });

  it('leaves the compared identity untouched', () => {
    const currentVote = drepVote(KEY_CIP129);
    const before = JSON.stringify(currentVote);
    expect(isSameVoteTarget(KEY_CIP105, currentVote)).toBe(true);
    expect(JSON.stringify(currentVote)).toBe(before);
  });
});
