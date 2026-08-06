import { isSameDRep } from '../../../source/renderer/app/utils/governance/isSameDRep';
import { normalizeDRepIdentity } from '../../../source/renderer/app/utils/governance/normalizeDRepIdentity';
import type { DRepDelegation } from '../../../source/renderer/app/api/wallets/types';

const KEY_CIP129 = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const KEY_CIP105 =
  'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';
const KEY_CREDENTIAL_HEX =
  'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';
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

const drepVote = (id: string): DRepDelegation => ({
  kind: 'drep',
  drep: normalizeDRepIdentity(id),
  source: 'onchain',
});

describe('isSameDRep', () => {
  it('is false for every choice when the wallet has no current vote', () => {
    expect(isSameDRep(KEY_CIP129, null)).toBe(false);
    expect(isSameDRep('abstain', null)).toBe(false);
    expect(isSameDRep('no_confidence', null)).toBe(false);
  });

  it('matches a sentinel only against the same sentinel', () => {
    expect(isSameDRep('abstain', { kind: 'abstain' })).toBe(true);
    expect(isSameDRep('abstain', { kind: 'no_confidence' })).toBe(false);
    expect(isSameDRep('abstain', drepVote(KEY_CIP129))).toBe(false);
    expect(isSameDRep('no_confidence', { kind: 'no_confidence' })).toBe(
      true
    );
    expect(isSameDRep('no_confidence', { kind: 'abstain' })).toBe(false);
  });

  it('is false when a DRep id is compared against a sentinel vote', () => {
    expect(isSameDRep(KEY_CIP129, { kind: 'abstain' })).toBe(false);
    expect(isSameDRep(KEY_CIP129, { kind: 'no_confidence' })).toBe(false);
  });

  it('matches the same DRep across CIP-129 and CIP-105 encodings', () => {
    expect(isSameDRep(KEY_CIP129, drepVote(KEY_CIP129))).toBe(true);
    expect(isSameDRep(KEY_CIP105, drepVote(KEY_CIP129))).toBe(true);
    expect(isSameDRep(KEY_CIP129, drepVote(KEY_CIP105))).toBe(true);
  });

  it('is false for a different DRep', () => {
    expect(isSameDRep(OTHER_KEY_CIP129, drepVote(KEY_CIP129))).toBe(
      false
    );
  });

  it('never equates a key DRep and a script DRep sharing credential bytes', () => {
    expect(isSameDRep(OTHER_KEY_CIP129, drepVote(SCRIPT_CIP129))).toBe(
      false
    );
    expect(isSameDRep(SCRIPT_CIP129, drepVote(OTHER_KEY_CIP129))).toBe(
      false
    );
    expect(isSameDRep(SCRIPT_CIP129, drepVote(SCRIPT_CIP129))).toBe(true);
  });

  it('is false when the stored identity carries no credential hex', () => {
    const withoutHex: DRepDelegation = {
      kind: 'drep',
      drep: { raw: KEY_CIP129, credentialType: 'key' },
      source: 'onchain',
    };
    expect(isSameDRep(KEY_CIP129, withoutHex)).toBe(false);
    expect(isSameDRep(KEY_CIP105, withoutHex)).toBe(false);
  });

  it('is false, and does not throw, when the choice cannot be decoded', () => {
    expect(isSameDRep('', drepVote(KEY_CIP129))).toBe(false);
    expect(isSameDRep(UNDECODABLE_DREP, drepVote(KEY_CIP129))).toBe(
      false
    );
    expect(isSameDRep('not-a-bech32-string', drepVote(KEY_CIP129))).toBe(
      false
    );
  });

  it('leaves the compared identity untouched', () => {
    const currentDRep = drepVote(KEY_CIP129);
    const before = JSON.stringify(currentDRep);
    expect(isSameDRep(KEY_CIP105, currentDRep)).toBe(true);
    expect(JSON.stringify(currentDRep)).toBe(before);
  });
});

describe('isSameDRep letter-case stability', () => {
  const currentDRep: DRepDelegation = {
    kind: 'drep',
    drep: {
      raw: KEY_CIP129,
      cip129: KEY_CIP129,
      cip105: KEY_CIP105,
      credentialHex: KEY_CREDENTIAL_HEX,
      credentialType: 'key',
    },
    source: 'onchain',
  };

  it('matches an all-upper-case bech32 form of the current target', () => {
    expect(isSameDRep(KEY_CIP129.toUpperCase(), currentDRep)).toBe(true);
  });

  it('matches when the stored credential hex is upper-case', () => {
    expect(
      isSameDRep(KEY_CIP129, {
        ...currentDRep,
        drep: {
          ...currentDRep.drep,
          credentialHex: KEY_CREDENTIAL_HEX.toUpperCase(),
        },
      })
    ).toBe(true);
  });

  it('rejects a mixed-case form, which is not a decodable identifier', () => {
    expect(isSameDRep(`D${KEY_CIP129.slice(1)}`, currentDRep)).toBe(
      false
    );
  });
});
