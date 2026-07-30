import { DRepParamsType } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { Cardano } from '@cardano-sdk/core';
import { toLedgerCertificate } from './shelleyLedger';
import type { CoinSelectionCertificate } from '../api/transactions/types';

// Vectors generated from the fixed credential hashes below via
// Cardano.DRepID.cip129FromCredential / cip105FromCredential and verified
// round-trip with Cardano.DRepID.toCredential.
const KEY_HASH_HEX = 'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';
const SCRIPT_HASH_HEX =
  '0f1e2d3c4b5a69788796a5b4c3d2e1f00f1e2d3c4b5a69788796a5b4';
const CIP129_KEY = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const CIP129_SCRIPT =
  'drep1yv83utfufddxj7y8j6jmfs7ju8cq783d839456tcs7t2tdq508myt';
const CIP105_KEY = 'drep15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94cymwqu9';
const CIP105_SCRIPT =
  'drep_script1pu0z60zttf5h3puk5k6v85hp7q83utfufddxj7y8j6jmg6wjkzc';

const castVote = (vote: string): CoinSelectionCertificate =>
  ({
    certificateType: 'cast_vote',
    rewardAccountPath: ['1852H', '1815H', '0H', '2', '0'],
    vote,
  }) as CoinSelectionCertificate;

// The on-device identity is the credential the device derives from the vote
// string; it must be byte-equal to the credential decoded from chosenOption.
const decodedHash = (vote: string): string =>
  Cardano.DRepID.toCredential(Cardano.DRepID(vote)).hash;

describe('shelleyLedger cast_vote certificate mapping', () => {
  it('binds a CIP-129 key-hash DRep ID byte-equal to vote.chosenOption', () => {
    const result = toLedgerCertificate(castVote(CIP129_KEY));
    expect(result.params.dRep).toEqual({
      type: DRepParamsType.KEY_HASH,
      keyHashHex: KEY_HASH_HEX,
    });
    expect(KEY_HASH_HEX).toBe(decodedHash(CIP129_KEY));
  });

  it('binds a CIP-129 script-hash DRep ID byte-equal to vote.chosenOption', () => {
    const result = toLedgerCertificate(castVote(CIP129_SCRIPT));
    expect(result.params.dRep).toEqual({
      type: DRepParamsType.SCRIPT_HASH,
      scriptHashHex: SCRIPT_HASH_HEX,
    });
    expect(SCRIPT_HASH_HEX).toBe(decodedHash(CIP129_SCRIPT));
  });

  it('binds a CIP-105 key-hash DRep ID byte-equal to vote.chosenOption', () => {
    const result = toLedgerCertificate(castVote(CIP105_KEY));
    expect(result.params.dRep).toEqual({
      type: DRepParamsType.KEY_HASH,
      keyHashHex: KEY_HASH_HEX,
    });
    expect(KEY_HASH_HEX).toBe(decodedHash(CIP105_KEY));
  });

  it('binds a CIP-105 script-hash DRep ID byte-equal to vote.chosenOption', () => {
    const result = toLedgerCertificate(castVote(CIP105_SCRIPT));
    expect(result.params.dRep).toEqual({
      type: DRepParamsType.SCRIPT_HASH,
      scriptHashHex: SCRIPT_HASH_HEX,
    });
    expect(SCRIPT_HASH_HEX).toBe(decodedHash(CIP105_SCRIPT));
  });

  it('maps the abstain sentinel to the device ABSTAIN type', () => {
    expect(toLedgerCertificate(castVote('abstain')).params.dRep).toEqual({
      type: DRepParamsType.ABSTAIN,
    });
  });

  it('maps the no_confidence sentinel to the device NO_CONFIDENCE type', () => {
    expect(toLedgerCertificate(castVote('no_confidence')).params.dRep).toEqual({
      type: DRepParamsType.NO_CONFIDENCE,
    });
  });

  it('leaves dRep undefined for non-vote certificates', () => {
    const result = toLedgerCertificate({
      certificateType: 'register_reward_account',
      rewardAccountPath: ['1852H', '1815H', '0H', '2', '0'],
    } as CoinSelectionCertificate);
    expect(result.params.dRep).toBeUndefined();
  });

  it('derives the on-device credential from vote alone, ignoring display-only fields', () => {
    const withDisplayFields = {
      ...castVote(CIP129_KEY),
      verifiedName: 'Daedalus Test DRep',
    } as CoinSelectionCertificate;

    expect(toLedgerCertificate(withDisplayFields).params.dRep).toEqual(
      toLedgerCertificate(castVote(CIP129_KEY)).params.dRep
    );
    expect(toLedgerCertificate(withDisplayFields).params.dRep).toEqual({
      type: DRepParamsType.KEY_HASH,
      keyHashHex: decodedHash(CIP129_KEY),
    });
  });
});
