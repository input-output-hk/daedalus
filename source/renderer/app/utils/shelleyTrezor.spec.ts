import { PROTO } from '@trezor/connect';
import { Cardano } from '@cardano-sdk/core';
import { toTrezorCertificate } from './shelleyTrezor';
import type { CoinSelectionCertificate } from '../api/transactions/types';

// @trezor/connect transitively pulls an ESM-only @noble/curves build that
// Jest cannot parse; the authenticity module is irrelevant to certificate
// mapping, so stub it (Jest hoists this above the imports) to keep the real
// PROTO enums importable.
jest.mock('@trezor/device-authenticity', () => ({}));

// Same verified vectors as the Ledger spec (round-trip proven via
// Cardano.DRepID.toCredential).
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
  } as CoinSelectionCertificate);

const decodedHash = (vote: string): string =>
  Cardano.DRepID.toCredential(Cardano.DRepID(vote)).hash;

describe('shelleyTrezor cast_vote certificate mapping', () => {
  it('binds a CIP-129 key-hash DRep ID byte-equal to vote.chosenOption', () => {
    const result = toTrezorCertificate(castVote(CIP129_KEY)) as {
      type: number;
      dRep?: { type: number; keyHash?: string; scriptHash?: string };
    };
    expect(result.type).toBe(PROTO.CardanoCertificateType.VOTE_DELEGATION);
    expect(result.dRep).toEqual({
      type: PROTO.CardanoDRepType.KEY_HASH,
      keyHash: KEY_HASH_HEX,
    });
    expect(KEY_HASH_HEX).toBe(decodedHash(CIP129_KEY));
  });

  it('binds a CIP-129 script-hash DRep ID byte-equal to vote.chosenOption', () => {
    const result = toTrezorCertificate(castVote(CIP129_SCRIPT)) as {
      dRep?: { type: number; scriptHash?: string };
    };
    expect(result.dRep).toEqual({
      type: PROTO.CardanoDRepType.SCRIPT_HASH,
      scriptHash: SCRIPT_HASH_HEX,
    });
    expect(SCRIPT_HASH_HEX).toBe(decodedHash(CIP129_SCRIPT));
  });

  it('binds a CIP-105 key-hash DRep ID byte-equal to vote.chosenOption', () => {
    const result = toTrezorCertificate(castVote(CIP105_KEY)) as {
      dRep?: { type: number; keyHash?: string };
    };
    expect(result.dRep).toEqual({
      type: PROTO.CardanoDRepType.KEY_HASH,
      keyHash: KEY_HASH_HEX,
    });
    expect(KEY_HASH_HEX).toBe(decodedHash(CIP105_KEY));
  });

  it('binds a CIP-105 script-hash DRep ID byte-equal to vote.chosenOption', () => {
    const result = toTrezorCertificate(castVote(CIP105_SCRIPT)) as {
      dRep?: { type: number; scriptHash?: string };
    };
    expect(result.dRep).toEqual({
      type: PROTO.CardanoDRepType.SCRIPT_HASH,
      scriptHash: SCRIPT_HASH_HEX,
    });
    expect(SCRIPT_HASH_HEX).toBe(decodedHash(CIP105_SCRIPT));
  });

  it('maps the abstain sentinel to the device ABSTAIN type', () => {
    const result = toTrezorCertificate(castVote('abstain')) as {
      dRep?: { type: number };
    };
    expect(result.dRep).toEqual({ type: PROTO.CardanoDRepType.ABSTAIN });
  });

  it('maps the no_confidence sentinel to the device NO_CONFIDENCE type', () => {
    const result = toTrezorCertificate(castVote('no_confidence')) as {
      dRep?: { type: number };
    };
    expect(result.dRep).toEqual({ type: PROTO.CardanoDRepType.NO_CONFIDENCE });
  });

  it('carries no dRep for non-vote certificates', () => {
    const result = toTrezorCertificate({
      certificateType: 'register_reward_account',
      rewardAccountPath: ['1852H', '1815H', '0H', '2', '0'],
    } as CoinSelectionCertificate) as { dRep?: unknown };
    expect(result.dRep).toBeUndefined();
  });
});
