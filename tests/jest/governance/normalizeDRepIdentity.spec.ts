import { bech32 } from 'bech32';
import { normalizeDRepIdentity } from '../../../source/renderer/app/utils/governance/normalizeDRepIdentity';

// Checksum-verified vector set: each CIP-129 id decodes to a 29-byte payload
// (0x22 key / 0x23 script header) sharing its credential bytes with the
// matching CIP-105 form, so cross-encoding assertions are exact.
const KEY_CIP129 = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const KEY_CIP105 =
  'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';
const KEY_CREDENTIAL_HEX =
  'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';
const SCRIPT_CIP129 =
  'drep1yv83utfufddxj7y8j6jmfs7ju8cq783d839456tcs7t2tdq508myt';
const SCRIPT_CIP105 =
  'drep_script1pu0z60zttf5h3puk5k6v85hp7q83utfufddxj7y8j6jmg6wjkzc';
const SCRIPT_CREDENTIAL_HEX =
  '0f1e2d3c4b5a69788796a5b4c3d2e1f00f1e2d3c4b5a69788796a5b4';
// Deprecated pre-Conway form: HRP `drep` over a bare 28-byte credential.
const DEPRECATED_DREP_28_BYTE =
  'drep15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94cymwqu9';
const POOL_ID = 'pool1qvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsx6m90l2';

describe('normalizeDRepIdentity', () => {
  it('normalizes a CIP-129 key DRep id', () => {
    expect(normalizeDRepIdentity(KEY_CIP129)).toEqual({
      raw: KEY_CIP129,
      cip129: KEY_CIP129,
      cip105: KEY_CIP105,
      credentialHex: KEY_CREDENTIAL_HEX,
      credentialType: 'key',
    });
  });

  it('normalizes a CIP-129 script DRep id', () => {
    expect(normalizeDRepIdentity(SCRIPT_CIP129)).toEqual({
      raw: SCRIPT_CIP129,
      cip129: SCRIPT_CIP129,
      cip105: SCRIPT_CIP105,
      credentialHex: SCRIPT_CREDENTIAL_HEX,
      credentialType: 'script',
    });
  });

  it('normalizes a CIP-105 key-hash DRep id (drep_vkh)', () => {
    expect(normalizeDRepIdentity(KEY_CIP105)).toEqual({
      raw: KEY_CIP105,
      cip129: KEY_CIP129,
      cip105: KEY_CIP105,
      credentialHex: KEY_CREDENTIAL_HEX,
      credentialType: 'key',
    });
  });

  it('normalizes a CIP-105 script-hash DRep id (drep_script)', () => {
    expect(normalizeDRepIdentity(SCRIPT_CIP105)).toEqual({
      raw: SCRIPT_CIP105,
      cip129: SCRIPT_CIP129,
      cip105: SCRIPT_CIP105,
      credentialHex: SCRIPT_CREDENTIAL_HEX,
      credentialType: 'script',
    });
  });

  it('round-trips drep1 -> cip105 -> drep1 losslessly for key and script', () => {
    const keyIdentity = normalizeDRepIdentity(KEY_CIP129);
    expect(normalizeDRepIdentity(keyIdentity.cip105).cip129).toBe(KEY_CIP129);
    const scriptIdentity = normalizeDRepIdentity(SCRIPT_CIP129);
    expect(normalizeDRepIdentity(scriptIdentity.cip105).cip129).toBe(
      SCRIPT_CIP129
    );
  });

  it('never equates a key DRep and a script DRep sharing credential bytes', () => {
    const words = bech32.toWords(new Array(28).fill(7));
    const key = normalizeDRepIdentity(bech32.encode('drep_vkh', words));
    const script = normalizeDRepIdentity(bech32.encode('drep_script', words));
    expect(key.credentialHex).toBe(script.credentialHex);
    expect(key.credentialType).toBe('key');
    expect(script.credentialType).toBe('script');
    expect(key.cip129).not.toBe(script.cip129);
  });

  it('returns null, without throwing, for invalid or foreign input', () => {
    const invalidInputs = [
      '',
      'abstain',
      'no_confidence',
      'not-a-bech32-string',
      POOL_ID,
      DEPRECATED_DREP_28_BYTE,
      `${KEY_CIP129.slice(0, -1)}x`,
    ];
    invalidInputs.forEach((value) => {
      expect(normalizeDRepIdentity(value)).toBeNull();
    });
  });

  it('returns null for a drep payload with an unknown CIP-129 header byte', () => {
    const badHeader = bech32.encode(
      'drep',
      bech32.toWords([0x99, ...new Array(28).fill(7)])
    );
    expect(normalizeDRepIdentity(badHeader)).toBeNull();
  });

  it('returns null for a drep_vkh payload of the wrong length', () => {
    const wrongLength = bech32.encode(
      'drep_vkh',
      bech32.toWords(new Array(29).fill(7))
    );
    expect(normalizeDRepIdentity(wrongLength)).toBeNull();
  });
});
