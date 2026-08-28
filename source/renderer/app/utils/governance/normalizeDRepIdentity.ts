import { bech32 } from 'bech32';
import type { DRepIdentity } from '../../../../common/types/governance.types';

const CIP129_KEY_HEADER = 0x22;
const CIP129_SCRIPT_HEADER = 0x23;
const CREDENTIAL_BYTE_LENGTH = 28;

const toHex = (bytes: number[]): string =>
  bytes.map((byte) => byte.toString(16).padStart(2, '0')).join('');

/**
 * Pure decoder for DRep identifiers: CIP-129 `drep1…` (29-byte payload with a
 * 0x22 key / 0x23 script header) and CIP-105 `drep_vkh1…` / `drep_script1…`
 * (bare 28-byte credential). Unknown HRP, length mismatch, bad checksum, or
 * bad header returns null; never throws, never logs.
 */
export function normalizeDRepIdentity(raw: string): DRepIdentity | null {
  let prefix: string;
  let bytes: number[];
  try {
    const decoded = bech32.decode(raw);
    prefix = decoded.prefix;
    bytes = bech32.fromWords(decoded.words);
  } catch {
    return null;
  }
  if (prefix === 'drep') {
    if (bytes.length !== CREDENTIAL_BYTE_LENGTH + 1) {
      return null;
    }
    const header = bytes[0];
    if (header !== CIP129_KEY_HEADER && header !== CIP129_SCRIPT_HEADER) {
      return null;
    }
    const credentialType = header === CIP129_KEY_HEADER ? 'key' : 'script';
    const credential = bytes.slice(1);
    const cip105Hrp = credentialType === 'key' ? 'drep_vkh' : 'drep_script';
    return {
      raw,
      cip129: raw,
      cip105: bech32.encode(cip105Hrp, bech32.toWords(credential)),
      credentialHex: toHex(credential),
      credentialType,
    };
  }
  if (prefix === 'drep_vkh' || prefix === 'drep_script') {
    if (bytes.length !== CREDENTIAL_BYTE_LENGTH) {
      return null;
    }
    const credentialType = prefix === 'drep_vkh' ? 'key' : 'script';
    const header =
      credentialType === 'key' ? CIP129_KEY_HEADER : CIP129_SCRIPT_HEADER;
    return {
      raw,
      cip129: bech32.encode('drep', bech32.toWords([header, ...bytes])),
      cip105: raw,
      credentialHex: toHex(bytes),
      credentialType,
    };
  }
  return null;
}
