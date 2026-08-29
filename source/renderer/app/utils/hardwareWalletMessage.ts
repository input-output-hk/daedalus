import { prepareCip8Request } from '../../../common/cardano/cip8';
import type { Cip8ExpectedRequest } from '../../../common/cardano/cip8';
import type {
  ContextOwnership,
  DappNetwork,
} from '../../../common/cardano/transactionContext';
import type {
  HardwareMessageAddress,
  HardwareMessageRequest,
} from '../../../common/types/hardware-wallets.types';

const invalid = (): never => {
  throw new Error('Invalid hardware message request');
};

const path = (value: readonly number[]): readonly number[] =>
  Object.freeze([...value]);

const ownedKey = (
  ownership: readonly ContextOwnership[],
  expected: Cip8ExpectedRequest
): ContextOwnership => {
  const matches = ownership.filter(
    (candidate) =>
      candidate.credentialKind === expected.credentialKind &&
      candidate.credential === expected.credential.toString('hex') &&
      candidate.ownership === 'owned_key'
  );
  if (matches.length !== 1) return invalid();
  const match = matches[0];
  const role =
    expected.credentialKind === 'payment'
      ? [0, 1]
      : [expected.credentialKind === 'stake' ? 2 : 3];
  if (
    match.derivationPath.length !== 5 ||
    !role.includes(match.derivationPath[3]) ||
    (expected.credentialKind !== 'payment' && match.derivationPath[4] !== 0)
  )
    return invalid();
  return match;
};

const variableUint = (
  source: Buffer,
  start: number
): Readonly<{ value: number; next: number }> => {
  let value = 0;
  let offset = start;
  do {
    if (
      offset >= source.length ||
      value > Math.floor(Number.MAX_SAFE_INTEGER / 128)
    )
      return invalid();
    value = value * 128 + (source[offset] & 0x7f);
  } while ((source[offset++] & 0x80) !== 0);
  return { value, next: offset };
};

const addressBinding = (
  expected: Cip8ExpectedRequest,
  signer: ContextOwnership,
  ownership: readonly ContextOwnership[]
): HardwareMessageAddress => {
  if (expected.credentialKind === 'drep')
    return Object.freeze({
      kind: 'key_hash' as const,
      value: expected.credential.toString('hex'),
    });

  const bytes = Buffer.from(expected.address, 'hex');
  const addressType = bytes[0] >> 4;
  const signerPath = path(signer.derivationPath);
  if (addressType === 6)
    return Object.freeze({
      kind: 'address' as const,
      value: expected.address,
      addressType,
      paymentPath: signerPath,
    });
  if (addressType === 14)
    return Object.freeze({
      kind: 'address' as const,
      value: expected.address,
      addressType,
      stakePath: signerPath,
    });
  if (addressType === 4) {
    const block = variableUint(bytes, 29);
    const transaction = variableUint(bytes, block.next);
    const certificate = variableUint(bytes, transaction.next);
    if (certificate.next !== bytes.length) return invalid();
    return Object.freeze({
      kind: 'address' as const,
      value: expected.address,
      addressType,
      paymentPath: signerPath,
      pointer: Object.freeze({
        blockIndex: block.value,
        txIndex: transaction.value,
        certificateIndex: certificate.value,
      }),
    });
  }
  if (addressType !== 0 && addressType !== 2) return invalid();
  const stakeCredential = bytes.subarray(29, 57).toString('hex');
  if (bytes.length !== 57) return invalid();
  if (addressType === 2)
    return Object.freeze({
      kind: 'address' as const,
      value: expected.address,
      addressType,
      paymentPath: signerPath,
      stakeScriptHash: stakeCredential,
    });
  const stake = ownership.find(
    (candidate) =>
      candidate.credentialKind === 'stake' &&
      candidate.credential === stakeCredential &&
      candidate.ownership === 'owned_key'
  );
  return Object.freeze({
    kind: 'address' as const,
    value: expected.address,
    addressType,
    paymentPath: signerPath,
    ...(stake
      ? { stakePath: path(stake.derivationPath) }
      : { stakeKeyHash: stakeCredential }),
  });
};

export const prepareHardwareMessage = (
  address: string,
  payload: string,
  network: DappNetwork,
  ownership: readonly ContextOwnership[],
  drepCredential?: string
): HardwareMessageRequest => {
  const expected = prepareCip8Request(address, payload, {
    networkId: network.networkId,
    drepCredential,
  });
  const signer = ownedKey(ownership, expected);
  return Object.freeze({
    address: addressBinding(expected, signer, ownership),
    credentialKind: expected.credentialKind,
    credential: expected.credential.toString('hex'),
    protectedAddress: expected.protectedAddress.toString('hex'),
    payload: expected.payload.toString('hex'),
    path: path(signer.derivationPath),
    network,
  });
};
