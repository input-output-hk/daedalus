import { blake2b } from 'blakejs';
import {
  encodeVKeyWitnessSet,
  verifyVKeyWitness,
  WitnessSetError,
} from '../../../common/cardano/witnessSet';
import type {
  HardwareExactTransaction,
  HardwareTransactionWitnessResponse,
} from '../../../common/types/hardware-wallets.types';

const invalid = (): never => {
  throw new WitnessSetError();
};

export const verifyHardwareTransactionWitnesses = (
  exact: HardwareExactTransaction,
  response: HardwareTransactionWitnessResponse
): string => {
  if (response.bodyHash !== exact.bodyHash) return invalid();
  const witnesses = response.witnesses.map(({ publicKey, signature }) => {
    if (
      !/^[0-9a-f]{64}$/u.test(publicKey) ||
      !/^[0-9a-f]{128}$/u.test(signature)
    )
      return invalid();
    const value = {
      publicKey: Buffer.from(publicKey, 'hex'),
      signature: Buffer.from(signature, 'hex'),
    };
    verifyVKeyWitness(Buffer.from(exact.bodyHash, 'hex'), value);
    return {
      ...value,
      keyHash: Buffer.from(blake2b(value.publicKey, undefined, 28)).toString(
        'hex'
      ),
    };
  });
  const returned = witnesses.map(({ keyHash }) => keyHash);
  if (new Set(returned).size !== returned.length) return invalid();
  const expected = [...exact.witnesses.requestedDeviceKeyHashes].sort();
  if (
    [...returned].sort().join(',') !== expected.join(',') ||
    returned.some((keyHash) =>
      exact.witnesses.preExistingKeyHashes.includes(keyHash)
    )
  )
    return invalid();
  if (
    !exact.partialSign &&
    exact.witnesses.requiredKeyHashes.some(
      (keyHash) =>
        !exact.witnesses.preExistingKeyHashes.includes(keyHash) &&
        !returned.includes(keyHash)
    )
  )
    return invalid();
  return encodeVKeyWitnessSet(witnesses).toString('hex');
};
