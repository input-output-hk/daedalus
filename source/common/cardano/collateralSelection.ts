import { Cardano } from '@cardano-sdk/core';

import { invalidRequest } from '../cip30/errors';
import { parseCborItem } from './cborSlices';
import type { Cip30Utxo } from './cip30Serialization';

const MAX_UINT64 = BigInt('18446744073709551615');

const invalid = (): never => {
  throw invalidRequest();
};

const canonicalCoin = (value: bigint): Buffer => {
  if (value < BigInt(0) || value > MAX_UINT64) return invalid();
  if (value < BigInt(24)) return Buffer.from([Number(value)]);
  if (value <= BigInt(0xff)) return Buffer.from([0x18, Number(value)]);
  if (value <= BigInt(0xffff)) {
    const result = Buffer.alloc(3);
    result[0] = 0x19;
    result.writeUInt16BE(Number(value), 1);
    return result;
  }
  if (value <= BigInt(0xffffffff)) {
    const result = Buffer.alloc(5);
    result[0] = 0x1a;
    result.writeUInt32BE(Number(value), 1);
    return result;
  }
  const result = Buffer.alloc(9);
  result[0] = 0x1b;
  result.writeBigUInt64BE(value, 1);
  return result;
};

export const decodeCip30Coin = (hex: string): bigint => {
  try {
    if (!/^(?:[0-9a-f]{2})+$/.test(hex)) return invalid();
    const bytes = Buffer.from(hex, 'hex');
    const item = parseCborItem(bytes);
    if (
      item.major !== 0 ||
      item.value === undefined ||
      item.span.end !== bytes.length ||
      !canonicalCoin(item.value).equals(bytes)
    )
      return invalid();
    return item.value;
  } catch {
    return invalid();
  }
};

const outpoint = (utxo: Cip30Utxo): string =>
  `${
    utxo.context.outpoint.transactionId
  }:${utxo.context.outpoint.index.toString().padStart(10, '0')}`;

const paymentKeyTypes = new Set<Cardano.AddressType>([
  Cardano.AddressType.BasePaymentKeyStakeKey,
  Cardano.AddressType.BasePaymentKeyStakeScript,
  Cardano.AddressType.PointerKey,
  Cardano.AddressType.EnterpriseKey,
]);

export const isCip30CollateralCandidate = (utxo: Cip30Utxo): boolean => {
  try {
    const address = Cardano.Address.fromBytes(
      utxo.address as Parameters<typeof Cardano.Address.fromBytes>[0]
    );
    return !utxo.value.assets.length && paymentKeyTypes.has(address.getType());
  } catch {
    return false;
  }
};

type Selection = Readonly<{ utxos: readonly Cip30Utxo[]; total: bigint }>;

const better = (left: Selection, right?: Selection): boolean => {
  if (!right) return true;
  if (left.total !== right.total) return left.total < right.total;
  if (left.utxos.length !== right.utxos.length)
    return left.utxos.length < right.utxos.length;
  return (
    left.utxos.map(outpoint).join(',') < right.utxos.map(outpoint).join(',')
  );
};

export const selectCip30Collateral = (
  utxos: readonly Cip30Utxo[],
  amountCbor: string,
  maxCollateralInputs: number
): readonly string[] | null => {
  const amount = decodeCip30Coin(amountCbor);
  if (!Number.isSafeInteger(maxCollateralInputs) || maxCollateralInputs < 1)
    throw new RangeError('Invalid maxCollateralInputs');

  const candidates = utxos
    .filter(isCip30CollateralCandidate)
    .sort((left, right) => outpoint(left).localeCompare(outpoint(right)));
  let best: Selection | undefined;

  // ponytail: exhaustive up to the protocol input bound; use branch-and-bound only if wallet UTxO scale makes this measurable.
  const visit = (start: number, chosen: Cip30Utxo[], total: bigint): void => {
    if (chosen.length) {
      const selection = { utxos: [...chosen], total };
      if (total >= amount && better(selection, best)) best = selection;
    }
    if (
      chosen.length >= maxCollateralInputs ||
      start >= candidates.length ||
      (best !== undefined && total >= best.total)
    )
      return;
    for (let index = start; index < candidates.length; index += 1) {
      chosen.push(candidates[index]);
      visit(index + 1, chosen, total + candidates[index].value.coin);
      chosen.pop();
    }
  };
  visit(0, [], BigInt(0));

  return best ? best.utxos.map(({ context }) => context.unspentCbor) : null;
};
