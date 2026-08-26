import { Cardano } from '@cardano-sdk/core';
import { bech32 } from 'bech32';

import { invalidRequest } from '../cip30/errors';
import type { Paginate } from '../cip30/wire';
import { decodeConwayOutput, decodeConwayValue, Value } from './transaction';
import type {
  ContextOutput,
  TransactionContextSnapshot,
} from './transactionContext';

const MAX_UINT64 = BigInt('18446744073709551615');
const rawHex = /^(?:[0-9a-f]{2})+$/;

const invalid = (): never => {
  throw invalidRequest();
};

const encodeHead = (major: number, value: bigint): Buffer => {
  if (value < BigInt(0) || value > MAX_UINT64) return invalid();
  if (value < BigInt(24)) return Buffer.from([(major << 5) | Number(value)]);
  if (value <= BigInt(0xff))
    return Buffer.from([(major << 5) | 24, Number(value)]);
  if (value <= BigInt(0xffff)) {
    const result = Buffer.alloc(3);
    result[0] = (major << 5) | 25;
    result.writeUInt16BE(Number(value), 1);
    return result;
  }
  if (value <= BigInt(0xffffffff)) {
    const result = Buffer.alloc(5);
    result[0] = (major << 5) | 26;
    result.writeUInt32BE(Number(value), 1);
    return result;
  }
  const result = Buffer.alloc(9);
  result[0] = (major << 5) | 27;
  result.writeBigUInt64BE(value, 1);
  return result;
};

const encodeBytes = (value: string): Buffer => {
  const bytes = Buffer.from(value, 'hex');
  return Buffer.concat([encodeHead(2, BigInt(bytes.length)), bytes]);
};

const canonicalOrder = (left: Buffer, right: Buffer): number =>
  left.length - right.length || Buffer.compare(left, right);

export type Cip30Utxo = Readonly<{
  context: ContextOutput;
  value: Value;
  address: string;
}>;

export type Cip30UtxoPage =
  | Readonly<{ kind: 'page'; items: readonly string[] }>
  | Readonly<{ kind: 'paginate-error'; maxSize: number }>;

export const normalizeCip30Address = (
  input: string,
  networkId: 0 | 1
): string => {
  try {
    const encoded = rawHex.test(input) ? undefined : bech32.decode(input, 1000);
    const raw = encoded
      ? Buffer.from(bech32.fromWords(encoded.words)).toString('hex')
      : input;
    const address = Cardano.Address.fromBytes(
      raw as Parameters<typeof Cardano.Address.fromBytes>[0]
    );
    const bytes = address.toBytes();
    const reward =
      address.getType() === Cardano.AddressType.RewardKey ||
      address.getType() === Cardano.AddressType.RewardScript;
    const expectedHrp = `${reward ? 'stake' : 'addr'}${
      address.getNetworkId() === 0 ? '_test' : ''
    }`;
    if (
      bytes !== raw ||
      address.getType() === Cardano.AddressType.Byron ||
      address.getNetworkId() !== networkId ||
      (encoded !== undefined &&
        (encoded.prefix !== expectedHrp ||
          bech32.encode(encoded.prefix, encoded.words, 1000) !== input))
    )
      return invalid();
    return bytes;
  } catch {
    return invalid();
  }
};

export const decodeCip30Value = (hex: string): Value => {
  try {
    if (!rawHex.test(hex)) return invalid();
    return decodeConwayValue(Buffer.from(hex, 'hex'));
  } catch {
    return invalid();
  }
};

export const serializeCip30Value = (value: Value): string => {
  if (value.coin < BigInt(0) || value.coin > MAX_UINT64) return invalid();
  if (!value.assets.length) return encodeHead(0, value.coin).toString('hex');

  const policies = new Map<string, Map<string, bigint>>();
  value.assets.forEach(({ policyId, assetName, quantity }) => {
    if (
      !/^[0-9a-f]{56}$/.test(policyId) ||
      !/^(?:[0-9a-f]{2}){0,32}$/.test(assetName) ||
      quantity <= BigInt(0) ||
      quantity > MAX_UINT64
    )
      return invalid();
    const assets = policies.get(policyId) || new Map<string, bigint>();
    if (assets.has(assetName)) return invalid();
    assets.set(assetName, quantity);
    policies.set(policyId, assets);
  });

  const policyEntries = [...policies].map(([policyId, assets]) => {
    const assetEntries = [...assets]
      .map(([assetName, quantity]) => {
        const key = encodeBytes(assetName);
        return { key, encoded: Buffer.concat([key, encodeHead(0, quantity)]) };
      })
      .sort((left, right) => canonicalOrder(left.key, right.key));
    const key = encodeBytes(policyId);
    return {
      key,
      encoded: Buffer.concat([
        key,
        encodeHead(5, BigInt(assetEntries.length)),
        ...assetEntries.map(({ encoded }) => encoded),
      ]),
    };
  });
  policyEntries.sort((left, right) => canonicalOrder(left.key, right.key));
  return Buffer.concat([
    encodeHead(4, BigInt(2)),
    encodeHead(0, value.coin),
    encodeHead(5, BigInt(policyEntries.length)),
    ...policyEntries.map(({ encoded }) => encoded),
  ]).toString('hex');
};

const outpointOrder = (left: ContextOutput, right: ContextOutput): number =>
  left.outpoint.transactionId.localeCompare(right.outpoint.transactionId) ||
  left.outpoint.index - right.outpoint.index;

export const controlledCip30Utxos = (
  snapshot: Pick<TransactionContextSnapshot, 'outputs'>
): readonly Cip30Utxo[] =>
  snapshot.outputs
    .filter(({ walletMember }) => walletMember)
    .sort(outpointOrder)
    .map((context) => {
      try {
        const output = decodeConwayOutput(
          Buffer.from(context.canonicalCbor, 'hex')
        );
        return { context, value: output.value, address: output.address };
      } catch {
        return invalid();
      }
    });

const sumValues = (utxos: readonly Cip30Utxo[]): Value => {
  let coin = BigInt(0);
  const assets = new Map<
    string,
    { policyId: string; assetName: string; quantity: bigint }
  >();
  utxos.forEach(({ value }) => {
    coin += value.coin;
    value.assets.forEach((asset) => {
      const key = `${asset.policyId}:${asset.assetName}`;
      const previous = assets.get(key);
      assets.set(key, {
        ...asset,
        quantity: (previous?.quantity || BigInt(0)) + asset.quantity,
      });
    });
  });
  return { coin, assets: [...assets.values()] };
};

export const getCip30Balance = (
  snapshot: Pick<TransactionContextSnapshot, 'outputs'>
): string => serializeCip30Value(sumValues(controlledCip30Utxos(snapshot)));

const validatePaginate = (paginate: Paginate): void => {
  if (
    !paginate ||
    typeof paginate !== 'object' ||
    Object.keys(paginate).sort().join(',') !== 'limit,page' ||
    !Number.isSafeInteger(paginate.page) ||
    paginate.page < 0 ||
    !Number.isSafeInteger(paginate.limit) ||
    paginate.limit < 1 ||
    paginate.limit > 100
  )
    invalid();
};

export const getCip30Utxos = (
  snapshot: Pick<TransactionContextSnapshot, 'outputs'>,
  amount?: string,
  paginate?: Paginate
): Cip30UtxoPage | null => {
  const available = controlledCip30Utxos(snapshot);
  let selected = available;
  if (amount !== undefined) {
    const requested = decodeCip30Value(amount);
    const prefix: Cip30Utxo[] = [];
    const quantities = new Map<string, bigint>();
    let coin = BigInt(0);
    const isCovered = () =>
      coin >= requested.coin &&
      requested.assets.every(
        ({ policyId, assetName, quantity }) =>
          (quantities.get(`${policyId}:${assetName}`) || BigInt(0)) >= quantity
      );
    for (const utxo of available) {
      if (isCovered()) break;
      prefix.push(utxo);
      coin += utxo.value.coin;
      utxo.value.assets.forEach(({ policyId, assetName, quantity }) => {
        const key = `${policyId}:${assetName}`;
        quantities.set(key, (quantities.get(key) || BigInt(0)) + quantity);
      });
    }
    if (!isCovered()) return null;
    selected = prefix;
  }
  if (!paginate)
    return {
      kind: 'page',
      items: selected.map(({ context }) => context.unspentCbor),
    };

  validatePaginate(paginate);
  if (paginate.page > Math.floor(Number.MAX_SAFE_INTEGER / paginate.limit))
    return { kind: 'paginate-error', maxSize: selected.length };
  const start = paginate.page * paginate.limit;
  if (start > selected.length)
    return { kind: 'paginate-error', maxSize: selected.length };
  return {
    kind: 'page',
    items: selected
      .slice(start, start + paginate.limit)
      .map(({ context }) => context.unspentCbor),
  };
};
