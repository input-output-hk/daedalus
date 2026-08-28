import cbor from 'cbor';
import { blake2b } from 'blakejs';
import { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import {
  base58_decode,
  base58_encode,
} from '@cardano-foundation/ledgerjs-hw-app-cardano/dist/utils/address';
import type { CardanoSignTransaction } from '@trezor/connect';

import { bytesForSpan, CborItem, parseCborItem } from '../cardano/cborSlices';
import type {
  HardwareExactTransaction,
  HardwareOwnedInput,
  HardwareSigner,
} from '../types/hardware-wallets.types';

class ExactTrezorMappingError extends Error {
  public constructor(reason: string) {
    super(`Trezor exact transaction is not representable: ${reason}`);
    this.name = 'ExactTrezorMappingError';
  }
}

const exactFail = (reason: string): never => {
  throw new ExactTrezorMappingError(reason);
};
const isHex = (value: string, bytes: number): boolean =>
  new RegExp(`^[0-9a-f]{${bytes * 2}}$`, 'u').test(value);
const items = (item: CborItem): readonly CborItem[] =>
  item.major === 4 && item.items ? item.items : exactFail('array');
const uint = (item: CborItem): bigint =>
  item.major === 0 && item.value !== undefined ? item.value : exactFail('uint');
const bytes = (source: Buffer, item: CborItem): string => {
  if (item.major !== 2) return exactFail('bytes');
  if (item.content) return bytesForSpan(source, item.content).toString('hex');
  return Buffer.concat(
    items(item).map((chunk) => {
      if (!chunk.content) return exactFail('indefinite bytes');
      return bytesForSpan(source, chunk.content);
    })
  ).toString('hex');
};
const fields = (body: CborItem): ReadonlyMap<number, CborItem> => {
  if (body.major !== 5 || !body.entries) return exactFail('body map');
  const result = new Map<number, CborItem>();
  for (const { key, value } of body.entries) {
    const number = uint(key);
    if (number > BigInt(Number.MAX_SAFE_INTEGER) || result.has(Number(number)))
      exactFail('body key');
    result.set(Number(number), value);
  }
  return result;
};
const signer = (
  exact: HardwareExactTransaction,
  keyHash: string
): HardwareSigner | undefined =>
  exact.signers.find((entry) => entry.keyHash === keyHash);
const path = (value: readonly number[]): number[] => {
  if (
    !value.length ||
    value.some((part) => !Number.isSafeInteger(part) || part < 0)
  )
    return exactFail('path');
  return [...value];
};
const exactInput = (
  input: Readonly<{ transactionId: string; index: bigint }>,
  role: HardwareOwnedInput['role'],
  owned: readonly HardwareOwnedInput[]
) => {
  if (
    !isHex(input.transactionId, 32) ||
    input.index > BigInt(Number.MAX_SAFE_INTEGER)
  )
    return exactFail('input');
  const bound = owned.find(
    (entry) =>
      entry.role === role &&
      entry.transactionId === input.transactionId &&
      entry.index === input.index
  );
  return {
    prev_hash: input.transactionId,
    prev_index: Number(input.index),
    ...(bound ? { path: path(bound.path) } : {}),
  };
};
const assets = (
  value: readonly Readonly<{
    policyId: string;
    assetName: string;
    quantity: bigint;
  }>[]
) => {
  const grouped = new Map<
    string,
    Array<{ assetNameBytes: string; amount: string }>
  >();
  for (const { policyId, assetName, quantity } of value) {
    if (!isHex(policyId, 28) || !/^[0-9a-f]*$/u.test(assetName))
      exactFail('asset');
    const entries = grouped.get(policyId) || [];
    entries.push({ assetNameBytes: assetName, amount: quantity.toString() });
    grouped.set(policyId, entries);
  }
  const compare = (left: string, right: string): number => {
    const leftBytes = Buffer.from(left, 'hex');
    const rightBytes = Buffer.from(right, 'hex');
    return leftBytes.length === rightBytes.length
      ? Buffer.compare(leftBytes, rightBytes)
      : leftBytes.length - rightBytes.length;
  };
  return [...grouped.entries()]
    .sort(([left], [right]) => compare(left, right))
    .map(([policyId, tokenAmounts]) => ({
      policyId,
      tokenAmounts: tokenAmounts.sort((left, right) =>
        compare(left.assetNameBytes, right.assetNameBytes)
      ),
    }));
};
const output = (
  exact: HardwareExactTransaction,
  value: HardwareExactTransaction['transaction']['outputs'][number]
) => {
  const encoded = bytesForSpan(
    exact.transaction.envelope.cbor,
    value.exactSpan
  );
  const format = encoded[0] >> 5 === 5 ? 1 : 0;
  if (format === 0 && (value.datum?.kind === 'inline' || value.referenceScript))
    return exactFail('legacy output fields');
  const referenceScript = value.referenceScript
    ? (() => {
        const wrapper = parseCborItem(
          exact.transaction.envelope.cbor,
          value.referenceScript!.span.start
        );
        const wrapped = wrapper.items?.[0];
        if (
          wrapper.major !== 6 ||
          wrapper.tag !== BigInt(24) ||
          !wrapped?.content
        )
          return exactFail('reference script');
        return bytesForSpan(
          exact.transaction.envelope.cbor,
          wrapped.content
        ).toString('hex');
      })()
    : undefined;
  if (!isHex(value.address, Math.ceil(value.address.length / 2)))
    exactFail('address');
  const owned = exact.ownedOutputs.find(
    (candidate) => candidate.address === value.address
  );
  const rawAddress = Buffer.from(value.address, 'hex');
  const addressType = rawAddress[0] >> 4;
  let destination:
    | { address: string }
    | {
        addressParameters: {
          addressType: number;
          path: number[];
          stakingPath?: number[];
        };
      } = {
    address:
      addressType === 8
        ? base58_encode(rawAddress)
        : utils.bech32_encodeAddress(rawAddress),
  };
  if (owned?.paymentPath && addressType === 6)
    destination = {
      addressParameters: {
        addressType,
        path: path(owned.paymentPath),
      },
    };
  else if (owned?.paymentPath && owned.stakePath && addressType === 0)
    destination = {
      addressParameters: {
        addressType,
        path: path(owned.paymentPath),
        stakingPath: path(owned.stakePath),
      },
    };
  return {
    ...destination,
    amount: value.value.coin.toString(),
    ...(value.value.assets.length
      ? { tokenBundle: assets(value.value.assets) }
      : {}),
    ...(format ? { format } : {}),
    ...(value.datum?.kind === 'hash' ? { datumHash: value.datum.hash } : {}),
    ...(value.datum?.kind === 'inline' && value.datum.cbor
      ? { inlineDatum: value.datum.cbor }
      : {}),
    ...(referenceScript ? { referenceScript } : {}),
  };
};
const credential = (
  exact: HardwareExactTransaction,
  source: Buffer,
  item: CborItem
) => {
  const [kind, value, ...rest] = items(item);
  if (rest.length) return exactFail('credential length');
  const type = uint(kind);
  const hash = bytes(source, value);
  if (!isHex(hash, 28)) return exactFail('credential hash');
  if (type === BigInt(0)) {
    const found = signer(exact, hash);
    return found &&
      exact.witnesses.requestedDeviceKeyHashes.includes(found.keyHash)
      ? { path: path(found.path) }
      : { keyHash: hash };
  }
  if (type === BigInt(1)) return { scriptHash: hash };
  return exactFail('credential type');
};
const drep = (source: Buffer, item: CborItem) => {
  const [kind, value, ...rest] = items(item);
  if (rest.length) return exactFail('drep length');
  const type = uint(kind);
  if (type === BigInt(2) || type === BigInt(3)) return { type: Number(type) };
  const hash = bytes(source, value);
  if (!isHex(hash, 28) || (type !== BigInt(0) && type !== BigInt(1)))
    return exactFail('drep');
  return type === BigInt(0)
    ? { type: 0, keyHash: hash }
    : { type: 1, scriptHash: hash };
};
const isNull = (source: Buffer, item: CborItem): boolean =>
  item.major === 7 &&
  item.span.end === item.span.start + 1 &&
  source[item.span.start] === 0xf6;
const text = (source: Buffer, item: CborItem): string => {
  if (item.major !== 3 || !item.content) return exactFail('text');
  return bytesForSpan(source, item.content).toString();
};
const setItems = (item: CborItem): readonly CborItem[] =>
  items(taggedSet(item) ? item.items![0] : item);
const poolRelay = (source: Buffer, item: CborItem) => {
  const [kind, ...parts] = items(item);
  const type = Number(uint(kind));
  if (type === 0) {
    if (parts.length !== 3) return exactFail('pool relay');
    const ipv4 = isNull(source, parts[1])
      ? undefined
      : Buffer.from(bytes(source, parts[1]), 'hex');
    const ipv6 = isNull(source, parts[2])
      ? undefined
      : Buffer.from(bytes(source, parts[2]), 'hex');
    if ((!ipv4 || ipv4.length !== 4) && (!ipv6 || ipv6.length !== 16))
      return exactFail('pool relay');
    return {
      type,
      ...(isNull(source, parts[0]) ? {} : { port: Number(uint(parts[0])) }),
      ...(ipv4 && ipv4.length === 4
        ? { ipv4Address: [...ipv4].join('.') }
        : {}),
      ...(ipv6 && ipv6.length === 16
        ? {
            ipv6Address: ipv6.toString('hex').match(/.{4}/gu)!.join(':'),
          }
        : {}),
    };
  }
  if (type === 1 && parts.length === 2)
    return {
      type,
      ...(isNull(source, parts[0]) ? {} : { port: Number(uint(parts[0])) }),
      hostName: text(source, parts[1]),
    };
  if (type === 2 && parts.length === 1)
    return { type, hostName: text(source, parts[0]) };
  return exactFail('pool relay');
};
const poolRegistration = (
  exact: HardwareExactTransaction,
  source: Buffer,
  parts: readonly CborItem[]
) => {
  const parameters =
    parts.length === 1
      ? items(parts[0])
      : exactFail('pool registration length');
  const [
    operator,
    vrf,
    pledge,
    cost,
    margin,
    reward,
    owners,
    relays,
    metadata,
    ...rest
  ] = parameters;
  if (
    rest.length ||
    !operator ||
    !vrf ||
    !pledge ||
    !cost ||
    !margin ||
    !reward ||
    !owners ||
    !relays ||
    !metadata
  )
    return exactFail('pool registration length');
  const marginValue =
    margin.major === 6 &&
    margin.tag === BigInt(30) &&
    margin.items?.length === 1
      ? margin.items[0]
      : exactFail('pool margin');
  const [numerator, denominator, ...marginRest] = items(marginValue);
  if (marginRest.length) return exactFail('pool margin');
  const ownerValues = setItems(owners);
  if (!ownerValues.length) return exactFail('pool owners');
  const poolId = bytes(source, operator);
  const vrfKeyHash = bytes(source, vrf);
  const rewardAccount = bytes(source, reward);
  if (
    !isHex(poolId, 28) ||
    !isHex(vrfKeyHash, 32) ||
    !/^[0-9a-f]+$/u.test(rewardAccount)
  )
    return exactFail('pool registration');
  const mappedOwners = ownerValues.map((owner) => {
    const keyHash = bytes(source, owner);
    if (!isHex(keyHash, 28)) return exactFail('pool owner');
    const found = signer(exact, keyHash);
    return found &&
      exact.witnesses.requestedDeviceKeyHashes.includes(found.keyHash)
      ? { stakingKeyPath: path(found.path) }
      : { stakingKeyHash: keyHash };
  });
  if (mappedOwners.filter((owner) => 'stakingKeyPath' in owner).length !== 1)
    return exactFail('pool owner cardinality');
  return {
    type: 3,
    poolParameters: {
      poolId,
      vrfKeyHash,
      pledge: uint(pledge).toString(),
      cost: uint(cost).toString(),
      margin: {
        numerator: uint(numerator).toString(),
        denominator: uint(denominator).toString(),
      },
      rewardAccount,
      owners: mappedOwners,
      relays: items(relays).map((relay) => poolRelay(source, relay)),
      ...(isNull(source, metadata)
        ? {}
        : (() => {
            const [url, hash, ...metadataRest] = items(metadata);
            if (metadataRest.length || !isHex(bytes(source, hash), 32))
              return exactFail('pool metadata');
            return {
              metadata: { url: text(source, url), hash: bytes(source, hash) },
            };
          })()),
    },
  };
};

const certificates = (exact: HardwareExactTransaction) => {
  const source = exact.transaction.envelope.cbor;
  return exact.transaction.certificates.map(({ span }) => {
    const [tag, ...parts] = items(parseCborItem(source, span.start));
    const type = Number(uint(tag));
    switch (type) {
      case 0:
      case 1:
      case 7:
      case 8:
        if (parts.length !== (type >= 7 ? 2 : 1))
          return exactFail('certificate length');
        return {
          type,
          ...credential(exact, source, parts[0]),
          ...(type >= 7 ? { deposit: uint(parts[1]).toString() } : {}),
        };
      case 2:
        if (parts.length !== 2) return exactFail('delegation length');
        return {
          type,
          ...credential(exact, source, parts[0]),
          pool: bytes(source, parts[1]),
        };
      case 9:
        if (parts.length !== 2) return exactFail('vote delegation length');
        return {
          type,
          ...credential(exact, source, parts[0]),
          dRep: drep(source, parts[1]),
        };
      case 3:
        return poolRegistration(exact, source, parts);
      default:
        return exactFail(`certificate ${type}`);
    }
  });
};
const taggedSet = (item: CborItem): boolean =>
  item.major === 6 && item.tag === BigInt(258) && item.items?.length === 1;
const setTagging = (bodyFields: ReadonlyMap<number, CborItem>): boolean => {
  const setFields = [0, 4, 13, 14, 18]
    .map((key) => bodyFields.get(key))
    .filter((item): item is CborItem => item !== undefined);
  const tagged = setFields.map(taggedSet);
  if (tagged.some(Boolean) && !tagged.every(Boolean))
    return exactFail('mixed set tagging');
  return tagged.every(Boolean);
};

const poolRegistrationAsOwner = (exact: HardwareExactTransaction): boolean =>
  exact.transaction.certificates.some(({ span }) => {
    const [tag, parameters] = items(
      parseCborItem(exact.transaction.envelope.cbor, span.start)
    );
    if (uint(tag) !== BigInt(3)) return false;
    const operator = parameters ? items(parameters)[0] : undefined;
    if (!operator) return exactFail('pool registration');
    const poolOperator = signer(
      exact,
      bytes(exact.transaction.envelope.cbor, operator)
    );
    return (
      !poolOperator ||
      !exact.witnesses.requestedDeviceKeyHashes.includes(poolOperator.keyHash)
    );
  });

type TrezorOutput = CardanoSignTransaction['outputs'][number];
type TrezorCertificate = NonNullable<
  CardanoSignTransaction['certificates']
>[number];

const pathEquals = (
  left: readonly number[],
  right: readonly number[]
): boolean =>
  left.length === right.length &&
  left.every((part, index) => part === right[index]);

const keyHashForPath = (
  exact: HardwareExactTransaction,
  value: string | number[]
): string => {
  if (!Array.isArray(value)) return exactFail('string path');
  const found = exact.signers.find((entry) => pathEquals(entry.path, value));
  return found?.keyHash || exactFail('unknown path');
};

const requestCredential = (
  exact: HardwareExactTransaction,
  value: Readonly<{
    path?: string | number[];
    keyHash?: string;
    scriptHash?: string;
  }>
): unknown[] => {
  if (value.path)
    return [0, Buffer.from(keyHashForPath(exact, value.path), 'hex')];
  if (value.keyHash && isHex(value.keyHash, 28))
    return [0, Buffer.from(value.keyHash, 'hex')];
  if (value.scriptHash && isHex(value.scriptHash, 28))
    return [1, Buffer.from(value.scriptHash, 'hex')];
  return exactFail('request credential');
};

const requestAddress = (
  exact: HardwareExactTransaction,
  value: TrezorOutput
): Buffer => {
  if ('address' in value) {
    try {
      return value.address.startsWith('addr')
        ? utils.bech32_decodeAddress(value.address)
        : base58_decode(value.address);
    } catch {
      return exactFail('request address');
    }
  }
  const { path: paymentPath, stakingPath } = value.addressParameters;
  if (!Array.isArray(paymentPath)) return exactFail('request address path');
  const owned = exact.ownedOutputs.find(
    (candidate) =>
      candidate.paymentPath &&
      pathEquals(candidate.paymentPath, paymentPath) &&
      (stakingPath === undefined ||
        (Array.isArray(stakingPath) &&
          candidate.stakePath &&
          pathEquals(candidate.stakePath, stakingPath)))
  );
  return owned
    ? Buffer.from(owned.address, 'hex')
    : exactFail('request owned address');
};

const requestAssets = (
  groups: NonNullable<TrezorOutput['tokenBundle']>,
  quantity: 'amount' | 'mintAmount'
): Map<Buffer, Map<Buffer, bigint>> =>
  new Map(
    groups.map(({ policyId, tokenAmounts }) => [
      Buffer.from(policyId, 'hex'),
      new Map(
        tokenAmounts.map((token) => {
          const amount = token[quantity];
          if (amount === undefined) return exactFail('request asset amount');
          return [Buffer.from(token.assetNameBytes, 'hex'), BigInt(amount)];
        })
      ),
    ])
  );

const requestValue = (value: TrezorOutput): unknown => {
  const coin = BigInt(value.amount);
  return value.tokenBundle?.length
    ? [coin, requestAssets(value.tokenBundle, 'amount')]
    : coin;
};

const requestOutput = (
  exact: HardwareExactTransaction,
  value: TrezorOutput
): unknown => {
  const address = requestAddress(exact, value);
  const amount = requestValue(value);
  if (value.format !== 1) {
    if (value.inlineDatum || value.referenceScript)
      return exactFail('request legacy output');
    return value.datumHash
      ? [address, amount, Buffer.from(value.datumHash, 'hex')]
      : [address, amount];
  }
  const result = new Map<number, unknown>([
    [0, address],
    [1, amount],
  ]);
  if (value.datumHash) result.set(2, [0, Buffer.from(value.datumHash, 'hex')]);
  else if (value.inlineDatum)
    result.set(2, [
      1,
      cbor.decodeFirstSync(Buffer.from(value.inlineDatum, 'hex')),
    ]);
  if (value.referenceScript)
    result.set(
      3,
      new cbor.Tagged(24, Buffer.from(value.referenceScript, 'hex'))
    );
  return result;
};

const requestDrep = (
  value: NonNullable<TrezorCertificate['dRep']>
): unknown[] => {
  if (value.type === 2 || value.type === 3) return [value.type];
  if (value.type === 0 && value.keyHash)
    return [0, Buffer.from(value.keyHash, 'hex')];
  if (value.type === 1 && value.scriptHash)
    return [1, Buffer.from(value.scriptHash, 'hex')];
  return exactFail('request drep');
};

const requestRelay = (
  value: NonNullable<
    NonNullable<TrezorCertificate['poolParameters']>['relays']
  >[number]
): unknown[] => {
  if (value.type === 0) {
    const ipv4 = value.ipv4Address
      ? Buffer.from(value.ipv4Address.split('.').map(Number))
      : null;
    const ipv6 = value.ipv6Address
      ? Buffer.concat(
          value.ipv6Address.split(':').map((part) => {
            const result = Buffer.allocUnsafe(2);
            result.writeUInt16BE(Number.parseInt(part, 16));
            return result;
          })
        )
      : null;
    if ((ipv4 && ipv4.length !== 4) || (ipv6 && ipv6.length !== 16))
      return exactFail('request relay address');
    return [0, value.port ?? null, ipv4, ipv6];
  }
  if (value.type === 1 && value.hostName)
    return [1, value.port ?? null, value.hostName];
  if (value.type === 2 && value.hostName) return [2, value.hostName];
  return exactFail('request relay');
};

const requestCertificate = (
  exact: HardwareExactTransaction,
  value: TrezorCertificate,
  tagged: boolean
): unknown[] => {
  if ([0, 1, 7, 8].includes(value.type)) {
    const result: unknown[] = [value.type, requestCredential(exact, value)];
    if (value.type === 7 || value.type === 8)
      result.push(BigInt(value.deposit || exactFail('request deposit')));
    return result;
  }
  if (value.type === 2)
    return [
      value.type,
      requestCredential(exact, value),
      Buffer.from(value.pool || exactFail('request pool'), 'hex'),
    ];
  if (value.type === 9)
    return [
      value.type,
      requestCredential(exact, value),
      requestDrep(value.dRep || exactFail('request drep')),
    ];
  if (value.type !== 3 || !value.poolParameters)
    return exactFail('request certificate');
  const pool = value.poolParameters;
  const owners = pool.owners.map((owner) =>
    Buffer.from(
      owner.stakingKeyPath
        ? keyHashForPath(exact, owner.stakingKeyPath)
        : owner.stakingKeyHash || exactFail('request pool owner'),
      'hex'
    )
  );
  return [
    3,
    [
      Buffer.from(pool.poolId, 'hex'),
      Buffer.from(pool.vrfKeyHash, 'hex'),
      BigInt(pool.pledge),
      BigInt(pool.cost),
      new cbor.Tagged(30, [
        BigInt(pool.margin.numerator),
        BigInt(pool.margin.denominator),
      ]),
      Buffer.from(pool.rewardAccount, 'hex'),
      tagged ? new cbor.Tagged(258, owners) : owners,
      pool.relays.map(requestRelay),
      pool.metadata
        ? [pool.metadata.url, Buffer.from(pool.metadata.hash, 'hex')]
        : null,
    ],
  ];
};

const requestInput = (
  value: Readonly<{ prev_hash: string; prev_index: number }>
): unknown[] => [Buffer.from(value.prev_hash, 'hex'), value.prev_index];

const reconstructTrezorBody = (
  exact: HardwareExactTransaction,
  request: CardanoSignTransaction
): Buffer => {
  const tagged = request.tagCborSets === true;
  const set = (values: unknown[]): unknown =>
    tagged ? new cbor.Tagged(258, values) : values;
  const body = new Map<number, unknown>([
    [0, set(request.inputs.map(requestInput))],
    [1, request.outputs.map((value) => requestOutput(exact, value))],
    [2, BigInt(request.fee)],
  ]);
  if (request.ttl !== undefined) body.set(3, BigInt(request.ttl));
  if (request.certificates)
    body.set(
      4,
      set(
        request.certificates.map((value) =>
          requestCertificate(exact, value, tagged)
        )
      )
    );
  if (request.withdrawals) {
    const withdrawals = new Map<Buffer, bigint>();
    for (const value of request.withdrawals) {
      const keyHash = value.path
        ? keyHashForPath(exact, value.path)
        : value.keyHash || value.scriptHash || exactFail('request withdrawal');
      const addressType = value.scriptHash ? 15 : 14;
      withdrawals.set(
        Buffer.concat([
          Buffer.from([(addressType << 4) | request.networkId]),
          Buffer.from(keyHash, 'hex'),
        ]),
        BigInt(value.amount)
      );
    }
    body.set(5, withdrawals);
  }
  if (request.auxiliaryData?.hash)
    body.set(7, Buffer.from(request.auxiliaryData.hash, 'hex'));
  if (request.validityIntervalStart !== undefined)
    body.set(8, BigInt(request.validityIntervalStart));
  if (request.mint)
    body.set(
      9,
      requestAssets(
        request.mint as NonNullable<TrezorOutput['tokenBundle']>,
        'mintAmount'
      )
    );
  if (request.scriptDataHash)
    body.set(11, Buffer.from(request.scriptDataHash, 'hex'));
  if (request.collateralInputs)
    body.set(13, set(request.collateralInputs.map(requestInput)));
  if (request.requiredSigners)
    body.set(
      14,
      set(
        request.requiredSigners.map((value) =>
          Buffer.from(
            value.keyPath
              ? keyHashForPath(exact, value.keyPath)
              : value.keyHash || exactFail('request required signer'),
            'hex'
          )
        )
      )
    );
  if (request.includeNetworkId) body.set(15, request.networkId);
  if (request.collateralReturn)
    body.set(16, requestOutput(exact, request.collateralReturn));
  if (request.totalCollateral !== undefined)
    body.set(17, BigInt(request.totalCollateral));
  if (request.referenceInputs)
    body.set(18, set(request.referenceInputs.map(requestInput)));
  return cbor.Encoder.encodeOne(body, {
    canonical: true,
    collapseBigIntegers: true,
  });
};

export const assertExactTrezorBody = (
  exact: HardwareExactTransaction,
  request: CardanoSignTransaction,
  bodyFields = fields(exact.transaction.envelope.body)
): void => {
  const body = bytesForSpan(
    exact.transaction.envelope.cbor,
    exact.transaction.envelope.spans.body
  );
  if (
    !isHex(exact.bodyHash, 32) ||
    Buffer.from(blake2b(body, undefined, 32)).toString('hex') !== exact.bodyHash
  )
    exactFail('incoming body hash');
  const reconstructed = reconstructTrezorBody(exact, request);
  if (!reconstructed.equals(body)) {
    const limit = Math.min(reconstructed.length, body.length);
    let offset = 0;
    while (offset < limit && reconstructed[offset] === body[offset])
      offset += 1;
    exactFail(
      `body reconstruction at ${offset} (${reconstructed.length}/${body.length})`
    );
  }
  const known = new Set([
    0,
    1,
    2,
    3,
    4,
    5,
    7,
    8,
    9,
    11,
    13,
    14,
    15,
    16,
    17,
    18,
  ]);
  for (const key of bodyFields.keys())
    if (!known.has(key)) exactFail(`body key ${key}`);
  if (
    request.inputs.length !== exact.transaction.inputs.normal.length ||
    request.outputs.length !== exact.transaction.outputs.length ||
    request.fee !== exact.transaction.fee.toString()
  )
    exactFail('body reconstruction');
};

export const toExactTrezorSignTransactionRequest = (
  exact: HardwareExactTransaction
): CardanoSignTransaction => {
  if (
    exact.capability.vendor !== 'trezor' ||
    exact.capability.artifactId !== 'trezor-connect-9.7.2' ||
    !exact.capability.staticallyRepresentable ||
    !exact.capability.staticGatesPassed
  )
    exactFail('Trezor Connect 9.7.2 static gate');
  const bodyFields = fields(exact.transaction.envelope.body);
  for (const [family, disposition] of Object.entries(
    exact.capability.familyDispositions
  ))
    if (disposition !== 'representable') exactFail(`${family}:${disposition}`);
  for (const key of [19, 20, 21, 22])
    if (bodyFields.has(key)) exactFail(`body key ${key}`);
  if (
    exact.transaction.governance.votes.length ||
    exact.transaction.governance.proposals.length ||
    exact.transaction.governance.treasuryValue !== undefined ||
    exact.transaction.governance.donation !== undefined
  )
    exactFail('governance');
  const requested = exact.signers.filter(({ keyHash }) =>
    exact.witnesses.requestedDeviceKeyHashes.includes(keyHash)
  );
  if (
    requested.length !== exact.witnesses.requestedDeviceKeyHashes.length ||
    new Set(requested.map(({ keyHash }) => keyHash)).size !== requested.length
  )
    exactFail('missing signer path');
  const paths = requested.map(({ path: signerPath }) =>
    path(signerPath).join('/')
  );
  if (new Set(paths).size !== paths.length) exactFail('ambiguous signer path');
  const owned = exact.ownedInputs.filter(({ path: inputPath }) =>
    paths.includes(path(inputPath).join('/'))
  );
  const normalPaths = new Set(
    owned
      .filter(({ role }) => role === 'normal')
      .map(({ path: inputPath }) => path(inputPath).join('/'))
  );
  const additionalWitnessRequests = requested
    .filter(
      ({ path: signerPath }) => !normalPaths.has(path(signerPath).join('/'))
    )
    .map(({ path: signerPath }) => path(signerPath));
  const isPlutus =
    exact.transaction.witnesses.redeemers.length > 0 ||
    exact.transaction.commitments.scriptDataHash !== undefined ||
    exact.transaction.inputs.collateral.length > 0 ||
    exact.transaction.collateral.return !== undefined ||
    exact.transaction.collateral.total !== undefined ||
    exact.transaction.inputs.reference.length > 0;
  let signingMode = 0;
  if (isPlutus) signingMode = 3;
  else if (poolRegistrationAsOwner(exact)) signingMode = 1;
  const request = {
    inputs: exact.transaction.inputs.normal.map((input) =>
      exactInput(input, 'normal', owned)
    ),
    outputs: exact.transaction.outputs.map((entry) => output(exact, entry)),
    fee: exact.transaction.fee.toString(),
    protocolMagic: exact.network.networkMagic,
    networkId: exact.network.networkId,
    signingMode,
    ...(exact.transaction.validityInterval.invalidHereafter !== undefined
      ? { ttl: exact.transaction.validityInterval.invalidHereafter.toString() }
      : {}),
    ...(exact.transaction.certificates.length
      ? { certificates: certificates(exact) }
      : {}),
    ...(exact.transaction.withdrawals.length
      ? {
          withdrawals: exact.transaction.withdrawals.map(
            ({ account, coin }) => {
              if (!isHex(account, 29)) return exactFail('withdrawal account');
              const addressType = Number.parseInt(account.slice(0, 1), 16);
              const credentialHash = account.slice(2);
              if (addressType === 15)
                return { scriptHash: credentialHash, amount: coin.toString() };
              if (addressType !== 14) return exactFail('withdrawal account');
              const found = signer(exact, credentialHash);
              return found &&
                exact.witnesses.requestedDeviceKeyHashes.includes(found.keyHash)
                ? { path: path(found.path), amount: coin.toString() }
                : { keyHash: credentialHash, amount: coin.toString() };
            }
          ),
        }
      : {}),
    ...(exact.transaction.commitments.auxiliaryDataHash
      ? {
          auxiliaryData: {
            hash: exact.transaction.commitments.auxiliaryDataHash,
          },
        }
      : {}),
    ...(exact.transaction.validityInterval.invalidBefore !== undefined
      ? {
          validityIntervalStart: exact.transaction.validityInterval.invalidBefore.toString(),
        }
      : {}),
    ...(exact.transaction.mint.length
      ? {
          mint: assets(exact.transaction.mint).map(
            ({ policyId, tokenAmounts }) => ({
              policyId,
              tokenAmounts: tokenAmounts.map(({ assetNameBytes, amount }) => ({
                assetNameBytes,
                mintAmount: amount,
              })),
            })
          ),
        }
      : {}),
    ...(exact.transaction.commitments.scriptDataHash
      ? { scriptDataHash: exact.transaction.commitments.scriptDataHash }
      : {}),
    ...(exact.transaction.inputs.collateral.length
      ? {
          collateralInputs: exact.transaction.inputs.collateral.map((input) =>
            exactInput(input, 'collateral', owned)
          ),
        }
      : {}),
    ...(exact.transaction.requiredSigners.length
      ? {
          requiredSigners: exact.transaction.requiredSigners.map((keyHash) => {
            const found = signer(exact, keyHash);
            return found &&
              exact.witnesses.requestedDeviceKeyHashes.includes(keyHash)
              ? { keyPath: path(found.path) }
              : { keyHash };
          }),
        }
      : {}),
    ...(exact.transaction.networkId !== undefined
      ? { includeNetworkId: true }
      : {}),
    ...(exact.transaction.collateral.return
      ? { collateralReturn: output(exact, exact.transaction.collateral.return) }
      : {}),
    ...(exact.transaction.collateral.total !== undefined
      ? { totalCollateral: exact.transaction.collateral.total.toString() }
      : {}),
    ...(exact.transaction.inputs.reference.length
      ? {
          referenceInputs: exact.transaction.inputs.reference.map(
            ({ transactionId, index }) => ({
              prev_hash: transactionId,
              prev_index: Number(index),
            })
          ),
        }
      : {}),
    ...(additionalWitnessRequests.length ? { additionalWitnessRequests } : {}),
    tagCborSets: setTagging(bodyFields),
  } as CardanoSignTransaction;
  assertExactTrezorBody(exact, request, bodyFields);
  return request;
};
