import cbor from 'cbor';
import { blake2b } from 'blakejs';
import {
  AddressType,
  CertificateType,
  CredentialParamsType,
  DRepParamsType,
  DatumType,
  PoolKeyType,
  PoolOwnerType,
  PoolRewardAccountType,
  RelayType,
  TransactionSigningMode,
  TxAuxiliaryDataType,
  TxOutputDestinationType,
  TxOutputFormat,
  TxRequiredSignerType,
  VoteOption,
  VoterType,
} from '@cardano-foundation/ledgerjs-hw-app-cardano';
import type {
  Certificate as LedgerCertificate,
  CredentialParams,
  DRepParams as LedgerDRepParams,
  PoolKey,
  PoolOwner,
  PoolRewardAccount,
  Relay,
  SignTransactionRequest,
  TxInput,
  TxOutput,
  Voter,
} from '@cardano-foundation/ledgerjs-hw-app-cardano';

import { bytesForSpan, CborItem, parseCborItem } from '../cardano/cborSlices';
import type {
  HardwareExactTransaction,
  HardwareOwnedInput,
  HardwareSigner,
} from '../types/hardware-wallets.types';

class ExactLedgerMappingError extends Error {
  public constructor(reason: string) {
    super(`Ledger exact transaction is not representable: ${reason}`);
    this.name = 'ExactLedgerMappingError';
  }
}

const exactFail = (reason: string): never => {
  throw new ExactLedgerMappingError(reason);
};

const cborItems = (item: CborItem): readonly CborItem[] =>
  item.major === 4 && item.items ? item.items : exactFail('array');
const cborEntries = (
  item: CborItem
): readonly Readonly<{ key: CborItem; value: CborItem }>[] =>
  item.major === 5 && item.entries ? item.entries : exactFail('map');
const cborUint = (item: CborItem): bigint =>
  item.major === 0 && item.value !== undefined ? item.value : exactFail('uint');
const cborBytes = (source: Buffer, item: CborItem): string => {
  if (item.major !== 2) return exactFail('bytes');
  if (item.content) return bytesForSpan(source, item.content).toString('hex');
  return Buffer.concat(
    cborItems(item).map((chunk) => {
      if (!chunk.content) return exactFail('indefinite bytes');
      return bytesForSpan(source, chunk.content);
    })
  ).toString('hex');
};
const bodyFields = (body: CborItem): ReadonlyMap<number, CborItem> => {
  const result = new Map<number, CborItem>();
  cborEntries(body).forEach(({ key, value }) => {
    const field = cborUint(key);
    if (field > BigInt(Number.MAX_SAFE_INTEGER)) exactFail('body key');
    result.set(Number(field), value);
  });
  return result;
};
const cborNull = (source: Buffer, item: CborItem): boolean =>
  item.major === 7 &&
  item.span.end === item.span.start + 1 &&
  source[item.span.start] === 0xf6;
const ledgerPath = (path: readonly number[]): number[] => [...path];
const isHex = (hex: string, bytes: number): boolean =>
  new RegExp(`^[0-9a-f]{${bytes * 2}}$`, 'u').test(hex);

const exactSigner = (
  exact: HardwareExactTransaction,
  keyHash: string
): HardwareSigner | undefined =>
  exact.signers.find((signer) => signer.keyHash === keyHash);

const credentialParams = (
  exact: HardwareExactTransaction,
  source: Buffer,
  item: CborItem,
  allowDrep = false
): CredentialParams | LedgerDRepParams => {
  const [kind, hash, ...rest] = cborItems(item);
  if (rest.length) return exactFail('credential length');
  const type = cborUint(kind);
  if (type === BigInt(0)) {
    const keyHash = cborBytes(source, hash);
    if (!isHex(keyHash, 28)) return exactFail('credential key hash');
    const signer = exactSigner(exact, keyHash);
    return signer
      ? {
          type: CredentialParamsType.KEY_PATH,
          keyPath: ledgerPath(signer.path),
        }
      : { type: CredentialParamsType.KEY_HASH, keyHashHex: keyHash };
  }
  if (type === BigInt(1)) {
    const scriptHash = cborBytes(source, hash);
    if (!isHex(scriptHash, 28)) return exactFail('credential script hash');
    return {
      type: CredentialParamsType.SCRIPT_HASH,
      scriptHashHex: scriptHash,
    };
  }
  if (allowDrep && type === BigInt(2)) return { type: DRepParamsType.ABSTAIN };
  if (allowDrep && type === BigInt(3))
    return { type: DRepParamsType.NO_CONFIDENCE };
  return exactFail('credential kind');
};
const stakeCredentialForAccount = (
  exact: HardwareExactTransaction,
  account: string
): CredentialParams => {
  if (!isHex(account, 29)) return exactFail('withdrawal account');
  const hash = account.slice(2);
  const addressType = Number.parseInt(account.slice(0, 1), 16);
  if (addressType === 14) {
    const signer = exactSigner(exact, hash);
    return signer
      ? {
          type: CredentialParamsType.KEY_PATH,
          keyPath: ledgerPath(signer.path),
        }
      : { type: CredentialParamsType.KEY_HASH, keyHashHex: hash };
  }
  if (addressType === 15)
    return { type: CredentialParamsType.SCRIPT_HASH, scriptHashHex: hash };
  return exactFail('withdrawal address type');
};
const exactInput = (
  input: Readonly<{ transactionId: string; index: bigint }>,
  role: HardwareOwnedInput['role'],
  bindings: readonly HardwareOwnedInput[]
): TxInput => {
  if (input.index > BigInt(Number.MAX_SAFE_INTEGER)) exactFail('input index');
  const bound = bindings.find(
    (candidate) =>
      candidate.role === role &&
      candidate.transactionId === input.transactionId &&
      candidate.index === input.index
  );
  return {
    txHashHex: input.transactionId,
    outputIndex: Number(input.index),
    path: bound ? ledgerPath(bound.path) : null,
  };
};

const assetGroups = (
  assets: readonly Readonly<{
    policyId: string;
    assetName: string;
    quantity: bigint;
  }>[]
) => {
  const byPolicy = new Map<
    string,
    Array<{ assetNameHex: string; amount: string }>
  >();
  assets.forEach(({ policyId, assetName, quantity }) => {
    if (!isHex(policyId, 28) || !/^[0-9a-f]*$/u.test(assetName))
      exactFail('asset id');
    const tokens = byPolicy.get(policyId) || [];
    tokens.push({ assetNameHex: assetName, amount: quantity.toString() });
    byPolicy.set(policyId, tokens);
  });
  const canonicalHex = (left: string, right: string): number => {
    const leftBytes = Buffer.from(left, 'hex');
    const rightBytes = Buffer.from(right, 'hex');
    return leftBytes.length === rightBytes.length
      ? Buffer.compare(leftBytes, rightBytes)
      : leftBytes.length - rightBytes.length;
  };
  return [...byPolicy.entries()]
    .sort(([left], [right]) => canonicalHex(left, right))
    .map(([policyIdHex, tokens]) => ({
      policyIdHex,
      tokens: tokens.sort((left, right) =>
        canonicalHex(left.assetNameHex, right.assetNameHex)
      ),
    }));
};

const outputDestination = (
  exact: HardwareExactTransaction,
  address: string
): TxOutput['destination'] => {
  const owned = exact.ownedOutputs.find(
    (candidate) => candidate.address === address
  );
  const bytes = Buffer.from(address, 'hex');
  const type = bytes[0] >> 4;
  if (owned?.paymentPath && type === 6)
    return {
      type: TxOutputDestinationType.DEVICE_OWNED as const,
      params: {
        type: AddressType.ENTERPRISE_KEY,
        params: { spendingPath: ledgerPath(owned.paymentPath) },
      },
    };
  if (owned?.paymentPath && owned.stakePath && type === 0)
    return {
      type: TxOutputDestinationType.DEVICE_OWNED as const,
      params: {
        type: AddressType.BASE_PAYMENT_KEY_STAKE_KEY,
        params: {
          spendingPath: ledgerPath(owned.paymentPath),
          stakingPath: ledgerPath(owned.stakePath),
        },
      },
    };
  return {
    type: TxOutputDestinationType.THIRD_PARTY as const,
    params: { addressHex: address },
  };
};

const exactOutput = (
  exact: HardwareExactTransaction,
  output: HardwareExactTransaction['transaction']['outputs'][number]
): TxOutput => {
  const source = exact.transaction.envelope.cbor;
  const encoded = bytesForSpan(source, output.exactSpan);
  const format =
    encoded[0] >> 5 === 5
      ? TxOutputFormat.MAP_BABBAGE
      : TxOutputFormat.ARRAY_LEGACY;
  const destination = outputDestination(exact, output.address);
  const tokenBundle = assetGroups(output.value.assets);
  if (format === TxOutputFormat.ARRAY_LEGACY) {
    if (output.datum?.kind === 'inline' || output.referenceScript)
      return exactFail('Alonzo output fields');
    return {
      destination,
      amount: output.value.coin.toString(),
      tokenBundle,
      ...(output.datum ? { datumHashHex: output.datum.hash } : {}),
    };
  }
  const referenceScriptHex = output.referenceScript
    ? (() => {
        const wrapper = parseCborItem(
          source,
          output.referenceScript.span.start
        );
        const bytes = wrapper.items?.[0];
        if (
          wrapper.major !== 6 ||
          wrapper.tag !== BigInt(24) ||
          !bytes?.content
        )
          return exactFail('reference script wrapper');
        return bytesForSpan(source, bytes.content).toString('hex');
      })()
    : undefined;
  const base = {
    format,
    destination,
    amount: output.value.coin.toString(),
    tokenBundle,
    ...(referenceScriptHex ? { referenceScriptHex } : {}),
  };
  if (output.datum?.kind === 'hash')
    return {
      ...base,
      datum: { type: DatumType.HASH, datumHashHex: output.datum.hash },
    };
  if (output.datum?.kind === 'inline')
    return {
      ...base,
      datum: {
        type: DatumType.INLINE,
        datumHex: output.datum.cbor || exactFail('inline datum'),
      },
    };
  return base;
};

const drep = (
  exact: HardwareExactTransaction,
  source: Buffer,
  item: CborItem
): LedgerDRepParams =>
  credentialParams(exact, source, item, true) as LedgerDRepParams;

const nullableAnchor = (
  source: Buffer,
  item: CborItem
): { url: string; hashHex: string } | null => {
  if (cborNull(source, item)) return null;
  const [url, hash, ...rest] = cborItems(item);
  if (rest.length || url.major !== 3) return exactFail('anchor');
  const text = bytesForSpan(
    source,
    url.content || exactFail('anchor url')
  ).toString();
  return { url: text, hashHex: cborBytes(source, hash) };
};

const exactCertificates = (
  exact: HardwareExactTransaction
): LedgerCertificate[] => {
  const source = exact.transaction.envelope.cbor;
  return exact.transaction.certificates.map(({ span }) => {
    const cert = parseCborItem(source, span.start);
    const [tag, ...parts] = cborItems(cert);
    const type = Number(cborUint(tag));
    const credential = (item: CborItem): CredentialParams =>
      credentialParams(exact, source, item) as CredentialParams;
    const make = <T extends LedgerCertificate['type']>(
      certificateType: T,
      params: Extract<LedgerCertificate, { type: T }>['params']
    ): Extract<LedgerCertificate, { type: T }> =>
      ({ type: certificateType, params } as Extract<
        LedgerCertificate,
        { type: T }
      >);
    switch (type) {
      case 0:
        return make(CertificateType.STAKE_REGISTRATION, {
          stakeCredential: credential(parts[0]),
        });
      case 1:
        return make(CertificateType.STAKE_DEREGISTRATION, {
          stakeCredential: credential(parts[0]),
        });
      case 2:
        return make(CertificateType.STAKE_DELEGATION, {
          stakeCredential: credential(parts[0]),
          poolKeyHashHex: cborBytes(source, parts[1]),
        });
      case 3:
        return poolRegistration(exact, source, parts);
      case 4:
        return make(CertificateType.STAKE_POOL_RETIREMENT, {
          poolKeyPath: ledgerPath(
            exactSigner(exact, cborBytes(source, parts[0]))?.path ||
              exactFail('pool retirement path')
          ),
          retirementEpoch: cborUint(parts[1]).toString(),
        });
      case 7:
        return make(CertificateType.STAKE_REGISTRATION_CONWAY, {
          stakeCredential: credential(parts[0]),
          deposit: cborUint(parts[1]).toString(),
        });
      case 8:
        return make(CertificateType.STAKE_DEREGISTRATION_CONWAY, {
          stakeCredential: credential(parts[0]),
          deposit: cborUint(parts[1]).toString(),
        });
      case 9:
        return make(CertificateType.VOTE_DELEGATION, {
          stakeCredential: credential(parts[0]),
          dRep: drep(exact, source, parts[1]),
        });
      case 10:
        return make(CertificateType.STAKE_POOL_AND_DREP_DELEGATION, {
          stakeCredential: credential(parts[0]),
          poolKeyHashHex: cborBytes(source, parts[1]),
          dRep: drep(exact, source, parts[2]),
        });
      case 11:
        return make(
          CertificateType.ACCOUNT_REGISTRATION_DELEGATION_TO_STAKE_POOL,
          {
            stakeCredential: credential(parts[0]),
            poolKeyHashHex: cborBytes(source, parts[1]),
            deposit: cborUint(parts[2]).toString(),
          }
        );
      case 12:
        return make(CertificateType.ACCOUNT_REGISTRATION_DELEGATION_TO_DREP, {
          stakeCredential: credential(parts[0]),
          dRep: drep(exact, source, parts[1]),
          deposit: cborUint(parts[2]).toString(),
        });
      case 13:
        return make(
          CertificateType.ACCOUNT_REGISTRATION_DELEGATION_TO_STAKE_POOL_AND_DREP,
          {
            stakeCredential: credential(parts[0]),
            poolKeyHashHex: cborBytes(source, parts[1]),
            dRep: drep(exact, source, parts[2]),
            deposit: cborUint(parts[3]).toString(),
          }
        );
      case 14:
        return make(CertificateType.AUTHORIZE_COMMITTEE_HOT, {
          coldCredential: credential(parts[0]),
          hotCredential: credential(parts[1]),
        });
      case 15:
        return make(CertificateType.RESIGN_COMMITTEE_COLD, {
          coldCredential: credential(parts[0]),
          anchor: nullableAnchor(source, parts[1]),
        });
      case 16:
        return make(CertificateType.DREP_REGISTRATION, {
          dRepCredential: credential(parts[0]),
          deposit: cborUint(parts[1]).toString(),
          anchor: nullableAnchor(source, parts[2]),
        });
      case 17:
        return make(CertificateType.DREP_DEREGISTRATION, {
          dRepCredential: credential(parts[0]),
          deposit: cborUint(parts[1]).toString(),
        });
      case 18:
        return make(CertificateType.DREP_UPDATE, {
          dRepCredential: credential(parts[0]),
          anchor: nullableAnchor(source, parts[1]),
        });
      default:
        return exactFail(`certificate ${type}`);
    }
  });
};

const cborSet = (item: CborItem): readonly CborItem[] => {
  const value =
    item.major === 6 && item.tag === BigInt(258) && item.items?.length === 1
      ? item.items[0]
      : item;
  return cborItems(value);
};
const cborText = (source: Buffer, item: CborItem): string => {
  if (item.major !== 3 || !item.content) return exactFail('text');
  return bytesForSpan(source, item.content).toString();
};
const poolKey = (exact: HardwareExactTransaction, keyHash: string): PoolKey => {
  const signer = exactSigner(exact, keyHash);
  return signer
    ? {
        type: PoolKeyType.DEVICE_OWNED,
        params: { path: ledgerPath(signer.path) },
      }
    : {
        type: PoolKeyType.THIRD_PARTY,
        params: { keyHashHex: keyHash },
      };
};
const poolReward = (
  exact: HardwareExactTransaction,
  account: string
): PoolRewardAccount => {
  const keyHash = account.slice(2);
  const signer = exactSigner(exact, keyHash);
  return signer
    ? {
        type: PoolRewardAccountType.DEVICE_OWNED,
        params: { path: ledgerPath(signer.path) },
      }
    : {
        type: PoolRewardAccountType.THIRD_PARTY,
        params: { rewardAccountHex: account },
      };
};
const poolRelay = (source: Buffer, item: CborItem): Relay => {
  const [kind, ...parts] = cborItems(item);
  switch (Number(cborUint(kind))) {
    case RelayType.SINGLE_HOST_IP_ADDR: {
      const ipv4 = cborNull(source, parts[1])
        ? null
        : [...Buffer.from(cborBytes(source, parts[1]), 'hex')].join('.');
      const ipv6 = cborNull(source, parts[2])
        ? null
        : Buffer.from(cborBytes(source, parts[2]), 'hex')
            .toString('hex')
            .match(/.{4}/gu)
            ?.join(':') || exactFail('pool IPv6');
      if (!ipv4 && !ipv6) return exactFail('pool IP relay address');
      return {
        type: RelayType.SINGLE_HOST_IP_ADDR,
        params: {
          portNumber: cborNull(source, parts[0])
            ? null
            : Number(cborUint(parts[0])),
          ipv4,
          ipv6,
        },
      };
    }
    case RelayType.SINGLE_HOST_HOSTNAME:
      return {
        type: RelayType.SINGLE_HOST_HOSTNAME,
        params: {
          portNumber: cborNull(source, parts[0])
            ? null
            : Number(cborUint(parts[0])),
          dnsName: cborText(source, parts[1]),
        },
      };
    case RelayType.MULTI_HOST:
      return {
        type: RelayType.MULTI_HOST,
        params: { dnsName: cborText(source, parts[0]) },
      };
    default:
      return exactFail('pool relay');
  }
};
const poolRegistration = (
  exact: HardwareExactTransaction,
  source: Buffer,
  parts: readonly CborItem[]
): Extract<
  LedgerCertificate,
  { type: CertificateType.STAKE_POOL_REGISTRATION }
> => {
  const [
    operatorItem,
    vrfItem,
    pledgeItem,
    costItem,
    marginItem,
    rewardAccountItem,
    ownersItem,
    relaysItem,
    metadata,
    ...rest
  ] = parts;
  if (rest.length) return exactFail('pool registration length');
  const operator = cborBytes(source, operatorItem);
  const marginWrapper =
    marginItem.major === 6 &&
    marginItem.tag === BigInt(30) &&
    marginItem.items?.length === 1
      ? marginItem.items[0]
      : exactFail('pool margin tag');
  const margin = cborItems(marginWrapper);
  if (margin.length !== 2) return exactFail('pool margin');
  const rewardAccount = cborBytes(source, rewardAccountItem);
  const owners: PoolOwner[] = cborSet(ownersItem).map((owner) => {
    const keyHash = cborBytes(source, owner);
    const signer = exactSigner(exact, keyHash);
    return signer
      ? {
          type: PoolOwnerType.DEVICE_OWNED,
          params: { stakingPath: ledgerPath(signer.path) },
        }
      : {
          type: PoolOwnerType.THIRD_PARTY,
          params: { stakingKeyHashHex: keyHash },
        };
  });
  const metadataParams = cborNull(source, metadata)
    ? null
    : (() => {
        const [url, hash, ...extra] = cborItems(metadata);
        if (extra.length) return exactFail('pool metadata');
        return {
          metadataUrl: cborText(source, url),
          metadataHashHex: cborBytes(source, hash),
        };
      })();
  return {
    type: CertificateType.STAKE_POOL_REGISTRATION,
    params: {
      poolKey: poolKey(exact, operator),
      vrfKeyHashHex: cborBytes(source, vrfItem),
      pledge: cborUint(pledgeItem).toString(),
      cost: cborUint(costItem).toString(),
      margin: {
        numerator: cborUint(margin[0]).toString(),
        denominator: cborUint(margin[1]).toString(),
      },
      rewardAccount: poolReward(exact, rewardAccount),
      poolOwners: owners,
      relays: cborItems(relaysItem).map((relay) => poolRelay(source, relay)),
      metadata: metadataParams,
    },
  };
};

const hasPoolOperatorPath = (exact: HardwareExactTransaction): boolean => {
  const source = exact.transaction.envelope.cbor;
  return exact.transaction.certificates.some(({ value, span }) => {
    if (value.kind !== 3) return false;
    const [, operator] = cborItems(parseCborItem(source, span.start));
    const signer = exactSigner(exact, cborBytes(source, operator));
    return Boolean(
      signer &&
        exact.witnesses.requestedDeviceKeyHashes.includes(signer.keyHash)
    );
  });
};

const exactVotingProcedures = (exact: HardwareExactTransaction) => {
  const source = exact.transaction.envelope.cbor;
  const fields = bodyFields(exact.transaction.envelope.body);
  const procedures = fields.get(19);
  if (!procedures) return undefined;
  return cborEntries(procedures).map(({ key, value }) => {
    const [type, hash] = cborItems(key);
    const hashHex = cborBytes(source, hash);
    const voterType = Number(cborUint(type));
    const signer = exactSigner(exact, hashHex);
    const voter = (() => {
      switch (voterType) {
        case 0:
          return signer
            ? {
                type: VoterType.COMMITTEE_KEY_PATH,
                keyPath: ledgerPath(signer.path),
              }
            : { type: VoterType.COMMITTEE_KEY_HASH, keyHashHex: hashHex };
        case 1:
          return {
            type: VoterType.COMMITTEE_SCRIPT_HASH,
            scriptHashHex: hashHex,
          };
        case 2:
          return signer
            ? {
                type: VoterType.DREP_KEY_PATH,
                keyPath: ledgerPath(signer.path),
              }
            : { type: VoterType.DREP_KEY_HASH, keyHashHex: hashHex };
        case 3:
          return { type: VoterType.DREP_SCRIPT_HASH, scriptHashHex: hashHex };
        case 4:
          return signer
            ? {
                type: VoterType.STAKE_POOL_KEY_PATH,
                keyPath: ledgerPath(signer.path),
              }
            : { type: VoterType.STAKE_POOL_KEY_HASH, keyHashHex: hashHex };
        default:
          return exactFail('voter');
      }
    })() as Voter;
    return {
      voter,
      votes: cborEntries(value).map(({ key: action, value: procedure }) => {
        const [transactionId, index] = cborItems(action);
        const [vote, anchor] = cborItems(procedure);
        return {
          govActionId: {
            txHashHex: cborBytes(source, transactionId),
            govActionIndex: Number(cborUint(index)),
          },
          votingProcedure: {
            vote: Number(cborUint(vote)) as VoteOption,
            ...(cborNull(source, anchor)
              ? {}
              : { anchor: nullableAnchor(source, anchor) }),
          },
        };
      }),
    };
  });
};

const ledgerSetTagging = (fields: ReadonlyMap<number, CborItem>): boolean => {
  const sets = [0, 4, 13, 14, 18]
    .map((key) => fields.get(key))
    .filter((item): item is CborItem => item !== undefined);
  const tagged = sets.map(
    (item) => item.major === 6 && item.tag === BigInt(258)
  );
  if (tagged.some(Boolean) && !tagged.every(Boolean))
    exactFail('mixed set tagging');
  return tagged.every(Boolean);
};

const ledgerHead = (major: number, value: bigint): Buffer => {
  if (value < BigInt(0)) return exactFail('negative CBOR length');
  if (value < BigInt(24)) return Buffer.from([(major << 5) | Number(value)]);
  if (value <= BigInt(0xff))
    return Buffer.from([(major << 5) | 24, Number(value)]);
  if (value <= BigInt(0xffff)) {
    const result = Buffer.allocUnsafe(3);
    result[0] = (major << 5) | 25;
    result.writeUInt16BE(Number(value), 1);
    return result;
  }
  if (value <= BigInt(0xffffffff)) {
    const result = Buffer.allocUnsafe(5);
    result[0] = (major << 5) | 26;
    result.writeUInt32BE(Number(value), 1);
    return result;
  }
  if (value <= BigInt('18446744073709551615')) {
    const result = Buffer.allocUnsafe(9);
    result[0] = (major << 5) | 27;
    result.writeBigUInt64BE(value, 1);
    return result;
  }
  return exactFail('CBOR integer overflow');
};
const encodeLedgerCbor = (value: LedgerCbor): Buffer => {
  if (value === null) return Buffer.from([0xf6]);
  if (Buffer.isBuffer(value))
    return Buffer.concat([ledgerHead(2, BigInt(value.length)), value]);
  if (typeof value === 'string') {
    const bytes = Buffer.from(value);
    return Buffer.concat([ledgerHead(3, BigInt(bytes.length)), bytes]);
  }
  if (typeof value === 'number' || typeof value === 'bigint') {
    const integer = BigInt(value);
    return integer >= BigInt(0)
      ? ledgerHead(0, integer)
      : ledgerHead(1, -integer - BigInt(1));
  }
  if (Array.isArray(value)) {
    const items = value.map(encodeLedgerCbor);
    return Buffer.concat([ledgerHead(4, BigInt(items.length)), ...items]);
  }
  if (value instanceof Map) {
    const entries = [...value.entries()]
      .map(
        ([key, item]) =>
          [encodeLedgerCbor(key), encodeLedgerCbor(item)] as const
      )
      .sort(([left], [right]) =>
        left.length === right.length
          ? Buffer.compare(left, right)
          : left.length - right.length
      );
    return Buffer.concat([
      ledgerHead(5, BigInt(entries.length)),
      ...entries.reduce<Buffer[]>(
        (result, [key, item]) => result.concat(key, item),
        []
      ),
    ]);
  }
  if (value instanceof cbor.Tagged)
    return Buffer.concat([
      ledgerHead(6, BigInt(value.tag)),
      encodeLedgerCbor(value.value as LedgerCbor),
    ]);
  return exactFail('CBOR value');
};

type LedgerCbor =
  | bigint
  | number
  | string
  | Buffer
  | null
  | LedgerCbor[]
  | Map<LedgerCbor, LedgerCbor>
  | cbor.Tagged;

const ledgerHex = (value: string): Buffer => Buffer.from(value, 'hex');
const ledgerBigint = (value: string | number | bigint): bigint => BigInt(value);
const ledgerPathHash = (
  exact: HardwareExactTransaction,
  path: readonly number[]
): Buffer => {
  const key = path.join('/');
  const signer = exact.signers.find(
    (candidate) => candidate.path.join('/') === key
  );
  return signer ? ledgerHex(signer.keyHash) : exactFail('unbound Ledger path');
};
const ledgerCredential = (
  exact: HardwareExactTransaction,
  credential: CredentialParams
): LedgerCbor[] => {
  switch (credential.type) {
    case CredentialParamsType.KEY_PATH:
      return [0, ledgerPathHash(exact, credential.keyPath)];
    case CredentialParamsType.KEY_HASH:
      return [0, ledgerHex(credential.keyHashHex)];
    case CredentialParamsType.SCRIPT_HASH:
      return [1, ledgerHex(credential.scriptHashHex)];
    default:
      return exactFail('Ledger credential');
  }
};
const ledgerDrep = (
  exact: HardwareExactTransaction,
  value: LedgerDRepParams
): LedgerCbor[] => {
  switch (value.type) {
    case DRepParamsType.KEY_PATH:
      return [0, ledgerPathHash(exact, value.keyPath)];
    case DRepParamsType.KEY_HASH:
      return [0, ledgerHex(value.keyHashHex)];
    case DRepParamsType.SCRIPT_HASH:
      return [1, ledgerHex(value.scriptHashHex)];
    case DRepParamsType.ABSTAIN:
      return [2];
    case DRepParamsType.NO_CONFIDENCE:
      return [3];
    default:
      return exactFail('Ledger DRep');
  }
};
const ledgerAnchor = (
  value: { url: string; hashHex: string } | null | undefined
): LedgerCbor => (value ? [value.url, ledgerHex(value.hashHex)] : null);
const ledgerSet = (tagged: boolean, values: LedgerCbor[]): LedgerCbor =>
  tagged ? new cbor.Tagged(258, values) : values;
const ledgerAssetMap = (
  groups: NonNullable<SignTransactionRequest['tx']['mint']>
): Map<LedgerCbor, LedgerCbor> =>
  new Map(
    groups.map((group) => [
      ledgerHex(group.policyIdHex),
      new Map(
        group.tokens.map((token) => [
          ledgerHex(token.assetNameHex),
          ledgerBigint(token.amount),
        ])
      ),
    ])
  );
const ledgerValue = (
  amount: string | number | bigint,
  groups: NonNullable<TxOutput['tokenBundle']>
): LedgerCbor =>
  groups.length
    ? [ledgerBigint(amount), ledgerAssetMap(groups)]
    : ledgerBigint(amount);
const ledgerAddress = (
  exact: HardwareExactTransaction,
  destination: TxOutput['destination']
): Buffer => {
  if (destination.type === TxOutputDestinationType.THIRD_PARTY)
    return ledgerHex(destination.params.addressHex);
  const { type, params } = destination.params;
  const network = exact.network.networkId;
  if (type === AddressType.ENTERPRISE_KEY && 'spendingPath' in params)
    return Buffer.concat([
      Buffer.from([(6 << 4) | network]),
      ledgerPathHash(exact, params.spendingPath),
    ]);
  if (
    type === AddressType.BASE_PAYMENT_KEY_STAKE_KEY &&
    'spendingPath' in params &&
    'stakingPath' in params
  )
    return Buffer.concat([
      Buffer.from([network]),
      ledgerPathHash(exact, params.spendingPath),
      ledgerPathHash(exact, params.stakingPath),
    ]);
  return exactFail('Ledger owned output address');
};
const ledgerOutput = (
  exact: HardwareExactTransaction,
  output: TxOutput
): LedgerCbor => {
  const address = ledgerAddress(exact, output.destination);
  const value = ledgerValue(output.amount, output.tokenBundle || []);
  if (output.format !== TxOutputFormat.MAP_BABBAGE) {
    const result: LedgerCbor[] = [address, value];
    if (output.datumHashHex) result.push(ledgerHex(output.datumHashHex));
    return result;
  }
  const result = new Map<LedgerCbor, LedgerCbor>([
    [0, address],
    [1, value],
  ]);
  if (output.datum) {
    result.set(
      2,
      output.datum.type === DatumType.HASH
        ? [0, ledgerHex(output.datum.datumHashHex)]
        : [1, new cbor.Tagged(24, ledgerHex(output.datum.datumHex))]
    );
  }
  if (output.referenceScriptHex)
    result.set(3, new cbor.Tagged(24, ledgerHex(output.referenceScriptHex)));
  return result;
};
const ledgerRewardAccount = (
  exact: HardwareExactTransaction,
  credential: CredentialParams
): Buffer => {
  const encoded = ledgerCredential(exact, credential);
  const kind = encoded[0] === 0 ? 14 : 15;
  return Buffer.concat([
    Buffer.from([(kind << 4) | exact.network.networkId]),
    encoded[1] as Buffer,
  ]);
};
const ledgerPoolKeyHash = (
  exact: HardwareExactTransaction,
  value: PoolKey
): Buffer =>
  value.type === PoolKeyType.DEVICE_OWNED
    ? ledgerPathHash(exact, value.params.path)
    : ledgerHex(value.params.keyHashHex);
const ledgerPoolOwnerHash = (
  exact: HardwareExactTransaction,
  value: PoolOwner
): Buffer =>
  value.type === PoolOwnerType.DEVICE_OWNED
    ? ledgerPathHash(exact, value.params.stakingPath)
    : ledgerHex(value.params.stakingKeyHashHex);
const ledgerPoolReward = (
  exact: HardwareExactTransaction,
  value: PoolRewardAccount
): Buffer =>
  value.type === PoolRewardAccountType.DEVICE_OWNED
    ? Buffer.concat([
        Buffer.from([(14 << 4) | exact.network.networkId]),
        ledgerPathHash(exact, value.params.path),
      ])
    : ledgerHex(value.params.rewardAccountHex);
const ledgerIp = (
  value: string | null | undefined,
  bytes: 4 | 16
): LedgerCbor => {
  if (value === null || value === undefined) return null;
  if (bytes === 4) return Buffer.from(value.split('.').map(Number));
  const result: number[] = [];
  value.split(':').forEach((word) => {
    const number = Number.parseInt(word || '0', 16);
    result.push(number >> 8, number & 0xff);
  });
  return Buffer.from(result);
};
const ledgerRelay = (value: Relay): LedgerCbor[] => {
  switch (value.type) {
    case RelayType.SINGLE_HOST_IP_ADDR:
      return [
        0,
        value.params.portNumber ?? null,
        ledgerIp(value.params.ipv4, 4),
        ledgerIp(value.params.ipv6, 16),
      ];
    case RelayType.SINGLE_HOST_HOSTNAME:
      return [1, value.params.portNumber ?? null, value.params.dnsName];
    case RelayType.MULTI_HOST:
      return [2, value.params.dnsName];
    default:
      return exactFail('Ledger relay');
  }
};
type LedgerCertificateParams = Readonly<{
  stakeCredential: CredentialParams;
  deposit: string | number | bigint;
  poolKeyHashHex: string;
  dRep: LedgerDRepParams;
  coldCredential: CredentialParams;
  hotCredential: CredentialParams;
  dRepCredential: CredentialParams;
  anchor?: { url: string; hashHex: string } | null;
  poolKeyPath: number[];
  retirementEpoch: string | number | bigint;
  poolKey: PoolKey;
  vrfKeyHashHex: string;
  pledge: string | number | bigint;
  cost: string | number | bigint;
  margin: {
    numerator: string | number | bigint;
    denominator: string | number | bigint;
  };
  rewardAccount: PoolRewardAccount;
  poolOwners: PoolOwner[];
  relays: Relay[];
  metadata?: { metadataUrl: string; metadataHashHex: string } | null;
}>;

const ledgerCertificate = (
  exact: HardwareExactTransaction,
  certificate: LedgerCertificate,
  taggedSets: boolean
): LedgerCbor[] => {
  const params = certificate.params as LedgerCertificateParams;
  switch (certificate.type) {
    case CertificateType.STAKE_REGISTRATION:
    case CertificateType.STAKE_DEREGISTRATION:
      return [
        certificate.type,
        ledgerCredential(exact, params.stakeCredential),
      ];
    case CertificateType.STAKE_REGISTRATION_CONWAY:
    case CertificateType.STAKE_DEREGISTRATION_CONWAY:
      return [
        certificate.type,
        ledgerCredential(exact, params.stakeCredential),
        ledgerBigint(params.deposit),
      ];
    case CertificateType.STAKE_DELEGATION:
      return [
        certificate.type,
        ledgerCredential(exact, params.stakeCredential),
        ledgerHex(params.poolKeyHashHex),
      ];
    case CertificateType.VOTE_DELEGATION:
      return [
        certificate.type,
        ledgerCredential(exact, params.stakeCredential),
        ledgerDrep(exact, params.dRep),
      ];
    case CertificateType.STAKE_POOL_AND_DREP_DELEGATION:
      return [
        certificate.type,
        ledgerCredential(exact, params.stakeCredential),
        ledgerHex(params.poolKeyHashHex),
        ledgerDrep(exact, params.dRep),
      ];
    case CertificateType.ACCOUNT_REGISTRATION_DELEGATION_TO_STAKE_POOL:
      return [
        certificate.type,
        ledgerCredential(exact, params.stakeCredential),
        ledgerHex(params.poolKeyHashHex),
        ledgerBigint(params.deposit),
      ];
    case CertificateType.ACCOUNT_REGISTRATION_DELEGATION_TO_DREP:
      return [
        certificate.type,
        ledgerCredential(exact, params.stakeCredential),
        ledgerDrep(exact, params.dRep),
        ledgerBigint(params.deposit),
      ];
    case CertificateType.ACCOUNT_REGISTRATION_DELEGATION_TO_STAKE_POOL_AND_DREP:
      return [
        certificate.type,
        ledgerCredential(exact, params.stakeCredential),
        ledgerHex(params.poolKeyHashHex),
        ledgerDrep(exact, params.dRep),
        ledgerBigint(params.deposit),
      ];
    case CertificateType.AUTHORIZE_COMMITTEE_HOT:
      return [
        certificate.type,
        ledgerCredential(exact, params.coldCredential),
        ledgerCredential(exact, params.hotCredential),
      ];
    case CertificateType.RESIGN_COMMITTEE_COLD:
      return [
        certificate.type,
        ledgerCredential(exact, params.coldCredential),
        ledgerAnchor(params.anchor),
      ];
    case CertificateType.DREP_REGISTRATION:
      return [
        certificate.type,
        ledgerCredential(exact, params.dRepCredential),
        ledgerBigint(params.deposit),
        ledgerAnchor(params.anchor),
      ];
    case CertificateType.DREP_DEREGISTRATION:
      return [
        certificate.type,
        ledgerCredential(exact, params.dRepCredential),
        ledgerBigint(params.deposit),
      ];
    case CertificateType.DREP_UPDATE:
      return [
        certificate.type,
        ledgerCredential(exact, params.dRepCredential),
        ledgerAnchor(params.anchor),
      ];
    case CertificateType.STAKE_POOL_RETIREMENT:
      return [
        certificate.type,
        ledgerPathHash(exact, params.poolKeyPath),
        ledgerBigint(params.retirementEpoch),
      ];
    case CertificateType.STAKE_POOL_REGISTRATION:
      return [
        certificate.type,
        ledgerPoolKeyHash(exact, params.poolKey),
        ledgerHex(params.vrfKeyHashHex),
        ledgerBigint(params.pledge),
        ledgerBigint(params.cost),
        new cbor.Tagged(30, [
          ledgerBigint(params.margin.numerator),
          ledgerBigint(params.margin.denominator),
        ]),
        ledgerPoolReward(exact, params.rewardAccount),
        ledgerSet(
          taggedSets,
          params.poolOwners.map((owner) => ledgerPoolOwnerHash(exact, owner))
        ),
        params.relays.map(ledgerRelay),
        params.metadata
          ? [
              params.metadata.metadataUrl,
              ledgerHex(params.metadata.metadataHashHex),
            ]
          : null,
      ];
    default:
      return exactFail('Ledger certificate');
  }
};
const ledgerVoter = (
  exact: HardwareExactTransaction,
  voter: Voter
): LedgerCbor[] => {
  switch (voter.type) {
    case VoterType.COMMITTEE_KEY_HASH:
    case VoterType.DREP_KEY_HASH:
    case VoterType.STAKE_POOL_KEY_HASH:
      return [voter.type, ledgerHex(voter.keyHashHex)];
    case VoterType.COMMITTEE_KEY_PATH:
      return [0, ledgerPathHash(exact, voter.keyPath)];
    case VoterType.DREP_KEY_PATH:
      return [2, ledgerPathHash(exact, voter.keyPath)];
    case VoterType.STAKE_POOL_KEY_PATH:
      return [4, ledgerPathHash(exact, voter.keyPath)];
    case VoterType.COMMITTEE_SCRIPT_HASH:
    case VoterType.DREP_SCRIPT_HASH:
      return [voter.type, ledgerHex(voter.scriptHashHex)];
    default:
      return exactFail('Ledger voter');
  }
};
const reconstructLedgerBody = (
  exact: HardwareExactTransaction,
  request: SignTransactionRequest
): Buffer => {
  const { tx } = request;
  const tagged = request.options?.tagCborSets === true;
  const input = (value: TxInput): LedgerCbor[] => [
    ledgerHex(value.txHashHex),
    value.outputIndex,
  ];
  const body = new Map<LedgerCbor, LedgerCbor>([
    [0, ledgerSet(tagged, tx.inputs.map(input))],
    [1, tx.outputs.map((output) => ledgerOutput(exact, output))],
    [2, ledgerBigint(tx.fee)],
  ]);
  if (tx.ttl !== undefined && tx.ttl !== null)
    body.set(3, ledgerBigint(tx.ttl));
  if (tx.certificates?.length)
    body.set(
      4,
      ledgerSet(
        tagged,
        tx.certificates.map((certificate) =>
          ledgerCertificate(exact, certificate, tagged)
        )
      )
    );
  if (tx.withdrawals?.length)
    body.set(
      5,
      new Map(
        tx.withdrawals.map((withdrawal) => [
          ledgerRewardAccount(exact, withdrawal.stakeCredential),
          ledgerBigint(withdrawal.amount),
        ])
      )
    );
  if (tx.auxiliaryData?.type === TxAuxiliaryDataType.ARBITRARY_HASH)
    body.set(7, ledgerHex(tx.auxiliaryData.params.hashHex));
  if (
    tx.validityIntervalStart !== undefined &&
    tx.validityIntervalStart !== null
  )
    body.set(8, ledgerBigint(tx.validityIntervalStart));
  if (tx.mint?.length) body.set(9, ledgerAssetMap(tx.mint));
  if (tx.scriptDataHashHex) body.set(11, ledgerHex(tx.scriptDataHashHex));
  if (tx.collateralInputs?.length)
    body.set(13, ledgerSet(tagged, tx.collateralInputs.map(input)));
  if (tx.requiredSigners?.length)
    body.set(
      14,
      ledgerSet(
        tagged,
        tx.requiredSigners.map((signer) =>
          signer.type === TxRequiredSignerType.PATH
            ? ledgerPathHash(exact, signer.path)
            : ledgerHex(signer.hashHex)
        )
      )
    );
  if (tx.includeNetworkId) body.set(15, tx.network.networkId);
  if (tx.collateralOutput)
    body.set(16, ledgerOutput(exact, tx.collateralOutput));
  if (tx.totalCollateral !== undefined && tx.totalCollateral !== null)
    body.set(17, ledgerBigint(tx.totalCollateral));
  if (tx.referenceInputs?.length)
    body.set(18, ledgerSet(tagged, tx.referenceInputs.map(input)));
  if (tx.votingProcedures?.length)
    body.set(
      19,
      new Map(
        tx.votingProcedures.map(({ voter, votes }) => [
          ledgerVoter(exact, voter),
          new Map(
            votes.map(({ govActionId, votingProcedure }) => [
              [ledgerHex(govActionId.txHashHex), govActionId.govActionIndex],
              [votingProcedure.vote, ledgerAnchor(votingProcedure.anchor)],
            ])
          ),
        ])
      )
    );
  if (tx.treasury !== undefined && tx.treasury !== null)
    body.set(21, ledgerBigint(tx.treasury));
  if (tx.donation !== undefined && tx.donation !== null)
    body.set(22, ledgerBigint(tx.donation));
  return encodeLedgerCbor(body);
};

const assertExactLedgerBody = (
  exact: HardwareExactTransaction,
  request: SignTransactionRequest,
  fields: ReadonlyMap<number, CborItem>
): void => {
  const source = exact.transaction.envelope.cbor;
  const body = bytesForSpan(source, exact.transaction.envelope.spans.body);
  const bodyHash = Buffer.from(blake2b(body, undefined, 32)).toString('hex');
  if (bodyHash !== exact.bodyHash || !isHex(exact.bodyHash, 32))
    exactFail('incoming body hash');
  if (!reconstructLedgerBody(exact, request).equals(body))
    exactFail('body reconstruction');
  const knownKeys = [
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
    19,
    21,
    22,
  ];
  for (const key of fields.keys())
    if (!knownKeys.includes(key)) exactFail(`body key ${key}`);
  const represented = [
    [3, request.tx.ttl],
    [4, request.tx.certificates],
    [5, request.tx.withdrawals],
    [7, request.tx.auxiliaryData],
    [8, request.tx.validityIntervalStart],
    [9, request.tx.mint],
    [11, request.tx.scriptDataHashHex],
    [13, request.tx.collateralInputs],
    [14, request.tx.requiredSigners],
    [15, request.tx.includeNetworkId],
    [16, request.tx.collateralOutput],
    [17, request.tx.totalCollateral],
    [18, request.tx.referenceInputs],
    [19, request.tx.votingProcedures],
    [21, request.tx.treasury],
    [22, request.tx.donation],
  ] as const;
  if (
    request.tx.inputs.length !== exact.transaction.inputs.normal.length ||
    request.tx.outputs.length !== exact.transaction.outputs.length ||
    request.tx.fee !== exact.transaction.fee.toString() ||
    represented.some(
      ([key, value]) => fields.has(key) !== (value !== undefined)
    )
  )
    exactFail('body reconstruction');
};

export const toExactLedgerSignTransactionRequest = (
  exact: HardwareExactTransaction
): SignTransactionRequest => {
  if (
    exact.capability.vendor !== 'ledger' ||
    exact.capability.artifactId !== 'ledger-8.0.0-candidate' ||
    !exact.capability.staticallyRepresentable ||
    !exact.capability.staticGatesPassed
  )
    exactFail('Ledger 8 static gate');
  const fields = bodyFields(exact.transaction.envelope.body);
  if (fields.has(20) || exact.transaction.governance.proposals.length)
    exactFail('proposal procedure');
  for (const [family, disposition] of Object.entries(
    exact.capability.familyDispositions
  ))
    if (disposition !== 'representable') exactFail(`${family}:${disposition}`);
  const requestedSigners = exact.signers.filter(({ keyHash }) =>
    exact.witnesses.requestedDeviceKeyHashes.includes(keyHash)
  );
  const signerPaths = requestedSigners.map(({ path }) => path.join('/'));
  if (new Set(signerPaths).size !== signerPaths.length)
    exactFail('ambiguous Ledger signer path');
  const ownedInputs = exact.ownedInputs.filter(({ path }) =>
    requestedSigners.some((signer) => signer.path.join('/') === path.join('/'))
  );
  const normalPaths = ownedInputs.map(({ path }) => path.join('/'));
  const additionalWitnessPaths = requestedSigners
    .filter(({ path }) => !normalPaths.includes(path.join('/')))
    .map(({ path }) => ledgerPath(path))
    .filter(
      (path, index, paths) =>
        paths.findIndex(
          (candidate) => candidate.join('/') === path.join('/')
        ) === index
    );
  const certificates = exactCertificates(exact);
  const votingProcedures = exactVotingProcedures(exact);
  let signingMode = TransactionSigningMode.ORDINARY_TRANSACTION;
  if (
    exact.transaction.governance.votes.length ||
    exact.transaction.governance.treasuryValue !== undefined ||
    exact.transaction.governance.donation !== undefined
  )
    signingMode = TransactionSigningMode.UNRESTRICTED_TRANSACTION;
  else if (
    exact.transaction.witnesses.redeemers.length ||
    exact.transaction.commitments.scriptDataHash !== undefined ||
    exact.transaction.inputs.collateral.length > 0 ||
    exact.transaction.collateral.return !== undefined ||
    exact.transaction.collateral.total !== undefined ||
    exact.transaction.inputs.reference.length > 0
  )
    signingMode = TransactionSigningMode.PLUTUS_TRANSACTION;
  else if (
    certificates.some(
      ({ type }) => type === CertificateType.STAKE_POOL_REGISTRATION
    )
  )
    signingMode = hasPoolOperatorPath(exact)
      ? TransactionSigningMode.POOL_REGISTRATION_AS_OPERATOR
      : TransactionSigningMode.POOL_REGISTRATION_AS_OWNER;
  const request: SignTransactionRequest = {
    tx: {
      network: {
        networkId: exact.network.networkId,
        protocolMagic: exact.network.networkMagic,
      },
      inputs: exact.transaction.inputs.normal.map((input) =>
        exactInput(input, 'normal', ownedInputs)
      ),
      outputs: exact.transaction.outputs.map((output) =>
        exactOutput(exact, output)
      ),
      fee: exact.transaction.fee.toString(),
      ...(exact.transaction.validityInterval.invalidHereafter !== undefined
        ? {
            ttl: exact.transaction.validityInterval.invalidHereafter.toString(),
          }
        : {}),
      ...(certificates.length ? { certificates } : {}),
      ...(exact.transaction.withdrawals.length
        ? {
            withdrawals: exact.transaction.withdrawals.map(
              ({ account, coin }) => ({
                stakeCredential: stakeCredentialForAccount(exact, account),
                amount: coin.toString(),
              })
            ),
          }
        : {}),
      ...(exact.transaction.commitments.auxiliaryDataHash
        ? {
            auxiliaryData: {
              type: TxAuxiliaryDataType.ARBITRARY_HASH,
              params: {
                hashHex: exact.transaction.commitments.auxiliaryDataHash,
              },
            },
          }
        : {}),
      ...(exact.transaction.validityInterval.invalidBefore !== undefined
        ? {
            validityIntervalStart: exact.transaction.validityInterval.invalidBefore.toString(),
          }
        : {}),
      ...(exact.transaction.mint.length
        ? { mint: assetGroups(exact.transaction.mint) }
        : {}),
      ...(exact.transaction.commitments.scriptDataHash
        ? { scriptDataHashHex: exact.transaction.commitments.scriptDataHash }
        : {}),
      ...(exact.transaction.inputs.collateral.length
        ? {
            collateralInputs: exact.transaction.inputs.collateral.map((input) =>
              exactInput(input, 'collateral', ownedInputs)
            ),
          }
        : {}),
      ...(exact.transaction.requiredSigners.length
        ? {
            requiredSigners: exact.transaction.requiredSigners.map((hash) => {
              const signer = exactSigner(exact, hash);
              return signer &&
                exact.witnesses.requestedDeviceKeyHashes.includes(hash)
                ? {
                    type: TxRequiredSignerType.PATH,
                    path: ledgerPath(signer.path),
                  }
                : { type: TxRequiredSignerType.HASH, hashHex: hash };
            }),
          }
        : {}),
      ...(exact.transaction.networkId !== undefined
        ? { includeNetworkId: true }
        : {}),
      ...(exact.transaction.collateral.return
        ? {
            collateralOutput: exactOutput(
              exact,
              exact.transaction.collateral.return
            ),
          }
        : {}),
      ...(exact.transaction.collateral.total !== undefined
        ? { totalCollateral: exact.transaction.collateral.total.toString() }
        : {}),
      ...(exact.transaction.inputs.reference.length
        ? {
            referenceInputs: exact.transaction.inputs.reference.map((input) =>
              exactInput(input, 'normal', [])
            ),
          }
        : {}),
      ...(votingProcedures ? { votingProcedures } : {}),
      ...(exact.transaction.governance.treasuryValue !== undefined
        ? { treasury: exact.transaction.governance.treasuryValue.toString() }
        : {}),
      ...(exact.transaction.governance.donation !== undefined
        ? { donation: exact.transaction.governance.donation.toString() }
        : {}),
    },
    signingMode,
    ...(additionalWitnessPaths.length ? { additionalWitnessPaths } : {}),
    options: {
      tagCborSets: ledgerSetTagging(fields),
    },
  };
  assertExactLedgerBody(exact, request, fields);
  return request;
};
