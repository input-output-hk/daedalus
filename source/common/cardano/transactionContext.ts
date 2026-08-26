import { createHmac, createPublicKey, timingSafeEqual, verify } from 'crypto';
import { blake2b } from 'blakejs';

import { bytesForSpan, parseCborItem } from './cborSlices';
import {
  CommitmentContext,
  decodeConwayOutput,
  decodeConwayTransaction,
  Hex,
  ResolvedDatum,
  Script,
} from './transaction';
import { parseConwayTransactionEnvelope } from './transactionEnvelope';

export class TransactionContextError extends Error {
  public constructor(message = 'invalid transaction context') {
    super(message);
    this.name = 'TransactionContextError';
  }
}

type Json = Record<string, unknown>;
export type DappNetwork = Readonly<{
  networkId: 0 | 1;
  networkMagic: number;
  genesisHash: Hex;
}>;
export type ContextExpectation = Readonly<{
  walletId: string;
  network: DappNetwork;
  transactions: readonly Hex[];
}>;
export type ContextChainPoint =
  | Readonly<{ kind: 'genesis' }>
  | Readonly<{ kind: 'block'; slot: bigint; blockHash: Hex }>;
export type ContextOwnership = Readonly<{
  credentialKind: 'payment' | 'stake' | 'drep' | 'policy';
  credential: Hex;
  ownership: 'unowned' | 'owned_key' | 'script';
  derivationPath: readonly number[];
  proofKinds: readonly ProofKind[];
}>;
export type ProofKind =
  | 'normal_input'
  | 'collateral'
  | 'withdrawal'
  | 'certificate'
  | 'required_signer'
  | 'native_script'
  | 'policy';
export type PreExistingWitness = Readonly<{
  transactionIndex: number;
  kind: 'vkey' | 'bootstrap';
  cbor: Hex;
}>;
export type ContextOutput = Readonly<{
  outpoint: Readonly<{ transactionId: Hex; index: number }>;
  sourceCbor: Hex;
  inputCbor: Hex;
  canonicalCbor: Hex;
  unspentCbor: Hex;
  provenance: readonly ('earlier' | 'pending' | 'node')[];
  roles: readonly ('normal' | 'collateral' | 'reference' | 'wallet_snapshot')[];
  walletMember: boolean;
  pendingState: 'none' | 'outcome_unknown';
}>;
export type TransactionContextSnapshot = Readonly<{
  walletId: string;
  network: DappNetwork;
  chainPoint: ContextChainPoint;
  walletGeneration: bigint;
  pendingGeneration: bigint;
  contextDigest: Hex;
  contextToken: Hex;
  records: readonly Hex[];
  transactions: readonly Hex[];
  outputs: readonly ContextOutput[];
  ownership: readonly ContextOwnership[];
  commitmentContexts: readonly CommitmentContext[];
  transactionsSemantic: readonly ReturnType<typeof decodeConwayTransaction>[];
  preExistingWitnesses: readonly PreExistingWitness[];
}>;
type ContextResponse = Readonly<{
  walletId: string;
  network: DappNetwork;
  chainPoint: ContextChainPoint;
  walletGeneration: bigint;
  pendingGeneration: bigint;
  protocolVersion: Readonly<{ major: number; minor: number }>;
  protocolParametersCbor: Hex;
  outputs: readonly ContextOutput[];
  ownership: readonly ContextOwnership[];
  requiredProofs: readonly Json[];
  pendingTransactions: readonly Json[];
  records: readonly Hex[];
  contextDigest: Hex;
  contextToken: Hex;
}>;

type DecodedRecord = Readonly<{ tag: number; encoded: Buffer; body: Buffer }>;

const fail = (message?: string): never => {
  throw new TransactionContextError(message);
};
const object = (value: unknown, name: string): Json =>
  value !== null && typeof value === 'object' && !Array.isArray(value)
    ? (value as Json)
    : fail(`invalid ${name}`);
const exactKeys = (
  value: Json,
  keys: readonly string[],
  name: string
): void => {
  const actual = Object.keys(value).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  )
    fail(`invalid ${name} fields`);
};
const array = (value: unknown, name: string): unknown[] =>
  Array.isArray(value) ? value : fail(`invalid ${name}`);
const boolean = (value: unknown, name: string): boolean =>
  typeof value === 'boolean' ? value : fail(`invalid ${name}`);
const uint32 = (value: unknown, name: string): number =>
  Number.isInteger(value) && Number(value) >= 0 && Number(value) <= 0xffffffff
    ? Number(value)
    : fail(`invalid ${name}`);
const word64 = (value: unknown, name: string): bigint => {
  if (typeof value !== 'string' || !/^(0|[1-9][0-9]*)$/.test(value))
    fail(`invalid ${name}`);
  const result = BigInt(value as string);
  if (result > BigInt('18446744073709551615')) fail(`invalid ${name}`);
  return result;
};
const enumValue = <T extends string>(
  value: unknown,
  values: readonly T[],
  name: string
): T =>
  typeof value === 'string' && values.includes(value as T)
    ? (value as T)
    : fail(`invalid ${name}`);
const hex = (value: unknown, name: string, length?: number): Hex => {
  if (
    typeof value !== 'string' ||
    !/^(?:[0-9a-f]{2})+$/.test(value) ||
    (length !== undefined && value.length !== length * 2)
  )
    fail(`invalid ${name}`);
  return value as string;
};
const unique = <T>(values: readonly T[], name: string): void => {
  if (new Set(values).size !== values.length) fail(`duplicate ${name}`);
};
const parseCbor = (value: Hex, name: string): Buffer => {
  const bytes = Buffer.from(value, 'hex');
  const item = parseCborItem(bytes);
  if (item.span.end !== bytes.length) fail(`invalid ${name}`);
  return bytes;
};

const u8 = (value: number): Buffer => Buffer.from([value]);
const u32 = (value: number): Buffer => {
  const result = Buffer.alloc(4);
  result.writeUInt32BE(value);
  return result;
};
const u64 = (value: bigint): Buffer => {
  const result = Buffer.alloc(8);
  result.writeBigUInt64BE(value);
  return result;
};
const sized = (value: Buffer): Buffer =>
  Buffer.concat([u32(value.length), value]);
const vector = (values: readonly Buffer[]): Buffer =>
  Buffer.concat([u32(values.length), ...values.map(sized)]);
const outpointBytes = (value: { transactionId: Hex; index: number }): Buffer =>
  Buffer.concat([Buffer.from(value.transactionId, 'hex'), u32(value.index)]);
const encodedRecord = (tag: number, body: Buffer): Buffer =>
  Buffer.concat([u8(tag), sized(body)]);
const digestBytes = (value: Buffer): Buffer =>
  Buffer.from(blake2b(value, undefined, 32));

class Reader {
  private offset = 0;
  private readonly content: Buffer;
  public constructor(bytes: Buffer) {
    this.content = bytes;
  }
  public remaining(): number {
    return this.content.length - this.offset;
  }
  public take(length: number): Buffer {
    if (
      !Number.isSafeInteger(length) ||
      length < 0 ||
      length > this.remaining()
    )
      fail('malformed context record length');
    const result = this.content.subarray(this.offset, this.offset + length);
    this.offset += length;
    return result;
  }
  public u8(): number {
    return this.take(1)[0];
  }
  public u32(): number {
    return this.take(4).readUInt32BE(0);
  }
  public u64(): bigint {
    return this.take(8).readBigUInt64BE(0);
  }
  public bytesValue(): Buffer {
    return this.take(this.u32());
  }
  public boolean(): boolean {
    const value = this.u8();
    if (value > 1) fail('invalid context boolean');
    return value === 1;
  }
  public done(): void {
    if (this.remaining()) fail('trailing context record bytes');
  }
}

const parseOutpoint = (value: unknown, name: string) => {
  const candidate = object(value, name);
  exactKeys(candidate, ['transaction_id', 'index'], name);
  return {
    transactionId: hex(candidate.transaction_id, `${name} transaction id`, 32),
    index: uint32(candidate.index, `${name} index`),
  };
};
const assertInputCbor = (
  value: Buffer,
  expected: { transactionId: Hex; index: number }
): void => {
  const item = parseCborItem(value);
  if (
    item.span.end !== value.length ||
    item.major !== 4 ||
    item.items?.length !== 2 ||
    item.items[0].content === undefined ||
    bytesForSpan(value, item.items[0].content).toString('hex') !==
      expected.transactionId ||
    item.items[1].value !== BigInt(expected.index)
  )
    fail('transaction input CBOR mismatch');
};
const outputIdentity = (value: ReturnType<typeof decodeConwayOutput>): string =>
  JSON.stringify({
    address: value.address,
    coin: value.value.coin.toString(),
    assets: value.value.assets.map((asset) => ({
      ...asset,
      quantity: asset.quantity.toString(),
    })),
    datum: value.datum && {
      kind: value.datum.kind,
      hash: value.datum.hash,
      cbor: value.datum.cbor,
    },
    script: value.referenceScript && {
      language: value.referenceScript.language,
      hash: value.referenceScript.hash,
      bytes: value.referenceScript.bytes,
    },
  });
const parseNetwork = (value: unknown): DappNetwork => {
  const network = object(value, 'network');
  exactKeys(
    network,
    ['network_id', 'network_magic', 'genesis_hash'],
    'network'
  );
  const networkId = uint32(network.network_id, 'network id');
  if (networkId !== 0 && networkId !== 1) fail('invalid network id');
  return {
    networkId: networkId as 0 | 1,
    networkMagic: uint32(network.network_magic, 'network magic'),
    genesisHash: hex(network.genesis_hash, 'genesis hash', 32),
  };
};
const parsePoint = (value: unknown, name: string): ContextChainPoint => {
  const point = object(value, name);
  if (point.kind === 'genesis') {
    exactKeys(point, ['kind'], name);
    return { kind: 'genesis' };
  }
  exactKeys(point, ['kind', 'slot', 'block_hash'], name);
  if (point.kind !== 'block') fail(`invalid ${name}`);
  return {
    kind: 'block',
    slot: word64(point.slot, `${name} slot`),
    blockHash: hex(point.block_hash, `${name} block hash`, 32),
  };
};
const parseProofKinds = (value: unknown): ProofKind[] => {
  const result = array(value, 'proof kinds').map((item) =>
    enumValue(
      item,
      [
        'normal_input',
        'collateral',
        'withdrawal',
        'certificate',
        'required_signer',
        'native_script',
        'policy',
      ] as const,
      'proof kind'
    )
  );
  unique(result, 'proof kind');
  return result;
};
const parseOwnership = (value: unknown): ContextOwnership => {
  const row = object(value, 'ownership');
  exactKeys(
    row,
    [
      'credential_kind',
      'credential',
      'ownership',
      'derivation_path',
      'proof_kinds',
    ],
    'ownership'
  );
  const credentialKind = enumValue(
    row.credential_kind,
    ['payment', 'stake', 'drep', 'policy'] as const,
    'credential kind'
  );
  const ownership = enumValue(
    row.ownership,
    ['unowned', 'owned_key', 'script'] as const,
    'ownership kind'
  );
  const derivationPath = array(
    row.derivation_path,
    'derivation path'
  ).map((item) => uint32(item, 'derivation path'));
  if (ownership !== 'owned_key' && derivationPath.length)
    fail('untrusted derivation path');
  if (ownership === 'owned_key') {
    const valid =
      (credentialKind === 'payment' &&
        derivationPath.length === 5 &&
        (derivationPath[3] === 0 || derivationPath[3] === 1) &&
        derivationPath[4] < 0x80000000) ||
      ((credentialKind === 'stake' || credentialKind === 'drep') &&
        derivationPath.length === 5 &&
        derivationPath[3] === (credentialKind === 'stake' ? 2 : 3) &&
        derivationPath[4] === 0) ||
      (credentialKind === 'policy' &&
        derivationPath.length === 3 &&
        derivationPath[0] === 0x8000073f &&
        derivationPath[1] === 0x80000717 &&
        derivationPath[2] === 0x80000000);
    if (
      !valid ||
      (credentialKind !== 'policy' &&
        (derivationPath[0] !== 0x8000073c ||
          derivationPath[1] !== 0x80000717 ||
          derivationPath[2] < 0x80000000))
    )
      fail('invalid owned derivation path');
  }
  return {
    credentialKind,
    credential: hex(row.credential, 'credential', 28),
    ownership,
    derivationPath,
    proofKinds: parseProofKinds(row.proof_kinds),
  };
};

const parseOutput = (value: unknown): ContextOutput => {
  const output = object(value, 'context output');
  exactKeys(
    output,
    [
      'outpoint',
      'transaction_input_cbor',
      'source_transaction_output_cbor',
      'canonical_transaction_output_cbor',
      'transaction_unspent_output_cbor',
      'provenance',
      'roles',
      'wallet_member',
      'pending_state',
    ],
    'context output'
  );
  const provenance = array(output.provenance, 'provenance').map((item) =>
    enumValue(item, ['earlier', 'pending', 'node'] as const, 'provenance')
  );
  const roles = array(output.roles, 'roles').map((item) =>
    enumValue(
      item,
      ['normal', 'collateral', 'reference', 'wallet_snapshot'] as const,
      'role'
    )
  );
  if (!provenance.length || !roles.length) fail('empty output authority');
  unique(provenance, 'provenance');
  unique(roles, 'role');
  const outpoint = parseOutpoint(output.outpoint, 'output outpoint');
  const inputCbor = hex(
    output.transaction_input_cbor,
    'transaction input CBOR'
  );
  const sourceCbor = hex(
    output.source_transaction_output_cbor,
    'source transaction output CBOR'
  );
  const canonicalCbor = hex(
    output.canonical_transaction_output_cbor,
    'canonical transaction output CBOR'
  );
  const unspentCbor = hex(
    output.transaction_unspent_output_cbor,
    'transaction unspent output CBOR'
  );
  const inputBytes = parseCbor(inputCbor, 'transaction input CBOR');
  assertInputCbor(inputBytes, outpoint);
  const sourceBytes = parseCbor(sourceCbor, 'source transaction output CBOR');
  const canonicalBytes = parseCbor(
    canonicalCbor,
    'canonical transaction output CBOR'
  );
  if (
    outputIdentity(decodeConwayOutput(sourceBytes)) !==
    outputIdentity(decodeConwayOutput(canonicalBytes))
  )
    fail('source and canonical transaction output mismatch');
  const unspentBytes = parseCbor(
    unspentCbor,
    'transaction unspent output CBOR'
  );
  const pair = parseCborItem(unspentBytes);
  if (
    pair.major !== 4 ||
    pair.items?.length !== 2 ||
    !bytesForSpan(unspentBytes, pair.items[0].span).equals(inputBytes) ||
    !bytesForSpan(unspentBytes, pair.items[1].span).equals(canonicalBytes)
  )
    fail('transaction unspent output CBOR mismatch');
  return {
    outpoint,
    sourceCbor,
    inputCbor,
    canonicalCbor,
    unspentCbor,
    provenance,
    roles,
    walletMember: boolean(output.wallet_member, 'wallet member'),
    pendingState: enumValue(
      output.pending_state,
      ['none', 'outcome_unknown'] as const,
      'pending state'
    ),
  };
};

const parseResponse = (value: unknown): ContextResponse => {
  const response = object(value, 'context response');
  exactKeys(
    response,
    [
      'revision',
      'wallet_id',
      'network',
      'chain_point',
      'wallet_generation',
      'pending_generation',
      'era',
      'protocol_version',
      'protocol_parameters_cbor',
      'volatile_delta',
      'outputs',
      'pending_overlay',
      'ownership',
      'required_wallet_proofs',
      'batch_overlay',
      'records',
      'context_digest',
      'context_token',
    ],
    'context response'
  );
  if (response.revision !== 1 || response.era !== 'conway')
    fail('unsupported context revision');
  const protocol = object(response.protocol_version, 'protocol version');
  exactKeys(protocol, ['major', 'minor'], 'protocol version');
  const volatile = object(response.volatile_delta, 'volatile delta');
  exactKeys(volatile, ['point', 'node_transaction_inputs'], 'volatile delta');
  parsePoint(volatile.point, 'volatile point');
  array(
    volatile.node_transaction_inputs,
    'node transaction inputs'
  ).forEach((item) =>
    parseCbor(hex(item, 'node transaction input'), 'node transaction input')
  );
  const pending = object(response.pending_overlay, 'pending overlay');
  exactKeys(
    pending,
    ['transactions', 'spent_wallet_inputs', 'produced_wallet_outputs'],
    'pending overlay'
  );
  const pendingTransactions = array(
    pending.transactions,
    'pending transactions'
  ).map((item) => {
    const transaction = object(item, 'pending transaction');
    exactKeys(
      transaction,
      [
        'transaction_id',
        'state',
        'transaction_cbor',
        'normal_inputs',
        'collateral_inputs',
        'expiry_slot',
      ],
      'pending transaction'
    );
    hex(transaction.transaction_id, 'pending transaction id', 32);
    if (transaction.state !== 'outcome_unknown') fail('invalid pending state');
    parseCbor(
      hex(transaction.transaction_cbor, 'pending transaction CBOR'),
      'pending transaction CBOR'
    );
    array(
      transaction.normal_inputs,
      'pending normal inputs'
    ).forEach((candidate) => parseOutpoint(candidate, 'pending normal input'));
    array(
      transaction.collateral_inputs,
      'pending collateral inputs'
    ).forEach((candidate) =>
      parseOutpoint(candidate, 'pending collateral input')
    );
    if (transaction.expiry_slot !== null)
      word64(transaction.expiry_slot, 'pending expiry slot');
    return transaction;
  });
  array(
    pending.spent_wallet_inputs,
    'spent wallet inputs'
  ).forEach((candidate) => parseOutpoint(candidate, 'spent wallet input'));
  if (array(pending.produced_wallet_outputs, 'produced wallet outputs').length)
    fail('unexpected pending produced output');
  const ownership = array(response.ownership, 'ownership').map(parseOwnership);
  const ownershipIds = ownership.map(
    (item) => `${item.credentialKind}:${item.credential}`
  );
  unique(ownershipIds, 'ownership record');
  const requiredProofs = array(
    response.required_wallet_proofs,
    'required wallet proofs'
  ).map((item) => {
    const proof = object(item, 'required wallet proof');
    exactKeys(
      proof,
      [
        'transaction_index',
        'proof_kind',
        'credential_kind',
        'credential',
        'required',
      ],
      'required wallet proof'
    );
    uint32(proof.transaction_index, 'proof transaction index');
    enumValue(
      proof.proof_kind,
      parseProofKinds([proof.proof_kind]),
      'proof kind'
    );
    enumValue(
      proof.credential_kind,
      ['payment', 'stake', 'drep', 'policy'] as const,
      'proof credential kind'
    );
    hex(proof.credential, 'proof credential', 28);
    boolean(proof.required, 'proof required');
    return proof;
  });
  const batch = object(response.batch_overlay, 'batch overlay');
  exactKeys(batch, ['dependencies', 'conflicts'], 'batch overlay');
  array(batch.dependencies, 'dependencies').forEach((item) =>
    object(item, 'dependency')
  );
  array(batch.conflicts, 'conflicts').forEach((item) =>
    object(item, 'conflict')
  );
  const records = array(response.records, 'records').map((item) =>
    hex(item, 'context record')
  );
  unique(records, 'context record');
  if ([...records].sort().some((item, index) => item !== records[index]))
    fail('unsorted context records');
  return {
    walletId: hex(response.wallet_id, 'wallet id', 20),
    network: parseNetwork(response.network),
    chainPoint: parsePoint(response.chain_point, 'chain point'),
    walletGeneration: word64(response.wallet_generation, 'wallet generation'),
    pendingGeneration: word64(
      response.pending_generation,
      'pending generation'
    ),
    protocolVersion: {
      major: uint32(protocol.major, 'protocol major'),
      minor: uint32(protocol.minor, 'protocol minor'),
    },
    protocolParametersCbor: hex(
      response.protocol_parameters_cbor,
      'protocol parameters CBOR'
    ),
    outputs: array(response.outputs, 'outputs').map(parseOutput),
    ownership,
    requiredProofs,
    pendingTransactions,
    records,
    contextDigest: hex(response.context_digest, 'context digest', 32),
    contextToken: hex(response.context_token, 'context token'),
  };
};

const decodeRecord = (encoded: Hex): DecodedRecord => {
  const bytes = Buffer.from(encoded, 'hex');
  const reader = new Reader(bytes);
  const tag = reader.u8();
  if (tag < 1 || tag > 7) fail('unknown context record tag');
  const body = reader.bytesValue();
  reader.done();
  const bodyReader = new Reader(body);
  const credential = () => {
    const kind = bodyReader.u8();
    if (kind < 1 || kind > 4) fail('unknown credential kind');
    const value = bodyReader.bytesValue();
    if (value.length !== 28) fail('invalid credential length');
  };
  const outpoint = () => {
    bodyReader.take(32);
    bodyReader.u32();
  };
  if (tag === 1) {
    outpoint();
    if (bodyReader.u8() & ~0x07) fail('unknown provenance bits');
    const roles = bodyReader.u8();
    if (!roles || roles & ~0x0f) fail('unknown role bits');
    bodyReader.boolean();
    if (bodyReader.u8() > 7) fail('unknown pending state');
    parseCbor(
      bodyReader.bytesValue().toString('hex'),
      'record transaction output'
    );
  } else if (tag === 2) {
    credential();
    if (bodyReader.u8() > 2) fail('unknown ownership kind');
    const pathLength = bodyReader.u32();
    for (let index = 0; index < pathLength; index += 1) bodyReader.u32();
    if (bodyReader.u32() & ~0x1ff) fail('unknown proof bits');
  } else if (tag === 3) {
    if (bodyReader.bytesValue().toString('utf8') !== 'conway')
      fail('unknown context era');
    if (bodyReader.u8() > 1) fail('unknown network id');
    bodyReader.u32();
    bodyReader.u32();
    bodyReader.u32();
    parseCbor(
      bodyReader.bytesValue().toString('hex'),
      'record protocol parameters'
    );
  } else if (tag === 4) {
    credential();
    if (bodyReader.u8() > 4) fail('unknown registration state');
  } else if (tag === 5) {
    credential();
    const role = bodyReader.u8();
    const state = bodyReader.u8();
    if (role < 1 || role > 3 || state > 4) fail('unknown governance value');
    bodyReader.u64();
    if (bodyReader.boolean()) credential();
  } else if (tag === 6) {
    bodyReader.u32();
    const proof = bodyReader.u8();
    if (proof < 1 || proof > 9) fail('unknown proof kind');
    credential();
    bodyReader.boolean();
  } else {
    bodyReader.take(32);
    if (bodyReader.u8() > 7) fail('unknown pending state');
    parseCbor(
      bodyReader.bytesValue().toString('hex'),
      'record pending transaction'
    );
    const normal = bodyReader.u32();
    for (let index = 0; index < normal; index += 1) {
      const item = bodyReader.bytesValue();
      if (item.length !== 36) fail('invalid normal outpoint');
    }
    const collateral = bodyReader.u32();
    for (let index = 0; index < collateral; index += 1) {
      const item = bodyReader.bytesValue();
      if (item.length !== 36) fail('invalid collateral outpoint');
    }
    if (bodyReader.boolean()) bodyReader.u64();
  }
  bodyReader.done();
  return { tag, encoded: bytes, body };
};

export const validateContextRecords = (records: readonly Hex[]): void => {
  unique(records, 'context record');
  records.forEach(decodeRecord);
};

const pointBytes = (point: ContextChainPoint): Buffer =>
  point.kind === 'genesis'
    ? u8(0)
    : Buffer.concat([
        u8(1),
        u64(point.slot),
        sized(Buffer.from(point.blockHash, 'hex')),
      ]);

export const computeContextDigest = (
  expectation: ContextExpectation,
  point: ContextChainPoint,
  walletGeneration: bigint,
  pendingGeneration: bigint,
  records: readonly Hex[]
): Hex => {
  const encoded = records
    .map((item) => Buffer.from(item, 'hex'))
    .sort(Buffer.compare);
  unique(
    encoded.map((item) => item.toString('hex')),
    'context record'
  );
  return digestBytes(
    Buffer.concat([
      Buffer.from('daedalus-dapp-context-v1'),
      sized(Buffer.from(expectation.walletId)),
      sized(Buffer.from(expectation.network.genesisHash, 'hex')),
      pointBytes(point),
      u64(walletGeneration),
      u64(pendingGeneration),
      vector(expectation.transactions.map((item) => Buffer.from(item, 'hex'))),
      vector(encoded),
    ])
  ).toString('hex');
};

const parseToken = (
  token: Hex,
  expectation: ContextExpectation,
  digest: Hex
): Readonly<{ payload: Buffer; mac: Buffer; processGeneration: Hex }> => {
  const bytes = Buffer.from(token, 'hex');
  if (bytes.length < 1 + 16 + 4 + 4 + 32 + 32 + 32)
    fail('invalid context token length');
  const reader = new Reader(bytes);
  if (reader.u8() !== 1) fail('unknown context token version');
  const processGeneration = reader.take(16);
  if (reader.u32() !== 1) fail('unknown context capability revision');
  const walletId = reader.bytesValue();
  if (!walletId.equals(Buffer.from(expectation.walletId)))
    fail('context token wallet mismatch');
  if (
    !reader.take(32).equals(Buffer.from(expectation.network.genesisHash, 'hex'))
  )
    fail('context token network mismatch');
  if (!reader.take(32).equals(Buffer.from(digest, 'hex')))
    fail('context token digest mismatch');
  if (reader.remaining() !== 32) fail('invalid context token MAC length');
  const mac = reader.take(32);
  return {
    payload: bytes.subarray(0, bytes.length - 32),
    mac,
    processGeneration: processGeneration.toString('hex'),
  };
};

export const verifyContextTokenMac = (
  token: Hex,
  expectation: ContextExpectation,
  digest: Hex,
  key: Buffer
): boolean => {
  if (key.length !== 32) return false;
  const parsed = parseToken(token, expectation, digest);
  const expected = createHmac('sha256', key)
    .update(
      Buffer.concat([
        Buffer.from('daedalus-dapp-context-token-v1'),
        sized(parsed.payload),
      ])
    )
    .digest();
  return timingSafeEqual(parsed.mac, expected);
};

const scriptCredential = (address: Hex): Hex | undefined => {
  const bytes = Buffer.from(address, 'hex');
  if (bytes.length < 29) fail('invalid resolved address');
  const type = bytes[0] >> 4;
  return [1, 3, 5, 7].includes(type)
    ? bytes.subarray(1, 29).toString('hex')
    : undefined;
};

const witnessParts = (
  encoded: Hex
): Readonly<{ key: Buffer; signature: Buffer }> => {
  const bytes = Buffer.from(encoded, 'hex');
  const item = parseCborItem(bytes);
  if (item.span.end !== bytes.length || item.major !== 4 || !item.items)
    fail('invalid existing witness');
  const parts = item.items;
  if (parts.length !== 2 && parts.length !== 4)
    fail('invalid existing witness');
  const payload = (index: number, length: number) => {
    const part = parts[index];
    if (!part.content) fail('invalid existing witness');
    const value = bytesForSpan(bytes, part.content);
    if (value.length !== length) fail('invalid existing witness');
    return value;
  };
  return { key: payload(0, 32), signature: payload(1, 64) };
};

const verifiedWitnesses = (
  transactionIndex: number,
  transactionId: Hex,
  vkeys: readonly Hex[],
  bootstrap: readonly Hex[]
): Readonly<{
  set: ReadonlySet<Hex>;
  metadata: readonly PreExistingWitness[];
}> => {
  const set = new Set<Hex>();
  const metadata: PreExistingWitness[] = [];
  const check = (kind: 'vkey' | 'bootstrap', cbor: Hex) => {
    const witness = witnessParts(cbor);
    const publicKey = createPublicKey({
      key: Buffer.concat([
        Buffer.from('302a300506032b6570032100', 'hex'),
        witness.key,
      ]),
      format: 'der',
      type: 'spki',
    });
    if (
      !verify(
        null,
        Buffer.from(transactionId, 'hex'),
        publicKey,
        witness.signature
      )
    )
      fail('invalid existing witness signature');
    set.add(cbor);
    metadata.push({ transactionIndex, kind, cbor });
  };
  vkeys.forEach((item) => check('vkey', item));
  bootstrap.forEach((item) => check('bootstrap', item));
  return { set, metadata };
};

const credentialKinds = { payment: 1, stake: 2, drep: 3, policy: 4 } as const;
const ownershipKinds = { unowned: 0, owned_key: 1, script: 2 } as const;
const proofBits: Readonly<Record<ProofKind, number>> = {
  normal_input: 0x01,
  collateral: 0x02,
  withdrawal: 0x04,
  certificate: 0x08,
  required_signer: 0x10,
  native_script: 0x20,
  policy: 0x40,
};
const proofKinds = {
  normal_input: 1,
  collateral: 2,
  withdrawal: 3,
  certificate: 4,
  required_signer: 5,
  native_script: 6,
  policy: 7,
} as const;
const expectedOwnershipRecord = (item: ContextOwnership): Hex =>
  encodedRecord(
    2,
    Buffer.concat([
      u8(credentialKinds[item.credentialKind]),
      sized(Buffer.from(item.credential, 'hex')),
      u8(ownershipKinds[item.ownership]),
      u32(item.derivationPath.length),
      ...item.derivationPath.map(u32),
      u32(item.proofKinds.reduce((bits, proof) => bits | proofBits[proof], 0)),
    ])
  ).toString('hex');
const expectedRequiredProofRecord = (item: Json): Hex =>
  encodedRecord(
    6,
    Buffer.concat([
      u32(item.transaction_index as number),
      u8(proofKinds[item.proof_kind as ProofKind]),
      u8(credentialKinds[item.credential_kind as keyof typeof credentialKinds]),
      sized(Buffer.from(item.credential as string, 'hex')),
      u8(Number(item.required)),
    ])
  ).toString('hex');
const expectedPendingRecord = (item: Json): Hex => {
  const normal = (item.normal_inputs as Json[]).map((candidate) =>
    outpointBytes({
      transactionId: candidate.transaction_id as string,
      index: candidate.index as number,
    })
  );
  const collateral = (item.collateral_inputs as Json[]).map((candidate) =>
    outpointBytes({
      transactionId: candidate.transaction_id as string,
      index: candidate.index as number,
    })
  );
  const expiry = item.expiry_slot;
  return encodedRecord(
    7,
    Buffer.concat([
      Buffer.from(item.transaction_id as string, 'hex'),
      u8(4),
      sized(Buffer.from(item.transaction_cbor as string, 'hex')),
      vector(normal),
      vector(collateral),
      u8(Number(expiry !== null)),
      ...(expiry === null ? [] : [u64(BigInt(expiry as string))]),
    ])
  ).toString('hex');
};

export const reconcileTransactionContext = (
  value: unknown,
  expectation: ContextExpectation
): TransactionContextSnapshot => {
  if (!/^[0-9a-f]{40}$/.test(expectation.walletId))
    fail('invalid expected wallet id');
  if (!expectation.transactions.length || expectation.transactions.length > 50)
    fail('invalid transaction count');
  expectation.transactions.forEach((item) => {
    const bytes = parseCbor(hex(item, 'transaction'), 'transaction');
    if (bytes.length > 65_536) fail('transaction too large');
  });
  const response = parseResponse(value);
  if (response.walletId !== expectation.walletId) fail('route wallet mismatch');
  if (
    response.network.networkId !== expectation.network.networkId ||
    response.network.networkMagic !== expectation.network.networkMagic ||
    response.network.genesisHash !== expectation.network.genesisHash
  )
    fail('network identity mismatch');
  const decodedRecords = response.records.map(decodeRecord);
  const digest = computeContextDigest(
    expectation,
    response.chainPoint,
    response.walletGeneration,
    response.pendingGeneration,
    response.records
  );
  if (digest !== response.contextDigest) fail('context digest mismatch');
  parseToken(response.contextToken, expectation, digest);

  const expectedRecords = response.outputs.map((output) => {
    const provenanceBits = output.provenance.reduce(
      (bits, item) =>
        bits | ({ earlier: 1, pending: 2, node: 4 } as const)[item],
      0
    );
    const roleBits = output.roles.reduce(
      (bits, item) =>
        bits |
        ({
          normal: 1,
          collateral: 2,
          reference: 4,
          wallet_snapshot: 8,
        } as const)[item],
      0
    );
    return encodedRecord(
      1,
      Buffer.concat([
        outpointBytes(output.outpoint),
        u8(provenanceBits),
        u8(roleBits),
        u8(Number(output.walletMember)),
        u8(output.pendingState === 'none' ? 0 : 4),
        sized(Buffer.from(output.sourceCbor, 'hex')),
      ])
    ).toString('hex');
  });
  expectedRecords.push(
    ...response.ownership.map(expectedOwnershipRecord),
    encodedRecord(
      3,
      Buffer.concat([
        sized(Buffer.from('conway')),
        u8(response.network.networkId),
        u32(response.network.networkMagic),
        u32(response.protocolVersion.major),
        u32(response.protocolVersion.minor),
        sized(Buffer.from(response.protocolParametersCbor, 'hex')),
      ])
    ).toString('hex'),
    ...response.requiredProofs.map(expectedRequiredProofRecord),
    ...response.pendingTransactions.map(expectedPendingRecord)
  );
  const actualRecords = decodedRecords
    .filter(({ tag }) => tag !== 4 && tag !== 5)
    .map(({ encoded }) => encoded.toString('hex'));
  if (actualRecords.sort().join(',') !== expectedRecords.sort().join(','))
    fail('context record mismatch');

  const parsed = expectation.transactions.map((item) =>
    decodeConwayTransaction(
      parseConwayTransactionEnvelope(Buffer.from(item, 'hex'))
    )
  );
  response.outputs.forEach((output) => {
    if (
      !output.provenance.includes('earlier') &&
      !output.provenance.includes('pending')
    )
      return;
    const authoritative: Buffer[] = [];
    if (output.provenance.includes('earlier')) {
      const parent = parsed.find(
        (transaction) =>
          transaction.transactionId === output.outpoint.transactionId
      );
      const produced = parent?.outputs[output.outpoint.index];
      if (!parent || !produced) fail('unresolved earlier output authority');
      authoritative.push(
        bytesForSpan(parent.envelope.cbor, produced.exactSpan)
      );
    }
    if (output.provenance.includes('pending')) {
      const pending = response.pendingTransactions.find(
        (transaction) =>
          transaction.transaction_id === output.outpoint.transactionId
      );
      if (!pending) fail('unresolved pending output authority');
      const envelope = parseConwayTransactionEnvelope(
        Buffer.from(pending.transaction_cbor as string, 'hex')
      );
      if (envelope.transactionId !== output.outpoint.transactionId)
        fail('pending transaction identity mismatch');
      const produced = envelope.outputs[output.outpoint.index];
      if (!produced) fail('unresolved pending output authority');
      authoritative.push(bytesForSpan(envelope.cbor, produced.span));
    }
    if (
      authoritative.some(
        (candidate) => candidate.toString('hex') !== output.sourceCbor
      )
    )
      fail('authoritative output bytes conflict');
  });
  const requiredInputs = new Map<
    string,
    Set<'normal' | 'collateral' | 'reference'>
  >();
  parsed.forEach((transaction) =>
    (['normal', 'collateral', 'reference'] as const).forEach((role) =>
      transaction.inputs[role].forEach((outpoint) => {
        const key = `${outpoint.transactionId}:${outpoint.index}`;
        const roles =
          requiredInputs.get(key) ||
          new Set<'normal' | 'collateral' | 'reference'>();
        roles.add(role);
        requiredInputs.set(key, roles);
      })
    )
  );
  const outputById = new Map<string, ContextOutput>();
  response.outputs.forEach((output) => {
    const id = `${output.outpoint.transactionId}:${output.outpoint.index}`;
    if (outputById.has(id)) fail('duplicate resolved output');
    outputById.set(id, output);
  });
  requiredInputs.forEach((roles, id) => {
    const output = outputById.get(id);
    if (!output || [...roles].some((role) => !output.roles.includes(role)))
      fail('missing exact resolved input context');
  });

  const preExistingWitnesses: PreExistingWitness[] = [];
  const commitmentContexts = parsed.map((transaction, transactionIndex) => {
    const verified = verifiedWitnesses(
      transactionIndex,
      transaction.transactionId,
      transaction.witnesses.vkeys,
      transaction.witnesses.bootstrap
    );
    preExistingWitnesses.push(...verified.metadata);
    const resolvedInputs = [
      ...transaction.inputs.normal,
      ...transaction.inputs.collateral,
      ...transaction.inputs.reference,
    ].map((outpoint): NonNullable<
      CommitmentContext['resolvedInputs']
    >[number] => {
      const output = outputById.get(
        `${outpoint.transactionId}:${outpoint.index}`
      );
      if (!output) fail('missing exact resolved input context');
      const bytes = Buffer.from(output.sourceCbor, 'hex');
      const decoded = decodeConwayOutput(bytes);
      let datum: ResolvedDatum | undefined;
      if (decoded.datum?.kind === 'inline') {
        if (!decoded.datum.cbor) fail('missing inline datum bytes');
        datum = { kind: 'inline', cbor: decoded.datum.cbor };
      } else if (decoded.datum) {
        datum = { kind: 'hash', hash: decoded.datum.hash };
      }
      return {
        outpoint,
        value: decoded.value,
        datum,
        scriptHash: scriptCredential(decoded.address),
        referenceScript: decoded.referenceScript,
      };
    });
    const referenceScripts = resolvedInputs
      .map((item) => item.referenceScript)
      .filter((item): item is Script => item !== undefined);
    const usedPlutusLanguages = [
      ...transaction.witnesses.plutusScripts,
      ...referenceScripts,
    ]
      .map((script) => script.language)
      .filter((language): language is 'plutus:v1' | 'plutus:v2' | 'plutus:v3' =>
        language.startsWith('plutus:')
      )
      .map((language) => (Number(language.slice(-1)) - 1) as 0 | 1 | 2)
      .filter((item, index, values) => values.indexOf(item) === index);
    return {
      resolvedInputs,
      usedPlutusLanguages,
      verifiedWitnesses: verified.set,
    };
  });
  const transactionsSemantic = expectation.transactions.map((item, index) =>
    decodeConwayTransaction(
      parseConwayTransactionEnvelope(Buffer.from(item, 'hex')),
      commitmentContexts[index]
    )
  );
  if (transactionsSemantic.some(({ review }) => !review.complete))
    fail('incomplete authenticated transaction review');

  return Object.freeze({
    walletId: response.walletId,
    network: Object.freeze(response.network),
    chainPoint: Object.freeze(response.chainPoint),
    walletGeneration: response.walletGeneration,
    pendingGeneration: response.pendingGeneration,
    contextDigest: response.contextDigest,
    contextToken: response.contextToken,
    records: Object.freeze([...response.records]),
    transactions: Object.freeze([...expectation.transactions]),
    outputs: Object.freeze([...response.outputs]),
    ownership: Object.freeze([...response.ownership]),
    commitmentContexts: Object.freeze(commitmentContexts),
    transactionsSemantic: Object.freeze(transactionsSemantic),
    preExistingWitnesses: Object.freeze(preExistingWitnesses),
  });
};
