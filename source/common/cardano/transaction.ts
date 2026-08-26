import { blake2b } from 'blakejs';

import { bytesForSpan, CborItem, CborSpan, parseCborItem } from './cborSlices';
import { ExactTransactionEnvelope } from './transactionEnvelope';

export class TransactionSemanticError extends Error {
  public constructor(message = 'invalid Conway transaction semantics') {
    super(message);
    this.name = 'TransactionSemanticError';
  }
}

export type Hex = string;
export type SemanticValue =
  | Readonly<{ kind: 'integer'; value: bigint }>
  | Readonly<{ kind: 'bytes'; hex: Hex }>
  | Readonly<{ kind: 'text'; bytes: Hex }>
  | Readonly<{ kind: 'array'; items: readonly SemanticValue[] }>
  | Readonly<{
      kind: 'map';
      entries: readonly Readonly<{
        key: SemanticValue;
        value: SemanticValue;
      }>[];
    }>
  | Readonly<{ kind: 'tag'; tag: bigint; value: SemanticValue }>
  | Readonly<{ kind: 'simple'; value: 'false' | 'true' | 'null' }>;
export type DecodedItem<T> = Readonly<{
  value: T;
  decoded: SemanticValue;
  span: CborSpan;
}>;
export type Outpoint = Readonly<{
  transactionId: Hex;
  index: bigint;
  span: CborSpan;
}>;
export type AssetQuantity = Readonly<{
  policyId: Hex;
  assetName: Hex;
  quantity: bigint;
}>;
export type Value = Readonly<{
  coin: bigint;
  assets: readonly AssetQuantity[];
}>;
export type Datum = Readonly<{
  kind: 'hash' | 'inline';
  hash: Hex;
  cbor?: Hex;
  data?: SemanticValue;
  span: CborSpan;
}>;
export type Output = Readonly<{
  address: Hex;
  value: Value;
  datum?: Datum;
  referenceScript?: Script;
  exactSpan: CborSpan;
}>;
export type Script = Readonly<{
  language: 'native' | 'plutus:v1' | 'plutus:v2' | 'plutus:v3';
  hash: Hex;
  bytes: Hex;
  decoded?: SemanticValue;
  span: CborSpan;
}>;
export type RedeemerPurpose =
  | 'spend'
  | 'mint'
  | 'certificate'
  | 'withdrawal'
  | 'vote'
  | 'proposal';
export type Redeemer = Readonly<{
  purpose: RedeemerPurpose;
  index: bigint;
  target?: string;
  data: DecodedItem<Hex>;
  executionUnits: Readonly<{ memory: bigint; steps: bigint }>;
  exactSpan: CborSpan;
}>;
export type GovernanceVote = Readonly<{
  voter: string;
  scriptHash?: Hex;
  actions: readonly Readonly<{
    actionId: string;
    procedure: DecodedItem<Hex>;
  }>[];
  span: CborSpan;
}>;
export type GovernanceProposal = DecodedItem<Hex> &
  Readonly<{ policyScriptHashes: readonly Hex[] }>;
export type Certificate = Readonly<{
  kind: number;
  scriptCredentialHashes: readonly Hex[];
  targetScriptHashes: readonly Hex[];
}>;
export type BindingRequirement = Readonly<{
  kind:
    | 'resolved-input'
    | 'script'
    | 'datum'
    | 'language-view'
    | 'witness-proof';
  target: string;
  reason: string;
}>;
export type ReviewCompleteness =
  | Readonly<{ complete: true; signable: true; requirements: readonly [] }>
  | Readonly<{
      complete: false;
      signable: false;
      requirements: readonly BindingRequirement[];
    }>;
export type HardwareRepresentation =
  | Readonly<{ representable: true; unsupported: readonly [] }>
  | Readonly<{ representable: false; unsupported: readonly string[] }>;
export type ResolvedDatum =
  | Readonly<{ kind: 'hash'; hash: Hex; cbor?: Hex }>
  | Readonly<{ kind: 'inline'; cbor: Hex }>;

export type CommitmentContext = Readonly<{
  /** Exact, pinned language-view values keyed by Plutus language 0..2. */
  languageViews?: ReadonlyMap<number, Buffer>;
  /** Authenticated task-304 resolved-input facts, never inferred from user CBOR. */
  resolvedInputs?: ReadonlyArray<
    Readonly<{
      outpoint: Outpoint;
      value?: Value;
      datum?: ResolvedDatum;
      scriptHash?: Hex;
      referenceScript?: Script;
    }>
  >;
  /** Languages proven by the same authenticated context to execute this transaction. */
  usedPlutusLanguages?: readonly (0 | 1 | 2)[];
  /** Authenticated target-to-script bindings, keyed by the decoder's redeemer target. */
  redeemerScriptHashes?: ReadonlyMap<string, Hex>;
  /** Existing witness CBOR values independently verified against the exact body hash. */
  verifiedWitnesses?: ReadonlySet<Hex>;
}>;

export type SemanticTransaction = Readonly<{
  envelope: ExactTransactionEnvelope;
  transactionId: Hex;
  inputs: Readonly<{
    normal: readonly Outpoint[];
    collateral: readonly Outpoint[];
    reference: readonly Outpoint[];
  }>;
  outputs: readonly Output[];
  fee: bigint;
  validityInterval: Readonly<{
    invalidBefore?: bigint;
    invalidHereafter?: bigint;
  }>;
  mint: readonly AssetQuantity[];
  certificates: readonly DecodedItem<Certificate>[];
  withdrawals: readonly Readonly<{
    account: Hex;
    coin: bigint;
    span: CborSpan;
  }>[];
  requiredSigners: readonly Hex[];
  networkId?: 0 | 1;
  collateral: Readonly<{
    return?: Output;
    total?: bigint;
    maximumLoss?: Value;
    maximumLossRequirement?: BindingRequirement;
  }>;
  governance: Readonly<{
    votes: readonly GovernanceVote[];
    proposals: readonly GovernanceProposal[];
    treasuryValue?: bigint;
    donation?: bigint;
  }>;
  auxiliaryData?: DecodedItem<Hex>;
  witnesses: Readonly<{
    vkeys: readonly Hex[];
    bootstrap: readonly Hex[];
    nativeScripts: readonly Script[];
    plutusScripts: readonly Script[];
    datums: readonly DecodedItem<Hex>[];
    redeemers: readonly Redeemer[];
  }>;
  commitments: Readonly<{ auxiliaryDataHash?: Hex; scriptDataHash?: Hex }>;
  effects: readonly Readonly<{
    kind: string;
    value: unknown;
    span?: CborSpan;
  }>[];
  review: ReviewCompleteness;
}>;

const fail = (message?: string): never => {
  throw new TransactionSemanticError(message);
};
const hash = (bytes: Buffer): Hex =>
  Buffer.from(blake2b(bytes, undefined, 32)).toString('hex');
const scriptHash = (language: number, bytes: Buffer): Hex =>
  Buffer.from(
    blake2b(Buffer.concat([Buffer.from([language]), bytes]), undefined, 28)
  ).toString('hex');
const array = (item: CborItem): readonly CborItem[] =>
  item.major === 4 && item.items ? item.items : fail();
const map = (item: CborItem) =>
  item.major === 5 && item.entries ? item.entries : fail();
const uint = (item: CborItem): bigint =>
  item.major === 0 && item.value !== undefined ? item.value : fail();
const integer = (item: CborItem): bigint =>
  item.value !== undefined && (item.major === 0 || item.major === 1)
    ? item.value
    : fail();
const isNull = (source: Buffer, item: CborItem): boolean =>
  item.major === 7 &&
  item.span.end === item.span.start + 1 &&
  source[item.span.start] === 0xf6;
const encoded = (source: Buffer, item: CborItem): Buffer =>
  bytesForSpan(source, item.span);
const stringPayload = (
  source: Buffer,
  item: CborItem,
  major: 2 | 3
): Buffer => {
  if (item.major !== major) fail();
  if (item.content) return bytesForSpan(source, item.content);
  if (!item.items) fail();
  return Buffer.concat(
    item.items.map((chunk) => stringPayload(source, chunk, major))
  );
};
const payload = (source: Buffer, item: CborItem): Buffer =>
  stringPayload(source, item, 2);
const text = (source: Buffer, item: CborItem): Buffer =>
  stringPayload(source, item, 3);
const hex = (source: Buffer, item: CborItem, length?: number): Hex => {
  const value = payload(source, item);
  if (length !== undefined && value.length !== length) fail();
  return value.toString('hex');
};
const width = (source: Buffer, item: CborItem, length: number): void => {
  hex(source, item, length);
};
const semanticValue = (source: Buffer, item: CborItem): SemanticValue => {
  if ((item.major === 0 || item.major === 1) && item.value !== undefined)
    return { kind: 'integer', value: item.value };
  if (item.major === 2)
    return { kind: 'bytes', hex: payload(source, item).toString('hex') };
  if (item.major === 3)
    return { kind: 'text', bytes: text(source, item).toString('hex') };
  if (item.major === 4)
    return {
      kind: 'array',
      items: array(item).map((child) => semanticValue(source, child)),
    };
  if (item.major === 5)
    return {
      kind: 'map',
      entries: map(item).map((entry) => ({
        key: semanticValue(source, entry.key),
        value: semanticValue(source, entry.value),
      })),
    };
  if (item.major === 6 && item.tag !== undefined && item.items?.length === 1)
    return {
      kind: 'tag',
      tag: item.tag,
      value: semanticValue(source, item.items[0]),
    };
  if (item.major === 7 && item.span.end === item.span.start + 1) {
    const simple = source[item.span.start];
    if (simple === 0xf4) return { kind: 'simple', value: 'false' };
    if (simple === 0xf5) return { kind: 'simple', value: 'true' };
    if (simple === 0xf6) return { kind: 'simple', value: 'null' };
  }
  return fail('unsupported semantic value');
};
const semanticKey = (source: Buffer, item: CborItem): string => {
  if ((item.major === 0 || item.major === 1) && item.value !== undefined)
    return `i:${item.value}`;
  if (item.major === 2) return `b:${payload(source, item).toString('hex')}`;
  if (item.major === 3) return `t:${text(source, item).toString('hex')}`;
  if (item.major === 4)
    return `a:${array(item)
      .map((child) => semanticKey(source, child))
      .join('|')}`;
  if (item.major === 5)
    return `m:${map(item)
      .map(
        (entry) =>
          `${semanticKey(source, entry.key)}=${semanticKey(
            source,
            entry.value
          )}`
      )
      .sort()
      .join('|')}`;
  if (item.major === 6 && item.tag !== undefined && item.items?.length === 1)
    return `g:${item.tag}:${semanticKey(source, item.items[0])}`;
  return `s:${encoded(source, item).toString('hex')}`;
};
const exact = (source: Buffer, item: CborItem): DecodedItem<Hex> => ({
  value: encoded(source, item).toString('hex'),
  decoded: semanticValue(source, item),
  span: item.span,
});
const noDuplicate = (seen: Set<string>, value: string): void => {
  if (seen.has(value)) fail('duplicate semantic value');
  seen.add(value);
};
const key = (item: CborItem): number => {
  const value = uint(item);
  if (value > BigInt(Number.MAX_SAFE_INTEGER)) fail();
  return Number(value);
};
const boundedUint = (item: CborItem, maximum: bigint): bigint => {
  const result = uint(item);
  if (result > maximum) fail();
  return result;
};
const set = (item: CborItem, nonempty = false): readonly CborItem[] => {
  let content = item;
  if (content.major === 6) {
    if (
      content.tag !== BigInt(258) ||
      !content.items ||
      content.items.length !== 1
    )
      fail();
    [content] = content.items;
  }
  const members = array(content);
  if (nonempty && !members.length) fail();
  return members;
};
const fields = (item: CborItem): ReadonlyMap<number, CborItem> => {
  const found = new Map<number, CborItem>();
  for (const entry of map(item)) {
    const field = key(entry.key);
    if (found.has(field)) fail();
    found.set(field, entry.value);
  }
  return found;
};
const only = (
  item: CborItem,
  accepted: readonly number[]
): ReadonlyMap<number, CborItem> => {
  const result = fields(item);
  for (const value of result.keys())
    if (!accepted.includes(value)) fail('unknown Conway field');
  return result;
};
const cborItem = (source: Buffer, wrapper: CborItem): CborItem => {
  if (
    wrapper.major !== 6 ||
    wrapper.tag !== BigInt(24) ||
    !wrapper.items ||
    wrapper.items.length !== 1
  )
    fail();
  const bytes = payload(source, wrapper.items[0]);
  const result = parseCborItem(bytes);
  if (result.span.end !== bytes.length) fail();
  return result;
};
const plutusData = (item: CborItem, source: Buffer): void => {
  if (item.major === 0 || item.major === 1) return;
  if (item.major === 2) {
    const chunks = item.items || [item];
    chunks.forEach((chunk) => {
      if (!chunk.content || bytesForSpan(source, chunk.content).length > 64)
        fail('invalid bounded bytes');
    });
    return;
  }
  if (item.major === 4) {
    array(item).forEach((child) => plutusData(child, source));
    return;
  }
  if (item.major === 5) {
    const seen = new Set<string>();
    map(item).forEach(({ key, value }) => {
      noDuplicate(seen, semanticKey(source, key));
      plutusData(key, source);
      plutusData(value, source);
    });
    return;
  }
  if (
    item.major !== 6 ||
    item.tag === undefined ||
    !item.items ||
    item.items.length !== 1
  )
    fail();
  const [child] = item.items;
  if ((item.tag === BigInt(2) || item.tag === BigInt(3)) && child.major === 2) {
    const chunks = child.items || [child];
    chunks.forEach((chunk) => {
      if (!chunk.content || bytesForSpan(source, chunk.content).length > 64)
        fail('invalid bounded bytes');
    });
    return;
  }
  if (
    ((item.tag >= BigInt(121) && item.tag <= BigInt(127)) ||
      (item.tag >= BigInt(1280) && item.tag <= BigInt(1400))) &&
    child.major === 4
  ) {
    array(child).forEach((entry) => plutusData(entry, source));
    return;
  }
  if (item.tag === BigInt(102) && child.major === 4) {
    const parts = array(child);
    if (parts.length !== 2) fail();
    uint(parts[0]);
    array(parts[1]).forEach((entry) => plutusData(entry, source));
    return;
  }
  fail();
};
const credential = (
  source: Buffer,
  item: CborItem,
  allowDrep = false
): Hex | undefined => {
  const parts = array(item);
  const kind = uint(parts[0]);
  if (kind <= BigInt(1)) {
    if (parts.length !== 2) fail();
    const hashValue = hex(source, parts[1], 28);
    return kind === BigInt(1) ? hashValue : undefined;
  }
  if (!allowDrep || kind > BigInt(3) || parts.length !== 1) fail();
  return undefined;
};
const actionId = (source: Buffer, item: CborItem): string => {
  const parts = array(item);
  if (parts.length !== 2) fail();
  return `${hex(source, parts[0], 32)}:${boundedUint(
    parts[1],
    BigInt(0xffff)
  )}`;
};
const nullableActionId = (source: Buffer, item: CborItem): void => {
  if (!isNull(source, item)) actionId(source, item);
};
const rewardAccount = (
  source: Buffer,
  item: CborItem
): Readonly<{ account: Hex; scriptHash?: Hex }> => {
  const bytes = payload(source, item);
  if (bytes.length !== 29 || (bytes[0] >> 4 !== 0xe && bytes[0] >> 4 !== 0xf))
    fail('invalid reward account');
  return {
    account: bytes.toString('hex'),
    scriptHash:
      bytes[0] >> 4 === 0xf ? bytes.subarray(1).toString('hex') : undefined,
  };
};
const anchor = (source: Buffer, item: CborItem): void => {
  const parts = array(item);
  if (parts.length !== 2 || parts[0].major !== 3) fail();
  if (text(source, parts[0]).length > 128) fail();
  width(source, parts[1], 32);
};
const nullableAnchor = (source: Buffer, item: CborItem): void => {
  if (!isNull(source, item)) anchor(source, item);
};
const unitInterval = (item: CborItem): void => {
  if (
    item.major !== 6 ||
    item.tag !== BigInt(30) ||
    !item.items ||
    item.items.length !== 1
  )
    fail();
  const parts = array(item.items[0]);
  if (parts.length !== 2) fail();
  if (uint(parts[1]) === BigInt(0) || uint(parts[0]) > uint(parts[1])) fail();
};
const nonnegativeInterval = (item: CborItem): void => {
  if (
    item.major !== 6 ||
    item.tag !== BigInt(30) ||
    !item.items ||
    item.items.length !== 1
  )
    fail();
  const parts = array(item.items[0]);
  if (parts.length !== 2 || uint(parts[1]) === BigInt(0)) fail();
  uint(parts[0]);
};
const nativeScript = (source: Buffer, item: CborItem): void => {
  const parts = array(item);
  if (!parts.length) fail();
  const tag = uint(parts[0]);
  if (tag === BigInt(0) && parts.length === 2) width(source, parts[1], 28);
  else if ((tag === BigInt(1) || tag === BigInt(2)) && parts.length === 2)
    array(parts[1]).forEach((child) => nativeScript(source, child));
  else if (tag === BigInt(3) && parts.length === 3) {
    const threshold = integer(parts[1]);
    if (
      threshold < BigInt(-9223372036854775808) ||
      threshold > BigInt(9223372036854775807)
    )
      fail();
    array(parts[2]).forEach((child) => nativeScript(source, child));
  } else if ((tag === BigInt(4) || tag === BigInt(5)) && parts.length === 2)
    uint(parts[1]);
  else fail();
};
const metadata = (source: Buffer, item: CborItem): void => {
  if (item.major === 0 || item.major === 1) return;
  if (item.major === 2) {
    if (payload(source, item).length > 64) fail();
    return;
  }
  if (item.major === 3) {
    if (text(source, item).length > 64) fail();
    return;
  }
  if (item.major === 4) {
    array(item).forEach((child) => metadata(source, child));
    return;
  }
  if (item.major === 5) {
    const seen = new Set<string>();
    for (const entry of map(item)) {
      noDuplicate(seen, semanticKey(source, entry.key));
      metadata(source, entry.key);
      metadata(source, entry.value);
    }
    return;
  }
  fail();
};
const transactionMetadata = (source: Buffer, item: CborItem): void => {
  for (const entry of map(item)) {
    uint(entry.key);
    metadata(source, entry.value);
  }
};
const auxiliaryData = (source: Buffer, item: CborItem): void => {
  if (item.major === 5) {
    transactionMetadata(source, item);
    return;
  }
  if (item.major === 4) {
    const parts = array(item);
    if (parts.length !== 2) fail();
    transactionMetadata(source, parts[0]);
    array(parts[1]).forEach((script) => nativeScript(source, script));
    return;
  }
  if (
    item.major !== 6 ||
    item.tag !== BigInt(259) ||
    !item.items ||
    item.items.length !== 1
  )
    fail();
  const fields = only(item.items[0], [0, 1, 2, 3, 4]);
  if (fields.has(0)) transactionMetadata(source, fields.get(0)!);
  if (fields.has(1))
    array(fields.get(1)!).forEach((script) => nativeScript(source, script));
  ([2, 3, 4] as const).forEach((field) => {
    if (!fields.has(field)) return;
    array(fields.get(field)!).forEach((script) => payload(source, script));
  });
};
const decodeInput = (source: Buffer, item: CborItem): Outpoint => {
  const parts = array(item);
  if (parts.length !== 2) fail();
  return {
    transactionId: hex(source, parts[0], 32),
    index: boundedUint(parts[1], BigInt(0xffff)),
    span: item.span,
  };
};
const decodeInputs = (
  source: Buffer,
  item: CborItem,
  nonempty = false
): readonly Outpoint[] => {
  const seen = new Set<string>();
  return set(item, nonempty).map((member) => {
    const result = decodeInput(source, member);
    noDuplicate(seen, `${result.transactionId}:${result.index}`);
    return result;
  });
};
const value = (
  source: Buffer,
  item: CborItem,
  allowNegative = false
): Value => {
  if (item.major === 0) return { coin: uint(item), assets: [] };
  const parts = array(item);
  if (parts.length !== 2) fail();
  const coin = uint(parts[0]);
  const policies = map(parts[1]);
  const assets: AssetQuantity[] = [];
  const policyIds = new Set<string>();
  for (const { key: policy, value: assetMap } of policies) {
    const policyId = hex(source, policy, 28);
    noDuplicate(policyIds, policyId);
    const names = new Set<string>();
    for (const { key: name, value: quantity } of map(assetMap)) {
      const assetName = hex(source, name);
      if (Buffer.from(assetName, 'hex').length > 32) fail();
      noDuplicate(names, assetName);
      const amount = allowNegative ? integer(quantity) : uint(quantity);
      if (allowNegative ? amount === BigInt(0) : amount <= BigInt(0)) fail();
      assets.push({ policyId, assetName, quantity: amount });
    }
    if (!names.size) fail();
  }
  return { coin, assets };
};
const sumValues = (values: readonly Value[]): Value => {
  const assets = new Map<string, AssetQuantity>();
  let coin = BigInt(0);
  values.forEach((candidate) => {
    coin += candidate.coin;
    candidate.assets.forEach((asset) => {
      const id = `${asset.policyId}:${asset.assetName}`;
      const previous = assets.get(id);
      assets.set(id, {
        ...asset,
        quantity: (previous?.quantity || BigInt(0)) + asset.quantity,
      });
    });
  });
  return {
    coin,
    assets: [...assets.values()].filter(
      ({ quantity }) => quantity !== BigInt(0)
    ),
  };
};
const collateralLoss = (inputs: readonly Value[], returned?: Value): Value => {
  const total = sumValues(inputs);
  const change = returned || { coin: BigInt(0), assets: [] };
  const returnedAssets = new Map(
    change.assets.map((asset) => [
      `${asset.policyId}:${asset.assetName}`,
      asset.quantity,
    ])
  );
  for (const asset of total.assets) {
    const id = `${asset.policyId}:${asset.assetName}`;
    if ((returnedAssets.get(id) || BigInt(0)) !== asset.quantity)
      fail('collateral return must preserve every non-ADA asset');
    returnedAssets.delete(id);
  }
  if ([...returnedAssets.values()].some((quantity) => quantity !== BigInt(0)))
    fail('collateral return contains an unknown asset');
  const coin = total.coin - change.coin;
  if (coin < BigInt(0))
    fail('collateral return exceeds collateral input value');
  return { coin, assets: [] };
};
const decodeOutput = (source: Buffer, item: CborItem): Output => {
  let address: Hex;
  let amount: Value;
  let datum: Datum | undefined;
  let referenceScript: Script | undefined;
  if (item.major === 4) {
    const parts = array(item);
    if (parts.length !== 2 && parts.length !== 3) fail();
    address = hex(source, parts[0]);
    amount = value(source, parts[1]);
    if (parts[2])
      datum = {
        kind: 'hash',
        hash: hex(source, parts[2], 32),
        span: parts[2].span,
      };
  } else {
    const output = only(item, [0, 1, 2, 3]);
    const rawAddress = output.get(0);
    const rawValue = output.get(1);
    if (!rawAddress || !rawValue) fail();
    address = hex(source, rawAddress);
    amount = value(source, rawValue);
    const rawDatum = output.get(2);
    if (rawDatum) {
      const parts = array(rawDatum);
      if (parts.length !== 2 || uint(parts[0]) > BigInt(1)) fail();
      if (uint(parts[0]) === BigInt(0))
        datum = {
          kind: 'hash',
          hash: hex(source, parts[1], 32),
          span: parts[1].span,
        };
      else {
        const wrapper = parts[1];
        if (!wrapper.items || wrapper.items.length !== 1) fail();
        const embeddedSource = payload(source, wrapper.items[0]);
        const data = cborItem(source, wrapper);
        plutusData(data, embeddedSource);
        datum = {
          kind: 'inline',
          hash: hash(embeddedSource),
          data: semanticValue(embeddedSource, data),
          cbor: embeddedSource.toString('hex'),
          span: rawDatum.span,
        };
      }
    }
    const rawScript = output.get(3);
    if (rawScript) referenceScript = decodeReferenceScript(source, rawScript);
  }
  return {
    address,
    value: amount,
    datum,
    referenceScript,
    exactSpan: item.span,
  };
};
const decodeReferenceScript = (source: Buffer, wrapper: CborItem): Script => {
  if (!wrapper.items || wrapper.items.length !== 1) fail();
  const embeddedSource = payload(source, wrapper.items[0]);
  const embedded = cborItem(source, wrapper);
  const parts = array(embedded);
  if (parts.length !== 2) fail();
  const languageId = Number(uint(parts[0]));
  if (languageId < 0 || languageId > 3) fail();
  const language =
    languageId === 0
      ? 'native'
      : (`plutus:v${languageId}` as Script['language']);
  let raw: Buffer;
  if (languageId === 0) {
    nativeScript(embeddedSource, parts[1]);
    raw = encoded(embeddedSource, parts[1]);
  } else raw = payload(embeddedSource, parts[1]);
  return {
    language,
    hash: scriptHash(languageId, raw),
    bytes: raw.toString('hex'),
    decoded:
      languageId === 0 ? semanticValue(embeddedSource, parts[1]) : undefined,
    span: wrapper.span,
  };
};

export const decodeConwayOutput = (cbor: Buffer): Output => {
  const item = parseCborItem(cbor);
  if (item.span.end !== cbor.length) fail('trailing transaction output bytes');
  return decodeOutput(cbor, item);
};

export const decodeConwayValue = (cbor: Buffer): Value => {
  const item = parseCborItem(cbor);
  if (item.span.end !== cbor.length) fail('trailing value bytes');
  return value(cbor, item);
};
const poolParameters = (source: Buffer, item: CborItem): void => {
  const parts = array(item);
  if (parts.length !== 9) fail();
  width(source, parts[0], 28);
  width(source, parts[1], 32);
  uint(parts[2]);
  uint(parts[3]);
  unitInterval(parts[4]);
  rewardAccount(source, parts[5]);
  const owners = new Set<string>();
  for (const owner of set(parts[6]))
    noDuplicate(owners, hex(source, owner, 28));
  for (const relay of array(parts[7])) {
    const values = array(relay);
    const tag = Number(uint(values[0]));
    if (tag === 0 && values.length === 4) {
      if (!isNull(source, values[1])) boundedUint(values[1], BigInt(65535));
      if (!isNull(source, values[2])) width(source, values[2], 4);
      if (!isNull(source, values[3])) width(source, values[3], 16);
    } else if (tag === 1 && values.length === 3) {
      if (!isNull(source, values[1])) boundedUint(values[1], BigInt(65535));
      if (text(source, values[2]).length > 128) fail();
    } else if (tag === 2 && values.length === 2) {
      if (text(source, values[1]).length > 128) fail();
    } else fail();
  }
  if (!isNull(source, parts[8])) {
    const metadata = array(parts[8]);
    if (
      metadata.length !== 2 ||
      text(source, metadata[0]).length > 128 ||
      metadata[1].major !== 2
    )
      fail();
  }
};
const protocolUpdate = (item: CborItem): void => {
  const update = only(item, [
    0,
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    16,
    17,
    18,
    19,
    20,
    21,
    22,
    23,
    24,
    25,
    26,
    27,
    28,
    29,
    30,
    31,
    32,
    33,
  ]);
  for (const [field, candidate] of update) {
    if ([0, 1, 5, 6, 16, 17, 30, 31].includes(field)) uint(candidate);
    else if ([2, 3, 7, 22, 28, 29, 32].includes(field))
      boundedUint(candidate, BigInt(0xffffffff));
    else if ([4, 8, 23, 24, 27].includes(field))
      boundedUint(candidate, BigInt(0xffff));
    else if ([10, 11].includes(field)) unitInterval(candidate);
    else if ([9, 33].includes(field)) nonnegativeInterval(candidate);
    else if (field === 18)
      for (const model of map(candidate)) {
        boundedUint(model.key, BigInt(255));
        array(model.value).forEach((cost) => {
          const value = integer(cost);
          if (
            value < BigInt(-9223372036854775808) ||
            value > BigInt(9223372036854775807)
          )
            fail();
        });
      }
    else if (field === 19) {
      const prices = array(candidate);
      if (prices.length !== 2) fail();
      prices.forEach(unitInterval);
    } else if (field === 20 || field === 21) {
      const units = array(candidate);
      if (units.length !== 2) fail();
      units.forEach(uint);
    } else {
      const thresholds = array(candidate);
      if (thresholds.length !== (field === 25 ? 5 : 10)) fail();
      thresholds.forEach(unitInterval);
    }
  }
};
const certificate = (source: Buffer, item: CborItem): Certificate => {
  const parts = array(item);
  const tag = Number(uint(parts[0]));
  const arity: Readonly<Record<number, number>> = {
    0: 2,
    1: 2,
    2: 3,
    3: 2,
    4: 3,
    7: 3,
    8: 3,
    9: 3,
    10: 4,
    11: 4,
    12: 4,
    13: 5,
    14: 3,
    15: 3,
    16: 4,
    17: 3,
    18: 3,
  };
  if (arity[tag] !== parts.length) fail();
  const scriptCredentialHashes: Hex[] = [];
  const targetScriptHashes: Hex[] = [];
  const collect = (candidate: CborItem, allowDrep = false) => {
    const script = credential(source, candidate, allowDrep);
    if (script) scriptCredentialHashes.push(script);
  };
  const collectTarget = (candidate: CborItem, allowDrep = false) => {
    const script = credential(source, candidate, allowDrep);
    if (script) targetScriptHashes.push(script);
  };
  if ([0, 1, 2, 7, 8, 9, 10, 11, 12, 13].includes(tag)) collect(parts[1]);
  if ([14, 15, 16, 17, 18].includes(tag)) collect(parts[1]);
  if ([2, 10, 11, 13].includes(tag)) width(source, parts[2], 28);
  if ([9, 10, 12, 13].includes(tag))
    collectTarget(
      parts[tag === 9 ? 2 : tag === 10 ? 3 : tag === 12 ? 2 : 3],
      true
    );
  if (tag === 3) poolParameters(source, parts[1]);
  if (tag === 4) {
    width(source, parts[1], 28);
    uint(parts[2]);
  }
  if (tag === 14) collectTarget(parts[2]);
  if ([7, 8].includes(tag)) uint(parts[2]);
  if ([11, 12].includes(tag)) uint(parts[3]);
  if (tag === 13) uint(parts[4]);
  if ([16, 17].includes(tag)) uint(parts[2]);
  if ([15, 18].includes(tag)) nullableAnchor(source, parts[2]);
  if (tag === 16) nullableAnchor(source, parts[3]);
  return {
    kind: tag,
    scriptCredentialHashes: [...new Set(scriptCredentialHashes)],
    targetScriptHashes: [...new Set(targetScriptHashes)],
  };
};
const voter = (
  source: Buffer,
  item: CborItem
): Readonly<{ id: string; scriptHash?: Hex }> => {
  const parts = array(item);
  if (parts.length !== 2) fail();
  const kind = boundedUint(parts[0], BigInt(4));
  const credentialHash = hex(source, parts[1], 28);
  return {
    id: `${kind}:${credentialHash}`,
    scriptHash:
      kind === BigInt(1) || kind === BigInt(3) ? credentialHash : undefined,
  };
};
const governanceAction = (source: Buffer, item: CborItem): readonly Hex[] => {
  const parts = array(item);
  const tag = Number(uint(parts[0]));
  if (tag < 0 || tag > 6) fail();
  const policyScriptHashes: Hex[] = [];
  if (tag === 0) {
    if (parts.length !== 4) fail();
    nullableActionId(source, parts[1]);
    protocolUpdate(parts[2]);
    if (!isNull(source, parts[3]))
      policyScriptHashes.push(hex(source, parts[3], 28));
  } else if (tag === 1) {
    if (parts.length !== 3) fail();
    nullableActionId(source, parts[1]);
    const version = array(parts[2]);
    if (version.length !== 2) fail();
    boundedUint(version[0], BigInt(12));
    uint(version[1]);
  } else if (tag === 2) {
    if (parts.length !== 3) fail();
    const seen = new Set<string>();
    for (const entry of map(parts[1])) {
      noDuplicate(seen, semanticKey(source, entry.key));
      rewardAccount(source, entry.key);
      uint(entry.value);
    }
    if (!isNull(source, parts[2]))
      policyScriptHashes.push(hex(source, parts[2], 28));
  } else if (tag === 3) {
    if (parts.length !== 2) fail();
    nullableActionId(source, parts[1]);
  } else if (tag === 4) {
    if (parts.length !== 5) fail();
    nullableActionId(source, parts[1]);
    const removed = new Set<string>();
    set(parts[2]).forEach((candidate) => {
      noDuplicate(removed, semanticKey(source, candidate));
      credential(source, candidate);
    });
    const added = new Set<string>();
    map(parts[3]).forEach(({ key: candidate, value }) => {
      noDuplicate(added, semanticKey(source, candidate));
      credential(source, candidate);
      uint(value);
    });
    unitInterval(parts[4]);
  } else if (tag === 5) {
    if (parts.length !== 3) fail();
    nullableActionId(source, parts[1]);
    const constitution = array(parts[2]);
    if (constitution.length !== 2) fail();
    anchor(source, constitution[0]);
    if (!isNull(source, constitution[1])) width(source, constitution[1], 28);
  } else if (parts.length !== 1) fail();
  return policyScriptHashes;
};
const decodeRedeemers = (
  source: Buffer,
  item: CborItem
): readonly Redeemer[] => {
  const result: Redeemer[] = [];
  const seen = new Set<string>();
  const decode = (
    tagItem: CborItem,
    indexItem: CborItem,
    data: CborItem,
    units: CborItem,
    span: CborSpan
  ) => {
    const tag = Number(uint(tagItem));
    const purpose: RedeemerPurpose[] = [
      'spend',
      'mint',
      'certificate',
      'withdrawal',
      'vote',
      'proposal',
    ];
    if (!purpose[tag]) fail();
    const index = boundedUint(indexItem, BigInt(0xffffffff));
    plutusData(data, source);
    const ex = array(units);
    if (ex.length !== 2) fail();
    const value: Redeemer = {
      purpose: purpose[tag],
      index,
      data: exact(source, data),
      executionUnits: { memory: uint(ex[0]), steps: uint(ex[1]) },
      exactSpan: span,
    };
    noDuplicate(seen, `${value.purpose}:${value.index}`);
    result.push(value);
  };
  if (item.major === 4) {
    const members = array(item);
    if (!members.length) fail();
    members.forEach((entry) => {
      const parts = array(entry);
      if (parts.length !== 4) fail();
      decode(parts[0], parts[1], parts[2], parts[3], entry.span);
    });
  } else if (item.major === 5) {
    const members = map(item);
    if (!members.length) fail();
    members.forEach(({ key: k, value: v }) => {
      const target = array(k);
      const payloadValue = array(v);
      if (target.length !== 2 || payloadValue.length !== 2) fail();
      decode(target[0], target[1], payloadValue[0], payloadValue[1], v.span);
    });
  } else fail();
  return result;
};
const languageMap = (
  views: ReadonlyMap<number, Buffer>,
  languages: readonly number[]
): Buffer => {
  const entries = [...new Set(languages)]
    .map((language) => {
      if (language < 0 || language > 2) fail();
      const value = views.get(language);
      if (!value) fail('missing pinned language view');
      try {
        if (parseCborItem(value).span.end !== value.length)
          fail('invalid pinned language view');
      } catch (error) {
        if (error instanceof TransactionSemanticError) throw error;
        fail('invalid pinned language view');
      }
      return {
        key:
          language === 0 ? Buffer.from([0x41, 0x00]) : Buffer.from([language]),
        value,
      };
    })
    .sort(
      (left, right) =>
        left.key.length - right.key.length ||
        Buffer.compare(left.key, right.key)
    );
  if (entries.length > 3) fail();
  const encodedEntries = entries.reduce<Buffer[]>((result, entry) => {
    result.push(entry.key, entry.value);
    return result;
  }, []);
  return Buffer.concat([
    Buffer.from([0xa0 | entries.length]),
    ...encodedEntries,
  ]);
};
const hardware = (
  transaction: Pick<SemanticTransaction, 'governance'>
): HardwareRepresentation =>
  transaction.governance.votes.length ||
  transaction.governance.proposals.length ||
  transaction.governance.treasuryValue !== undefined ||
  transaction.governance.donation !== undefined
    ? { representable: false, unsupported: ['Conway CIP-95 governance'] }
    : { representable: true, unsupported: [] };

export const hardwareRepresentation = (
  transaction: SemanticTransaction
): HardwareRepresentation => hardware(transaction);

export const decodeConwayTransaction = (
  envelope: ExactTransactionEnvelope,
  context: CommitmentContext = {}
): SemanticTransaction => {
  const source = envelope.cbor;
  const body = only(envelope.body, [
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
    20,
    21,
    22,
  ]);
  const witnesses = only(envelope.witnessSet, [0, 1, 2, 3, 4, 5, 6, 7]);
  const normal = decodeInputs(source, body.get(0) || fail());
  const collateralInputs = body.has(13)
    ? decodeInputs(source, body.get(13)!, true)
    : [];
  const referenceInputs = body.has(18)
    ? decodeInputs(source, body.get(18)!, true)
    : [];
  const occupiedInputs = new Set(
    normal.map((input) => `${input.transactionId}:${input.index}`)
  );
  for (const input of collateralInputs.concat(referenceInputs)) {
    const id = `${input.transactionId}:${input.index}`;
    if (occupiedInputs.has(id)) fail('input appears in multiple roles');
    occupiedInputs.add(id);
  }
  const outputs = array(body.get(1) || fail()).map((item) =>
    decodeOutput(source, item)
  );
  const fee = uint(body.get(2) || fail());
  const certificates: DecodedItem<Certificate>[] = [];
  if (body.has(4)) {
    const seen = new Set<string>();
    for (const item of set(body.get(4)!, true)) {
      noDuplicate(seen, semanticKey(source, item));
      certificates.push({
        value: certificate(source, item),
        decoded: semanticValue(source, item),
        span: item.span,
      });
    }
  }
  const withdrawals: {
    account: Hex;
    coin: bigint;
    scriptHash?: Hex;
    span: CborSpan;
  }[] = [];
  if (body.has(5)) {
    const seen = new Set<string>();
    for (const entry of map(body.get(5)!)) {
      const decoded = rewardAccount(source, entry.key);
      noDuplicate(seen, decoded.account);
      withdrawals.push({
        ...decoded,
        coin: uint(entry.value),
        span: entry.value.span,
      });
    }
    if (!withdrawals.length) fail();
  }
  const mint: AssetQuantity[] = [];
  if (body.has(9)) {
    const seen = new Set<string>();
    for (const { key: policy, value: assets } of map(body.get(9)!)) {
      const policyId = hex(source, policy, 28);
      let count = 0;
      for (const { key: name, value: amount } of map(assets)) {
        const assetName = hex(source, name);
        if (Buffer.from(assetName, 'hex').length > 32) fail();
        const quantity = integer(amount);
        if (
          !quantity ||
          quantity < BigInt(-9223372036854775808) ||
          quantity > BigInt(9223372036854775807)
        )
          fail();
        noDuplicate(seen, `${policyId}:${assetName}`);
        mint.push({ policyId, assetName, quantity });
        count += 1;
      }
      if (!count) fail();
    }
    if (!mint.length) fail();
  }
  const requiredSigners: Hex[] = [];
  if (body.has(14)) {
    const seen = new Set<string>();
    for (const item of set(body.get(14)!, true)) {
      const signer = hex(source, item, 28);
      noDuplicate(seen, signer);
      requiredSigners.push(signer);
    }
  }
  const collateralReturn = body.has(16)
    ? decodeOutput(source, body.get(16)!)
    : undefined;
  const totalCollateral = body.has(17) ? uint(body.get(17)!) : undefined;
  const networkId = body.has(15)
    ? (Number(boundedUint(body.get(15)!, BigInt(1))) as 0 | 1)
    : undefined;
  const treasuryValue = body.has(21) ? uint(body.get(21)!) : undefined;
  const donation = body.has(22) ? uint(body.get(22)!) : undefined;
  if (donation === BigInt(0)) fail();
  const voteEffects: GovernanceVote[] = [];
  if (body.has(19)) {
    const voters = map(body.get(19)!);
    if (!voters.length) fail();
    const voterSeen = new Set<string>();
    for (const entry of voters) {
      const voterDetails = voter(source, entry.key);
      noDuplicate(voterSeen, voterDetails.id);
      const actions = map(entry.value);
      if (!actions.length) fail();
      const actionSeen = new Set<string>();
      const decodedActions: GovernanceVote['actions'][number][] = [];
      for (const action of actions) {
        const actionKey = actionId(source, action.key);
        noDuplicate(actionSeen, actionKey);
        const procedure = array(action.value);
        if (procedure.length !== 2 || uint(procedure[0]) > BigInt(2)) fail();
        nullableAnchor(source, procedure[1]);
        decodedActions.push({
          actionId: actionKey,
          procedure: exact(source, action.value),
        });
      }
      voteEffects.push({
        voter: voterDetails.id,
        scriptHash: voterDetails.scriptHash,
        actions: decodedActions,
        span: { start: entry.key.span.start, end: entry.value.span.end },
      });
    }
  }
  const proposals: GovernanceProposal[] = [];
  if (body.has(20)) {
    const seen = new Set<string>();
    for (const proposal of set(body.get(20)!, true)) {
      const parts = array(proposal);
      if (parts.length !== 4) fail();
      uint(parts[0]);
      rewardAccount(source, parts[1]);
      const policyScriptHashes = governanceAction(source, parts[2]);
      anchor(source, parts[3]);
      noDuplicate(seen, semanticKey(source, proposal));
      proposals.push({ ...exact(source, proposal), policyScriptHashes });
    }
  }
  const vkeys: Hex[] = [];
  const bootstrap: Hex[] = [];
  const nativeScripts: Script[] = [];
  const plutusScripts: Script[] = [];
  const datums: DecodedItem<Hex>[] = [];
  const vkeyHashes = new Set<string>();
  const bootstrapKeys = new Set<string>();
  const scriptHashes = new Set<string>();
  const datumHashes = new Set<string>();
  if (witnesses.has(0))
    for (const item of set(witnesses.get(0)!, true)) {
      const parts = array(item);
      if (parts.length !== 2) fail();
      const publicKey = hex(source, parts[0], 32);
      width(source, parts[1], 64);
      noDuplicate(vkeyHashes, publicKey);
      vkeys.push(encoded(source, item).toString('hex'));
    }
  if (witnesses.has(2))
    for (const item of set(witnesses.get(2)!, true)) {
      const parts = array(item);
      if (parts.length !== 4) fail();
      const publicKey = hex(source, parts[0], 32);
      width(source, parts[1], 64);
      width(source, parts[2], 32);
      payload(source, parts[3]);
      noDuplicate(bootstrapKeys, publicKey);
      bootstrap.push(encoded(source, item).toString('hex'));
    }
  if (witnesses.has(1))
    for (const item of set(witnesses.get(1)!, true)) {
      nativeScript(source, item);
      const raw = encoded(source, item);
      const script = {
        language: 'native' as const,
        hash: scriptHash(0, raw),
        bytes: raw.toString('hex'),
        decoded: semanticValue(source, item),
        span: item.span,
      };
      noDuplicate(scriptHashes, script.hash);
      nativeScripts.push(script);
    }
  ([3, 6, 7] as const).forEach((field) => {
    if (!witnesses.has(field)) return;
    const version = field === 3 ? 1 : field === 6 ? 2 : 3;
    const languageId = version - 1;
    for (const item of set(witnesses.get(field)!, true)) {
      const bytes = payload(source, item);
      const script = {
        language: `plutus:v${version}` as Script['language'],
        hash: scriptHash(version, bytes),
        bytes: bytes.toString('hex'),
        span: item.span,
      };
      noDuplicate(scriptHashes, script.hash);
      plutusScripts.push(script);
    }
  });
  if (witnesses.has(4))
    for (const item of set(witnesses.get(4)!, true)) {
      plutusData(item, source);
      const datum = exact(source, item);
      noDuplicate(datumHashes, hash(Buffer.from(datum.value, 'hex')));
      datums.push(datum);
    }
  const targetLists: Record<RedeemerPurpose, readonly string[]> = {
    spend: [...normal]
      .sort(
        (left, right) =>
          left.transactionId.localeCompare(right.transactionId) ||
          Number(left.index - right.index)
      )
      .map((input) => `${input.transactionId}:${input.index}`),
    mint: [...new Set(mint.map((asset) => asset.policyId))].sort(),
    certificate: certificates.map((_, index) => `certificate:${index}`),
    withdrawal: withdrawals.map((withdrawal) => withdrawal.account).sort(),
    vote: [...voteEffects]
      .sort((left, right) => left.voter.localeCompare(right.voter))
      .map((vote) => `vote:${vote.voter}`),
    proposal: proposals.map((_, index) => `proposal:${index}`),
  };
  const redeemers = (witnesses.has(5)
    ? decodeRedeemers(source, witnesses.get(5)!)
    : []
  ).map((redeemer) => {
    const target = targetLists[redeemer.purpose][Number(redeemer.index)];
    if (!target) fail('unbound redeemer');
    return { ...redeemer, target };
  });
  if (!isNull(source, envelope.auxiliaryData))
    auxiliaryData(source, envelope.auxiliaryData);
  const auxiliaryDataHash = body.has(7)
    ? hex(source, body.get(7)!, 32)
    : undefined;
  if (auxiliaryDataHash) {
    if (
      isNull(source, envelope.auxiliaryData) ||
      hash(encoded(source, envelope.auxiliaryData)) !== auxiliaryDataHash
    )
      fail('auxiliary data commitment mismatch');
  } else if (!isNull(source, envelope.auxiliaryData))
    fail('uncommitted auxiliary data');
  const scriptDataHash = body.has(11)
    ? hex(source, body.get(11)!, 32)
    : undefined;
  const requirements: BindingRequirement[] = [];
  vkeys.forEach((witness, index) => {
    if (!context.verifiedWitnesses?.has(witness))
      requirements.push({
        kind: 'witness-proof',
        target: `vkey:${index}`,
        reason:
          'authenticated witness relevance and signature proof are required',
      });
  });
  bootstrap.forEach((witness, index) => {
    if (!context.verifiedWitnesses?.has(witness))
      requirements.push({
        kind: 'witness-proof',
        target: `bootstrap:${index}`,
        reason:
          'authenticated witness relevance and signature proof are required',
      });
  });
  const resolvedById = new Map(
    (context.resolvedInputs || []).map((resolved) => [
      `${resolved.outpoint.transactionId}:${resolved.outpoint.index}`,
      resolved,
    ])
  );
  const allInputs = [
    ...normal.map((outpoint) => ({ role: 'normal' as const, outpoint })),
    ...collateralInputs.map((outpoint) => ({
      role: 'collateral' as const,
      outpoint,
    })),
    ...referenceInputs.map((outpoint) => ({
      role: 'reference' as const,
      outpoint,
    })),
  ];
  const expectedByTarget = new Map<string, Set<Hex>>();
  const addExpected = (target: string, expected: Hex) => {
    const hashes = expectedByTarget.get(target) || new Set<Hex>();
    hashes.add(expected);
    expectedByTarget.set(target, hashes);
  };
  for (const policy of new Set(mint.map((item) => item.policyId)))
    addExpected(policy, policy);
  certificates.forEach((candidate, index) =>
    candidate.value.scriptCredentialHashes.forEach((expected) =>
      addExpected(`certificate:${index}`, expected)
    )
  );
  withdrawals.forEach((candidate) => {
    if (candidate.scriptHash)
      addExpected(candidate.account, candidate.scriptHash);
  });
  voteEffects.forEach((candidate) => {
    if (candidate.scriptHash)
      addExpected(`vote:${candidate.voter}`, candidate.scriptHash);
  });
  proposals.forEach((candidate, index) =>
    candidate.policyScriptHashes.forEach((expected) =>
      addExpected(`proposal:${index}`, expected)
    )
  );
  const expectedDatumHashes = new Set<Hex>();
  const referenceScripts: Script[] = [];
  allInputs.forEach(({ role, outpoint }) => {
    const id = `${outpoint.transactionId}:${outpoint.index}`;
    const resolved = resolvedById.get(id);
    if (!resolved) {
      requirements.push({
        kind: 'resolved-input',
        target: id,
        reason: 'missing authenticated resolved input',
      });
      return;
    }
    if (!resolved.value)
      requirements.push({
        kind: 'resolved-input',
        target: id,
        reason: 'exact resolved input value is required',
      });
    if (resolved.referenceScript) {
      const language =
        resolved.referenceScript.language === 'native'
          ? 0
          : Number(resolved.referenceScript.language.slice(-1));
      if (
        scriptHash(
          language,
          Buffer.from(resolved.referenceScript.bytes, 'hex')
        ) !== resolved.referenceScript.hash
      )
        fail('invalid resolved reference script hash');
      if (role === 'reference') referenceScripts.push(resolved.referenceScript);
    }
    if (role === 'collateral' && resolved.scriptHash)
      fail('collateral input cannot be script controlled');
    if (role !== 'normal' || !resolved.scriptHash) return;
    addExpected(id, resolved.scriptHash);
    if (!resolved.datum) return;
    const encodedDatum = resolved.datum.cbor;
    if (encodedDatum !== undefined) {
      if (!/^(?:[0-9a-f]{2})+$/.test(encodedDatum))
        fail('invalid resolved datum CBOR');
      const datumBytes = Buffer.from(encodedDatum, 'hex');
      let parsed: CborItem;
      try {
        parsed = parseCborItem(datumBytes);
      } catch (_error) {
        fail('invalid resolved datum CBOR');
      }
      if (parsed.span.end !== datumBytes.length)
        fail('invalid resolved datum CBOR');
      plutusData(parsed, datumBytes);
      if (
        resolved.datum.kind === 'hash' &&
        hash(datumBytes) !== resolved.datum.hash
      )
        fail('resolved datum hash mismatch');
    }
    if (resolved.datum.kind === 'hash')
      expectedDatumHashes.add(resolved.datum.hash);
  });
  let calculatedCollateralLoss: Value | undefined;
  if (
    !collateralInputs.length &&
    (collateralReturn || totalCollateral !== undefined)
  )
    fail('collateral accounting requires collateral inputs');
  if (collateralInputs.length) {
    const values = collateralInputs.map(
      (input) =>
        resolvedById.get(`${input.transactionId}:${input.index}`)?.value
    );
    if (
      values.every((candidate): candidate is Value => candidate !== undefined)
    ) {
      calculatedCollateralLoss = collateralLoss(
        values,
        collateralReturn?.value
      );
      if (
        totalCollateral !== undefined &&
        totalCollateral !== calculatedCollateralLoss.coin
      )
        fail('declared total collateral is inconsistent');
    }
  }
  const maximumLossRequirement =
    collateralInputs.length && calculatedCollateralLoss === undefined
      ? {
          kind: 'resolved-input' as const,
          target: 'collateral-input-values',
          reason:
            'authenticated collateral values are required to bound maximum loss',
        }
      : undefined;
  const usedLanguages = context.usedPlutusLanguages || [];
  if (scriptDataHash) {
    if (!context.languageViews || (redeemers.length && !usedLanguages.length))
      requirements.push({
        kind: 'language-view',
        target: 'script-data-hash',
        reason:
          'pinned language views and authenticated executing languages are required',
      });
    else {
      const material = Buffer.concat([
        witnesses.has(5)
          ? encoded(source, witnesses.get(5)!)
          : Buffer.from([0xa0]),
        ...(witnesses.has(4) ? [encoded(source, witnesses.get(4)!)] : []),
        languageMap(context.languageViews, usedLanguages),
      ]);
      if (hash(material) !== scriptDataHash)
        fail('script data commitment mismatch');
    }
  } else if (redeemers.length || datums.length)
    fail('script data hash missing');
  const witnessScripts = [...nativeScripts, ...plutusScripts];
  const materialByHash = new Map(
    [...witnessScripts, ...referenceScripts].map((script) => [
      script.hash,
      script,
    ])
  );
  const validTargets = new Set<string>();
  Object.keys(targetLists).forEach((purpose) =>
    targetLists[purpose as RedeemerPurpose].forEach((target) =>
      validTargets.add(target)
    )
  );
  context.redeemerScriptHashes?.forEach((expected, target) => {
    if (!validTargets.has(target))
      fail('unknown authenticated redeemer target');
    const bodyExpected = expectedByTarget.get(target);
    if (bodyExpected?.size && !bodyExpected.has(expected))
      fail('redeemer script binding mismatch');
    addExpected(target, expected);
  });
  redeemers.forEach((redeemer) => {
    const target = redeemer.target!;
    const expected = expectedByTarget.get(target) || new Set<Hex>();
    const authenticated = context.redeemerScriptHashes?.get(target);
    if (authenticated) {
      if (expected.size && !expected.has(authenticated))
        fail('redeemer script binding mismatch');
      addExpected(target, authenticated);
    } else if (expected.size !== 1) {
      requirements.push({
        kind: 'script',
        target,
        reason: 'authenticated redeemer target script binding is required',
      });
    }
  });
  for (const [target, hashes] of expectedByTarget) {
    for (const expected of hashes) {
      const material = materialByHash.get(expected);
      if (!material) {
        requirements.push({
          kind: 'script',
          target: expected,
          reason: 'required script material is unavailable',
        });
        continue;
      }
      const redeemer = redeemers.find(
        (candidate) => candidate.target === target
      );
      if (material.language === 'native') {
        if (redeemer && context.redeemerScriptHashes?.get(target) === expected)
          fail('native script cannot bind a redeemer');
      } else if (!redeemer) {
        requirements.push({
          kind: 'script',
          target,
          reason: 'Plutus script target requires a redeemer',
        });
      }
    }
  }
  const expectedScriptHashes = new Set(
    [...expectedByTarget.values()].reduce<Hex[]>((all, hashes) => {
      hashes.forEach((candidate) => all.push(candidate));
      return all;
    }, [])
  );
  witnessScripts.forEach((script) => {
    if (!expectedScriptHashes.has(script.hash)) {
      if (!context.resolvedInputs && normal.length)
        requirements.push({
          kind: 'script',
          target: script.hash,
          reason: 'authenticated input script binding is required',
        });
      else fail('unbound supplied script');
    }
  });
  const witnessDatumHashes = new Set(
    datums.map((datum) => hash(Buffer.from(datum.value, 'hex')))
  );
  datums.forEach((datum) => {
    const datumHash = hash(Buffer.from(datum.value, 'hex'));
    if (!expectedDatumHashes.has(datumHash)) {
      if (!context.resolvedInputs && normal.length)
        requirements.push({
          kind: 'datum',
          target: datumHash,
          reason: 'authenticated input datum binding is required',
        });
      else fail('unbound supplied datum');
    }
  });
  expectedDatumHashes.forEach((expected) => {
    if (!witnessDatumHashes.has(expected))
      requirements.push({
        kind: 'datum',
        target: expected,
        reason: 'required datum material is unavailable',
      });
  });
  const review: ReviewCompleteness = requirements.length
    ? { complete: false, signable: false, requirements }
    : { complete: true, signable: true, requirements: [] };
  const decodedAuxiliaryData = isNull(source, envelope.auxiliaryData)
    ? undefined
    : exact(source, envelope.auxiliaryData);
  const effects: SemanticTransaction['effects'] = [
    ...normal.map((outpoint) => ({
      kind: 'input',
      value: {
        outpoint,
        resolved: resolvedById.get(
          `${outpoint.transactionId}:${outpoint.index}`
        )?.value,
      },
      span: outpoint.span,
    })),
    ...collateralInputs.map((outpoint) => ({
      kind: 'collateral-input',
      value: {
        outpoint,
        resolved: resolvedById.get(
          `${outpoint.transactionId}:${outpoint.index}`
        )?.value,
      },
      span: outpoint.span,
    })),
    ...referenceInputs.map((outpoint) => ({
      kind: 'reference-input',
      value: {
        outpoint,
        resolved: resolvedById.get(
          `${outpoint.transactionId}:${outpoint.index}`
        )?.value,
      },
      span: outpoint.span,
    })),
    ...outputs.map((value) => ({
      kind: 'output',
      value,
      span: value.exactSpan,
    })),
    { kind: 'fee', value: fee },
    ...mint.map((value) => ({
      kind: value.quantity < BigInt(0) ? 'burn' : 'mint',
      value,
    })),
    ...withdrawals.map((value) => ({
      kind: 'withdrawal',
      value,
      span: value.span,
    })),
    ...certificates.map((value) => ({
      kind: 'certificate',
      value,
      span: value.span,
    })),
    ...redeemers.map((value) => ({
      kind: 'redeemer',
      value,
      span: value.exactSpan,
    })),
    ...nativeScripts.map((value) => ({
      kind: 'native-script',
      value,
      span: value.span,
    })),
    ...plutusScripts.map((value) => ({
      kind: 'plutus-script',
      value,
      span: value.span,
    })),
    ...datums.map((value) => ({ kind: 'datum', value, span: value.span })),
    ...voteEffects.map((value) => ({ kind: 'vote', value, span: value.span })),
    ...proposals.map((value) => ({
      kind: 'proposal',
      value,
      span: value.span,
    })),
    ...(collateralReturn
      ? [
          {
            kind: 'collateral-return',
            value: collateralReturn,
            span: collateralReturn.exactSpan,
          },
        ]
      : []),
    ...(requiredSigners.length
      ? [{ kind: 'required-signers', value: requiredSigners }]
      : []),
    ...(networkId === undefined
      ? []
      : [{ kind: 'network-id', value: networkId }]),
    {
      kind: 'validity-interval',
      value: {
        invalidBefore: body.has(8) ? uint(body.get(8)!) : undefined,
        invalidHereafter: body.has(3) ? uint(body.get(3)!) : undefined,
      },
    },
    { kind: 'transaction-validity', value: envelope.isValid },
    ...(decodedAuxiliaryData
      ? [
          {
            kind: 'auxiliary-data',
            value: decodedAuxiliaryData,
            span: decodedAuxiliaryData.span,
          },
        ]
      : []),
    ...(auxiliaryDataHash
      ? [{ kind: 'auxiliary-data-hash', value: auxiliaryDataHash }]
      : []),
    ...(scriptDataHash
      ? [{ kind: 'script-data-hash', value: scriptDataHash }]
      : []),
    ...(treasuryValue === undefined
      ? []
      : [{ kind: 'treasury-value', value: treasuryValue }]),
    ...(donation === undefined ? [] : [{ kind: 'donation', value: donation }]),
    ...(calculatedCollateralLoss === undefined
      ? maximumLossRequirement
        ? [
            {
              kind: 'maximum-collateral-loss-unresolved',
              value: maximumLossRequirement,
            },
          ]
        : []
      : [{ kind: 'maximum-collateral-loss', value: calculatedCollateralLoss }]),
  ];
  return {
    envelope,
    transactionId: envelope.transactionId,
    inputs: {
      normal,
      collateral: collateralInputs,
      reference: referenceInputs,
    },
    outputs,
    fee,
    validityInterval: {
      invalidBefore: body.has(8) ? uint(body.get(8)!) : undefined,
      invalidHereafter: body.has(3) ? uint(body.get(3)!) : undefined,
    },
    mint,
    certificates,
    withdrawals,
    requiredSigners,
    networkId,
    collateral: {
      return: collateralReturn,
      total: totalCollateral,
      maximumLoss: calculatedCollateralLoss,
      maximumLossRequirement,
    },
    governance: { votes: voteEffects, proposals, treasuryValue, donation },
    auxiliaryData: decodedAuxiliaryData,
    witnesses: {
      vkeys,
      bootstrap,
      nativeScripts,
      plutusScripts,
      datums,
      redeemers,
    },
    commitments: { auxiliaryDataHash, scriptDataHash },
    effects,
    review,
  };
};
