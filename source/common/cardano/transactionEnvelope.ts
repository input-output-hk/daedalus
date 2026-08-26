import { blake2b } from 'blakejs';

import {
  bytesForSpan,
  CborItem,
  CborParseError,
  CborSpan,
  parseCborItem,
} from './cborSlices';

export class TransactionEnvelopeError extends Error {
  public constructor(message = 'invalid Conway transaction envelope') {
    super(message);
    this.name = 'TransactionEnvelopeError';
  }
}

export type ExactTransactionEnvelope = Readonly<{
  cbor: Buffer;
  root: CborItem;
  body: CborItem;
  witnessSet: CborItem;
  isValid: boolean;
  auxiliaryData: CborItem;
  outputs: readonly CborItem[];
  collateralReturn?: CborItem;
  spans: Readonly<{
    body: CborSpan;
    witnessSet: CborSpan;
    isValid: CborSpan;
    auxiliaryData: CborSpan;
    outputs: readonly CborSpan[];
    collateralReturn?: CborSpan;
  }>;
  transactionId: string;
}>;

const BODY_KEYS = new Set([
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
const WITNESS_KEYS = new Set([0, 1, 2, 3, 4, 5, 6, 7]);

const invalid = (): never => {
  throw new TransactionEnvelopeError();
};
const items = (node: CborItem): readonly CborItem[] => node.items || invalid();
const entries = (
  node: CborItem
): readonly Readonly<{ key: CborItem; value: CborItem }>[] =>
  node.entries || invalid();
const array = (node: CborItem): readonly CborItem[] =>
  node.major === 4 ? items(node) : invalid();
const map = (
  node: CborItem
): readonly Readonly<{ key: CborItem; value: CborItem }>[] =>
  node.major === 5 ? entries(node) : invalid();
const unsigned = (node: CborItem): bigint =>
  node.major === 0 && node.value !== undefined ? node.value : invalid();
const bytes = (node: CborItem): void => {
  if (node.major !== 2) invalid();
};
const simple = (source: Buffer, node: CborItem, value: number): void => {
  if (
    node.major !== 7 ||
    node.span.end !== node.span.start + 1 ||
    source[node.span.start] !== value
  )
    invalid();
};

const mapValue = (node: CborItem, key: number): CborItem | undefined =>
  entries(node).find(
    (entry) => entry.key.major === 0 && entry.key.value === BigInt(key)
  )?.value;

const noTags = (node: CborItem): void => {
  if (node.major === 6) invalid();
  (node.items || []).forEach(noTags);
  (node.entries || []).forEach(({ key, value }) => {
    noTags(key);
    noTags(value);
  });
};

const validatePlutusData = (node: CborItem): void => {
  if (node.major === 0 || node.major === 1 || node.major === 2) return;
  if (node.major === 4) {
    array(node).forEach(validatePlutusData);
    return;
  }
  if (node.major === 5) {
    map(node).forEach(({ key, value }) => {
      validatePlutusData(key);
      validatePlutusData(value);
    });
    return;
  }
  if (node.major !== 6 || node.tag === undefined) invalid();
  const child = items(node)[0];
  if ((node.tag === BigInt(2) || node.tag === BigInt(3)) && child.major === 2)
    return;
  if (node.tag >= BigInt(121) && node.tag <= BigInt(127) && child.major === 4) {
    array(child).forEach(validatePlutusData);
    return;
  }
  if (node.tag === BigInt(102) && child.major === 4) {
    const parts = array(child);
    if (parts.length !== 2) invalid();
    unsigned(parts[0]);
    array(parts[1]).forEach(validatePlutusData);
    return;
  }
  invalid();
};

const validateEmbedded = (
  source: Buffer,
  wrapper: CborItem,
  plutus: boolean
): void => {
  if (wrapper.major !== 6 || wrapper.tag !== BigInt(24)) invalid();
  const payload = items(wrapper)[0];
  if (payload.major !== 2 || payload.content === undefined) invalid();
  const embedded = bytesForSpan(source, payload.content);
  const item = parseCborItem(embedded);
  if (item.span.end !== embedded.length) invalid();
  if (plutus) validatePlutusData(item);
  else noTags(item);
};

const validateInput = (node: CborItem): void => {
  const parts = array(node);
  if (parts.length !== 2) invalid();
  bytes(parts[0]);
  unsigned(parts[1]);
  noTags(node);
};

const validateSet = (
  node: CborItem,
  nonempty: boolean,
  validateMember: typeof noTags
): void => {
  let content = node;
  if (node.major === 6) {
    if (node.tag !== BigInt(258) || items(node).length !== 1) invalid();
    [content] = items(node);
  }
  const members = array(content);
  if (nonempty && members.length === 0) invalid();
  members.forEach(validateMember);
};

const validateOutput = (source: Buffer, output: CborItem): void => {
  if (output.major === 4) {
    const parts = array(output);
    if (parts.length !== 2 && parts.length !== 3) invalid();
    bytes(parts[0]);
    if (parts[1].major !== 0 && parts[1].major !== 4) invalid();
    if (parts[2] !== undefined) bytes(parts[2]);
    noTags(output);
    return;
  }
  if (output.major !== 5) invalid();
  const address = mapValue(output, 0);
  const value = mapValue(output, 1);
  if (!address || !value || (value.major !== 0 && value.major !== 4)) invalid();
  for (const { key } of map(output)) {
    const valueKey = unsigned(key);
    if (valueKey > BigInt(3)) invalid();
  }
  bytes(address);
  const datum = mapValue(output, 2);
  if (datum !== undefined) {
    const parts = array(datum);
    if (parts.length !== 2 || unsigned(parts[0]) > BigInt(1)) invalid();
    if (parts[0].value === BigInt(0)) bytes(parts[1]);
    else validateEmbedded(source, parts[1], true);
  }
  const reference = mapValue(output, 3);
  if (reference !== undefined) validateEmbedded(source, reference, false);
  for (const { key, value: field } of map(output)) {
    if (key.value !== BigInt(2) && key.value !== BigInt(3)) noTags(field);
  }
};

const validateRedeemers = (node: CborItem): void => {
  const executionUnits = (candidate: CborItem): void => {
    const parts = array(candidate);
    if (parts.length !== 2) invalid();
    parts.forEach(unsigned);
  };
  if (node.major === 4) {
    const redeemers = array(node);
    if (redeemers.length === 0) invalid();
    redeemers.forEach((redeemer) => {
      const parts = array(redeemer);
      if (parts.length !== 4 || unsigned(parts[0]) > BigInt(5)) invalid();
      unsigned(parts[1]);
      validatePlutusData(parts[2]);
      executionUnits(parts[3]);
    });
    return;
  }
  if (node.major === 5) {
    const redeemers = map(node);
    if (redeemers.length === 0) invalid();
    redeemers.forEach(({ key, value }) => {
      const index = array(key);
      if (index.length !== 2 || unsigned(index[0]) > BigInt(5)) invalid();
      unsigned(index[1]);
      const parts = array(value);
      if (parts.length !== 2) invalid();
      validatePlutusData(parts[0]);
      executionUnits(parts[1]);
    });
    return;
  }
  invalid();
};

const validateWitnessSet = (witnesses: CborItem): void => {
  for (const { key, value } of map(witnesses)) {
    const kind = unsigned(key);
    if (kind > BigInt(7) || !WITNESS_KEYS.has(Number(kind))) invalid();
    if (kind === BigInt(5)) validateRedeemers(value);
    else if (kind === BigInt(4)) validateSet(value, true, validatePlutusData);
    else if (kind === BigInt(3) || kind === BigInt(6) || kind === BigInt(7))
      validateSet(value, true, (member) => {
        bytes(member);
        noTags(member);
      });
    else validateSet(value, true, noTags);
  }
};

const validateAuxiliaryData = (node: CborItem): void => {
  if (node.major === 5) {
    noTags(node);
    return;
  }
  if (node.major === 4) {
    const parts = array(node);
    if (parts.length !== 2 || parts[0].major !== 5 || parts[1].major !== 4)
      invalid();
    noTags(node);
    return;
  }
  if (
    node.major === 6 &&
    node.tag === BigInt(259) &&
    items(node).length === 1 &&
    items(node)[0].major === 5
  ) {
    noTags(items(node)[0]);
    return;
  }
  invalid();
};

const validateBody = (
  source: Buffer,
  body: CborItem
): { outputs: readonly CborItem[]; collateralReturn?: CborItem } => {
  const found = new Set<number>();
  for (const { key, value } of map(body)) {
    const field = unsigned(key);
    const number = Number(field);
    if (!BODY_KEYS.has(number) || found.has(number)) invalid();
    found.add(number);
    if (number === 0) validateSet(value, false, validateInput);
    else if (number === 1)
      array(value).forEach((output) => validateOutput(source, output));
    else if (
      number === 2 ||
      number === 3 ||
      number === 8 ||
      number === 17 ||
      number === 21
    )
      unsigned(value);
    else if (number === 7 || number === 11) bytes(value);
    else if (number === 4 || number === 20) validateSet(value, true, noTags);
    else if (number === 13 || number === 18)
      validateSet(value, true, validateInput);
    else if (number === 14)
      validateSet(value, true, (member) => {
        bytes(member);
        noTags(member);
      });
    else if (number === 15) {
      const network = unsigned(value);
      if (network > BigInt(1)) invalid();
    } else if (number === 16) validateOutput(source, value);
    else noTags(value);
  }
  if (!found.has(0) || !found.has(1) || !found.has(2)) invalid();
  const outputList = mapValue(body, 1);
  if (outputList === undefined) invalid();
  return { outputs: array(outputList), collateralReturn: mapValue(body, 16) };
};

export const parseConwayTransactionEnvelope = (
  cbor: Buffer
): ExactTransactionEnvelope => {
  try {
    if (!Buffer.isBuffer(cbor) || cbor.length === 0 || cbor.length > 65_536)
      invalid();
    const root = parseCborItem(cbor);
    if (root.span.end !== cbor.length || root.major !== 4) invalid();
    const outer = array(root);
    if (outer.length !== 4) invalid();
    const [body, witnessSet, isValid, auxiliaryData] = outer;
    map(body);
    map(witnessSet);
    simple(cbor, isValid, cbor[isValid.span.start] === 0xf4 ? 0xf4 : 0xf5);
    if (cbor[isValid.span.start] !== 0xf4 && cbor[isValid.span.start] !== 0xf5)
      invalid();
    const extracted = validateBody(cbor, body);
    validateWitnessSet(witnessSet);
    if (cbor[auxiliaryData.span.start] !== 0xf6)
      validateAuxiliaryData(auxiliaryData);
    const bodyBytes = bytesForSpan(cbor, body.span);
    return {
      cbor,
      root,
      body,
      witnessSet,
      isValid: cbor[isValid.span.start] === 0xf5,
      auxiliaryData,
      outputs: extracted.outputs,
      collateralReturn: extracted.collateralReturn,
      spans: {
        body: body.span,
        witnessSet: witnessSet.span,
        isValid: isValid.span,
        auxiliaryData: auxiliaryData.span,
        outputs: extracted.outputs.map((output) => output.span),
        collateralReturn: extracted.collateralReturn?.span,
      },
      transactionId: Buffer.from(blake2b(bodyBytes, undefined, 32)).toString(
        'hex'
      ),
    };
  } catch (error) {
    if (error instanceof TransactionEnvelopeError) throw error;
    if (error instanceof CborParseError) throw new TransactionEnvelopeError();
    throw new TransactionEnvelopeError();
  }
};
