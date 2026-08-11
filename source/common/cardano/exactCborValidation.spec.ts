import fs from 'fs';
import path from 'path';
import { createHash } from 'crypto';
import blake2b from 'blake2b';
import { blake2b as blake2bJs } from 'blakejs';

type CborNode = {
  start: number;
  end: number;
  major: number;
  additionalInformation: number;
  value?: bigint;
  tag?: bigint;
  contentStart?: number;
  contentEnd?: number;
  children?: CborNode[];
  entries?: Array<{ key: CborNode; value: CborNode }>;
};

type Fixture = {
  id: string;
  cborHex: string;
  provenance: { fixtureSha256: string };
};

type ExpectedSpans = ReturnType<typeof extractSpans>;

type Manifest = {
  inventory: {
    conway: {
      bodyKeys: number[];
      reservedBodyKeys: number[];
      witnessKeys: number[];
      certificateTags: number[];
      redeemerTags: number[];
      governanceActionTags: number[];
    };
    dijkstraDeltas: {
      changedBodyKeys: number[];
      addedBodyKeys: number[];
      addedProtocolParameterKeys: number[];
    };
  };
  fixtures: Array<{ id: string; file: string; expected: ExpectedSpans }>;
  sdkCases: Array<{
    id: string;
    fixtureId: string;
    mutation: string;
    strictExpected: 'accept' | 'reject';
  }>;
  sdkResults: Array<{
    version: string;
    file: string;
    strictRejectsAcceptedBySdk: number;
  }>;
  policyCases: Array<{
    id: string;
    cborHex: string;
    validator: string;
    expected: 'accept' | 'reject' | 'defer-semantic-uniqueness';
  }>;
  sourceInventoryFile: string;
  spanAnnotationsFile: string;
  wirePolicy: Array<{ family: string }>;
  protocolContextRules: Array<{ family: string; owners: string[] }>;
};

const fixtureDirectory = path.join(__dirname, 'fixtures', 'exact-cbor');

const readFixture = (name: string): Fixture =>
  JSON.parse(
    fs.readFileSync(path.join(fixtureDirectory, name), 'utf8')
  ) as Fixture;

const manifest = JSON.parse(
  fs.readFileSync(path.join(fixtureDirectory, 'manifest.json'), 'utf8')
) as Manifest;

const readArgument = (
  bytes: Buffer,
  offset: number,
  additionalInformation: number
): { next: number; value?: bigint; indefinite: boolean } => {
  if (additionalInformation < 24) {
    return {
      next: offset,
      value: BigInt(additionalInformation),
      indefinite: false,
    };
  }

  const widths: Record<number, number> = { 24: 1, 25: 2, 26: 4, 27: 8 };
  const width = widths[additionalInformation];
  if (width) {
    if (offset + width > bytes.length) throw new Error('truncated argument');
    let value = BigInt(0);
    for (let index = 0; index < width; index += 1) {
      value = (value << BigInt(8)) | BigInt(bytes[offset + index]);
    }
    return { next: offset + width, value, indefinite: false };
  }

  if (additionalInformation === 31) {
    return { next: offset, indefinite: true };
  }
  throw new Error('reserved additional information');
};

const parseItem = (bytes: Buffer, start = 0, depth = 0): CborNode => {
  if (depth > 128) throw new Error('maximum nesting exceeded');
  if (start >= bytes.length) throw new Error('truncated item');

  const initial = bytes[start];
  if (initial === 0xff) throw new Error('unexpected break');
  const major = initial >> 5;
  const argument = readArgument(bytes, start + 1, initial & 0x1f);
  let cursor = argument.next;
  const node: CborNode = {
    start,
    end: cursor,
    major,
    additionalInformation: initial & 0x1f,
  };

  if (major === 0 || major === 1) {
    if (argument.indefinite || argument.value === undefined) {
      throw new Error('indefinite integer');
    }
    node.value =
      major === 0 ? argument.value : BigInt(-1) - (argument.value as bigint);
  } else if (major === 2 || major === 3) {
    if (argument.indefinite) {
      const children: CborNode[] = [];
      while (bytes[cursor] !== 0xff) {
        const child = parseItem(bytes, cursor, depth + 1);
        if (child.major !== major || child.children) {
          throw new Error('invalid indefinite chunk');
        }
        children.push(child);
        cursor = child.end;
        if (cursor >= bytes.length) throw new Error('unterminated string');
      }
      cursor += 1;
      node.children = children;
    } else {
      const length = Number(argument.value);
      if (!Number.isSafeInteger(length) || cursor + length > bytes.length) {
        throw new Error('invalid string length');
      }
      node.contentStart = cursor;
      cursor += length;
      node.contentEnd = cursor;
    }
  } else if (major === 4) {
    const children: CborNode[] = [];
    if (argument.indefinite) {
      while (bytes[cursor] !== 0xff) {
        const child = parseItem(bytes, cursor, depth + 1);
        children.push(child);
        cursor = child.end;
        if (cursor >= bytes.length) throw new Error('unterminated array');
      }
      cursor += 1;
    } else {
      const length = Number(argument.value);
      if (!Number.isSafeInteger(length))
        throw new Error('invalid array length');
      for (let index = 0; index < length; index += 1) {
        const child = parseItem(bytes, cursor, depth + 1);
        children.push(child);
        cursor = child.end;
      }
    }
    node.children = children;
  } else if (major === 5) {
    const entries: Array<{ key: CborNode; value: CborNode }> = [];
    const encodedKeys = new Set<string>();
    const decodedScalarKeys = new Set<string>();
    const parseEntry = () => {
      const key = parseItem(bytes, cursor, depth + 1);
      cursor = key.end;
      const value = parseItem(bytes, cursor, depth + 1);
      cursor = value.end;
      const encodedKey = bytes.subarray(key.start, key.end).toString('hex');
      if (encodedKeys.has(encodedKey)) throw new Error('duplicate encoded key');
      encodedKeys.add(encodedKey);
      if (key.value !== undefined) {
        const scalar = `${key.major}:${key.value.toString()}`;
        if (decodedScalarKeys.has(scalar))
          throw new Error('duplicate scalar key');
        decodedScalarKeys.add(scalar);
      }
      entries.push({ key, value });
    };
    if (argument.indefinite) {
      while (bytes[cursor] !== 0xff) {
        parseEntry();
        if (cursor >= bytes.length) throw new Error('unterminated map');
      }
      cursor += 1;
    } else {
      const length = Number(argument.value);
      if (!Number.isSafeInteger(length)) throw new Error('invalid map length');
      for (let index = 0; index < length; index += 1) parseEntry();
    }
    node.entries = entries;
  } else if (major === 6) {
    if (argument.indefinite || argument.value === undefined) {
      throw new Error('invalid tag');
    }
    node.tag = argument.value;
    const child = parseItem(bytes, cursor, depth + 1);
    node.children = [child];
    cursor = child.end;
  } else if (major === 7) {
    if (argument.indefinite) throw new Error('unexpected break');
  }

  node.end = cursor;
  return node;
};

const mapValue = (map: CborNode, key: number): CborNode | undefined =>
  map.entries?.find(
    (entry) => entry.key.major === 0 && entry.key.value === BigInt(key)
  )?.value;

const extractSpans = (fixture: Fixture) => {
  const bytes = Buffer.from(fixture.cborHex, 'hex');
  const root = parseItem(bytes);
  if (root.end !== bytes.length) throw new Error('trailing bytes');
  if (root.major !== 4 || root.children?.length !== 4) {
    throw new Error('invalid Conway envelope');
  }
  const [body, witnessSet, isValid, auxiliaryData] = root.children;
  if (body.major !== 5 || witnessSet.major !== 5) {
    throw new Error('invalid body or witness set');
  }
  const outputList = mapValue(body, 1);
  const collateralReturn = mapValue(body, 16);
  const outputSpans = outputList?.children?.map(({ start, end }) => ({
    start,
    end,
  }));
  const bodyBytes = bytes.subarray(body.start, body.end);
  const bodyHashBlakejs = Buffer.from(
    blake2bJs(bodyBytes, undefined, 32)
  ).toString('hex');
  const bodyHashBlake2b = blake2b(32).update(bodyBytes).digest('hex') as string;
  if (bodyHashBlakejs !== bodyHashBlake2b) {
    throw new Error('independent Blake2b-256 mismatch');
  }
  return {
    byteLength: bytes.length,
    body: { start: body.start, end: body.end },
    witnessSet: { start: witnessSet.start, end: witnessSet.end },
    isValid: { start: isValid.start, end: isValid.end },
    auxiliaryData: { start: auxiliaryData.start, end: auxiliaryData.end },
    outputs: outputSpans || [],
    collateralReturn: collateralReturn
      ? { start: collateralReturn.start, end: collateralReturn.end }
      : null,
    bodyHashBlake2b256: bodyHashBlakejs,
  };
};

const replaceRanges = (
  bytes: Buffer,
  replacements: Array<{ start: number; end: number; bytes: Buffer }>
): Buffer =>
  replacements
    .sort((left, right) => right.start - left.start)
    .reduce(
      (result, replacement) =>
        Buffer.concat([
          result.subarray(0, replacement.start),
          replacement.bytes,
          result.subarray(replacement.end),
        ]),
      bytes
    );

const deriveBabbageMapOutputs = (fixture: Fixture): Buffer => {
  const bytes = Buffer.from(fixture.cborHex, 'hex');
  const root = parseItem(bytes);
  const body = root.children?.[0];
  if (!body) throw new Error('missing body');
  const outputList = mapValue(body, 1);
  const collateralReturn = mapValue(body, 16);
  const outputs = [...(outputList?.children || [])];
  if (collateralReturn) outputs.push(collateralReturn);
  return replaceRanges(
    bytes,
    outputs.map((output) => {
      if (output.major !== 4 || output.children?.length !== 2) {
        throw new Error('expected two-field Alonzo output');
      }
      const [address, value] = output.children;
      return {
        start: output.start,
        end: output.end,
        bytes: Buffer.concat([
          Buffer.from([0xa2, 0x00]),
          bytes.subarray(address.start, address.end),
          Buffer.from([0x01]),
          bytes.subarray(value.start, value.end),
        ]),
      };
    })
  );
};

const deriveUntaggedBodySets = (fixture: Fixture): Buffer => {
  const bytes = Buffer.from(fixture.cborHex, 'hex');
  const root = parseItem(bytes);
  const body = root.children?.[0];
  if (!body) throw new Error('missing body');
  return replaceRanges(
    bytes,
    [0, 13].map((key) => {
      const set = mapValue(body, key);
      const array = set?.children?.[0];
      if (set?.major !== 6 || set.tag !== BigInt(258) || !array) {
        throw new Error('expected tagged body set');
      }
      return {
        start: set.start,
        end: set.end,
        bytes: bytes.subarray(array.start, array.end),
      };
    })
  );
};

const fixtureById = (id: string): Fixture => {
  const entry = manifest.fixtures.find((fixture) => fixture.id === id);
  if (!entry) throw new Error(`unknown fixture ${id}`);
  return readFixture(entry.file);
};

const mutate = (fixture: Fixture, mutation: string): Buffer => {
  const bytes = Buffer.from(fixture.cborHex, 'hex');
  const expected = manifest.fixtures.find(({ id }) => id === fixture.id)
    ?.expected;
  if (!expected) throw new Error(`missing expected spans for ${fixture.id}`);
  if (mutation === 'none') return bytes;
  if (mutation === 'append-zero')
    return Buffer.concat([bytes, Buffer.from([0])]);
  if (mutation === 'indefinite-root') {
    return Buffer.concat([
      Buffer.from([0x9f]),
      bytes.subarray(1),
      Buffer.from([0xff]),
    ]);
  }
  if (
    mutation === 'insert-duplicate-fee' ||
    mutation === 'insert-unknown-body-key'
  ) {
    const mapHeader = bytes[expected.body.start];
    if (mapHeader < 0xa0 || mapHeader >= 0xb7)
      throw new Error('unsupported body map header');
    const pair =
      mutation === 'insert-duplicate-fee'
        ? Buffer.from('180200', 'hex')
        : Buffer.from('0c00', 'hex');
    return Buffer.concat([
      bytes.subarray(0, expected.body.start),
      Buffer.from([mapHeader + 1]),
      bytes.subarray(expected.body.start + 1, expected.body.end),
      pair,
      bytes.subarray(expected.body.end),
    ]);
  }
  if (mutation === 'replace-first-set-tag') {
    const tag = Buffer.from('d90102', 'hex');
    const index = bytes.indexOf(tag);
    if (index < 0) throw new Error('set tag not found');
    const result = Buffer.from(bytes);
    result[index + 2] = 0x03;
    return result;
  }
  throw new Error(`unknown mutation ${mutation}`);
};

const unwrapSet = (node: CborNode): CborNode | undefined => {
  if (node.major === 4) return node;
  if (node.major === 6 && node.tag === BigInt(258)) {
    return node.children?.[0];
  }
  return undefined;
};

const validateEmbeddedCbor = (bytes: Buffer, wrapper: CborNode) => {
  const payload = wrapper.children?.[0];
  if (
    wrapper.major !== 6 ||
    wrapper.tag !== BigInt(24) ||
    payload?.major !== 2 ||
    payload.contentStart === undefined ||
    payload.contentEnd === undefined
  ) {
    throw new Error('invalid embedded CBOR wrapper');
  }
  const embedded = bytes.subarray(payload.contentStart, payload.contentEnd);
  if (parseItem(embedded).end !== embedded.length) {
    throw new Error('embedded trailing bytes');
  }
};

const validateTransactionOutput = (bytes: Buffer, output: CborNode) => {
  let address: CborNode | undefined;
  let value: CborNode | undefined;
  let datum: CborNode | undefined;
  let scriptReference: CborNode | undefined;
  if (output.major === 4 && [2, 3].includes(output.children?.length || 0)) {
    [address, value, datum] = output.children || [];
  } else if (output.major === 5) {
    address = mapValue(output, 0);
    value = mapValue(output, 1);
    datum = mapValue(output, 2);
    scriptReference = mapValue(output, 3);
    if (
      !address ||
      !value ||
      output.entries?.some(
        ({ key }) =>
          key.major !== 0 ||
          key.value === undefined ||
          ![0, 1, 2, 3].includes(Number(key.value))
      )
    ) {
      throw new Error('invalid Babbage output map');
    }
  } else {
    throw new Error('invalid transaction output');
  }
  if (address?.major !== 2 || !value || ![0, 4].includes(value.major)) {
    throw new Error('invalid output address or value');
  }
  if (datum) {
    if (output.major === 4 && datum.major !== 2) {
      throw new Error('invalid Alonzo datum hash');
    }
    if (
      output.major === 5 &&
      (datum.major !== 4 ||
        datum.children?.length !== 2 ||
        datum.children[0].major !== 0 ||
        ![BigInt(0), BigInt(1)].includes(datum.children[0].value as bigint) ||
        (datum.children[0].value === BigInt(0) &&
          datum.children[1].major !== 2) ||
        (datum.children[0].value === BigInt(1) &&
          (datum.children[1].major !== 6 ||
            datum.children[1].tag !== BigInt(24) ||
            datum.children[1].children?.[0].major !== 2)))
    ) {
      throw new Error('invalid Babbage datum option');
    }
    if (output.major === 5 && datum.children?.[0].value === BigInt(1)) {
      validateEmbeddedCbor(bytes, datum.children[1]);
    }
  }
  if (
    scriptReference &&
    (scriptReference.major !== 6 ||
      scriptReference.tag !== BigInt(24) ||
      scriptReference.children?.[0].major !== 2)
  ) {
    throw new Error('invalid script reference');
  }
  if (scriptReference) validateEmbeddedCbor(bytes, scriptReference);
};

const validateRedeemers = (redeemers: CborNode) => {
  const validExecutionUnits = (node: CborNode) =>
    node.major === 4 &&
    node.children?.length === 2 &&
    node.children.every((item) => item.major === 0);
  const validFlatRedeemer = (node: CborNode) =>
    node.major === 4 &&
    node.children?.length === 4 &&
    node.children[0].major === 0 &&
    node.children[1].major === 0 &&
    validExecutionUnits(node.children[3]);
  if (
    redeemers.major === 4 &&
    (redeemers.children?.length || 0) > 0 &&
    redeemers.children?.every(validFlatRedeemer)
  ) {
    return;
  }
  if (
    redeemers.major === 5 &&
    (redeemers.entries?.length || 0) > 0 &&
    redeemers.entries?.every(
      ({ key, value }) =>
        key.major === 4 &&
        key.children?.length === 2 &&
        key.children.every((item) => item.major === 0) &&
        value.major === 4 &&
        value.children?.length === 2 &&
        validExecutionUnits(value.children[1])
    )
  ) {
    return;
  }
  throw new Error('invalid redeemers');
};

const validateAuxiliaryData = (auxiliaryData: CborNode) => {
  if (auxiliaryData.major === 5) return;
  if (
    auxiliaryData.major === 4 &&
    auxiliaryData.children?.length === 2 &&
    auxiliaryData.children[0].major === 5 &&
    auxiliaryData.children[1].major === 4
  ) {
    return;
  }
  if (
    auxiliaryData.major === 6 &&
    auxiliaryData.tag === BigInt(259) &&
    auxiliaryData.children?.[0].major === 5
  ) {
    return;
  }
  throw new Error('invalid auxiliary data');
};

const validateStrictEnvelope = (bytes: Buffer) => {
  const root = parseItem(bytes);
  if (root.end !== bytes.length) throw new Error('trailing bytes');
  if (root.major !== 4 || root.children?.length !== 4) {
    throw new Error('invalid Conway envelope');
  }
  const [body, witnessSet, isValid, auxiliaryData] = root.children;
  if (body.major !== 5 || witnessSet.major !== 5) {
    throw new Error('invalid transaction body or witness set');
  }
  if (isValid.major !== 7 || ![0xf4, 0xf5].includes(bytes[isValid.start])) {
    throw new Error('invalid isValid value');
  }
  if (bytes[auxiliaryData.start] !== 0xf6) validateAuxiliaryData(auxiliaryData);
  const allowedBodyKeys = new Set(manifest.inventory.conway.bodyKeys);
  for (const entry of body.entries || []) {
    if (entry.key.major !== 0 || entry.key.value === undefined) {
      throw new Error('invalid body key');
    }
    const key = Number(entry.key.value);
    if (!allowedBodyKeys.has(key)) throw new Error('unknown body key');
  }
  for (const key of [0, 13, 18]) {
    const set = mapValue(body, key);
    const array = set ? unwrapSet(set) : undefined;
    if (
      set &&
      (!array || ([13, 18].includes(key) && !array.children?.length))
    ) {
      throw new Error('wrong input set shape or tag');
    }
  }
  const outputs = mapValue(body, 1);
  if (outputs?.major !== 4) throw new Error('invalid output list');
  outputs.children?.forEach((output) =>
    validateTransactionOutput(bytes, output)
  );
  const collateralReturn = mapValue(body, 16);
  if (collateralReturn) validateTransactionOutput(bytes, collateralReturn);
  const allowedWitnessKeys = new Set(manifest.inventory.conway.witnessKeys);
  for (const entry of witnessSet.entries || []) {
    const key = Number(entry.key.value);
    if (entry.key.major !== 0 || !allowedWitnessKeys.has(key)) {
      throw new Error('unknown witness key');
    }
    if (key !== 5 && entry.value.major !== 4 && entry.value.major !== 6) {
      throw new Error('invalid witness set field');
    }
    if (key === 5) validateRedeemers(entry.value);
  }
};

const validatePolicyCase = (cborHex: string, validator: string) => {
  const bytes = Buffer.from(cborHex, 'hex');
  const root = parseItem(bytes);
  if (root.end !== bytes.length) throw new Error('trailing bytes');
  if (validator === 'complete-item') return;
  if (validator === 'set' || validator === 'nonempty-set') {
    const set = unwrapSet(root);
    if (!set || (validator === 'nonempty-set' && !set.children?.length))
      throw new Error('invalid set');
    return;
  }
  if (validator === 'transaction-output') {
    validateTransactionOutput(bytes, root);
    return;
  }
  if (validator === 'conway-envelope') {
    validateStrictEnvelope(bytes);
    return;
  }
  if (validator === 'redeemers') {
    validateRedeemers(root);
    return;
  }
  if (validator === 'auxiliary-data') {
    validateAuxiliaryData(root);
    return;
  }
  if (validator === 'embedded-cbor') {
    validateEmbeddedCbor(bytes, root);
    return;
  }
  if (validator === 'rational') {
    if (
      root.major !== 6 ||
      root.tag !== BigInt(30) ||
      root.children?.[0].major !== 4 ||
      root.children[0].children?.length !== 2
    ) {
      throw new Error('invalid rational');
    }
    return;
  }
  if (validator === 'auxiliary') {
    if (
      root.major !== 6 ||
      root.tag !== BigInt(259) ||
      root.children?.[0].major !== 5
    ) {
      throw new Error('invalid auxiliary tag');
    }
    return;
  }
  if (validator === 'known-tag') {
    const knownTags = new Set([
      2,
      3,
      24,
      30,
      102,
      121,
      122,
      123,
      124,
      125,
      126,
      127,
      258,
      259,
    ]);
    if (root.major !== 6 || !knownTags.has(Number(root.tag))) {
      throw new Error('unknown tag');
    }
    return;
  }
  throw new Error(`unknown policy validator ${validator}`);
};

describe('exact CBOR validation evidence', () => {
  it.each(manifest.fixtures)(
    'preserves the fixed exact spans for $id',
    ({ file, expected }) => {
      const fixture = readFixture(file);
      expect(
        createHash('sha256')
          .update(Buffer.from(fixture.cborHex, 'hex'))
          .digest('hex')
      ).toBe(fixture.provenance.fixtureSha256);
      expect(extractSpans(fixture)).toEqual(expected);
    }
  );

  it('reproduces the ledger-accepted Babbage and untagged fixture derivations', () => {
    const base = fixtureById('conway-regression-collateral');
    expect(deriveBabbageMapOutputs(base).toString('hex')).toBe(
      fixtureById('conway-babbage-map-outputs').cborHex
    );
    expect(deriveUntaggedBodySets(base).toString('hex')).toBe(
      fixtureById('conway-untagged-body-sets').cborHex
    );
  });

  it.each(manifest.sdkCases)(
    'enforces the frozen strict result for $id',
    ({ fixtureId, mutation, strictExpected }) => {
      const bytes = mutate(fixtureById(fixtureId), mutation);
      let rejected = false;
      try {
        validateStrictEnvelope(bytes);
      } catch (_error) {
        rejected = true;
      }
      expect(rejected).toBe(strictExpected === 'reject');
    }
  );

  it.each(manifest.policyCases)(
    'enforces the targeted encoding policy for $id',
    ({ cborHex, validator, expected }) => {
      let rejected = false;
      try {
        validatePolicyCase(cborHex, validator);
      } catch (_error) {
        rejected = true;
      }
      expect(rejected).toBe(expected === 'reject');
    }
  );

  it('keeps the manifest aligned with the generated pinned-source inventory', () => {
    const sourceInventory = JSON.parse(
      fs.readFileSync(
        path.join(fixtureDirectory, manifest.sourceInventoryFile),
        'utf8'
      )
    );
    expect(sourceInventory.eras.conway.sourceSha256).toBe(
      'ab2325fea52b97ab7792ccf9fefcc6dafb543e0bc795e9b761d3f2989b223271'
    );
    expect(sourceInventory.eras.dijkstra.sourceSha256).toBe(
      '0b7062eab1011c80dae7e0849f1414ec79183405473ffccb96c34d545bea2ee1'
    );
    expect(manifest.inventory.conway.bodyKeys).toEqual(
      sourceInventory.eras.conway.maps.transactionBody.map(({ key }) => key)
    );
    expect(manifest.inventory.conway.witnessKeys).toEqual(
      sourceInventory.eras.conway.maps.witnessSet.map(({ key }) => key)
    );
    expect(manifest.inventory.conway.certificateTags).toEqual(
      sourceInventory.eras.conway.discriminants.certificates.map(
        ({ tag }) => tag
      )
    );
    expect(manifest.inventory.conway.governanceActionTags).toEqual(
      sourceInventory.eras.conway.discriminants.governanceActions.map(
        ({ tag }) => tag
      )
    );
    expect(sourceInventory.eras.conway.discriminants.nativeScripts).toEqual([
      { name: 'script_pubkey', tag: 0 },
      { name: 'script_all', tag: 1 },
      { name: 'script_any', tag: 2 },
      { name: 'script_n_of_k', tag: 3 },
      { name: 'script_invalid_before', tag: 4 },
      { name: 'script_invalid_hereafter', tag: 5 },
    ]);
    expect(
      [
        'plutus_data',
        'metadata',
        'withdrawals',
        'mint',
        'required_signers',
        'vkeywitness',
        'bootstrap_witness',
        'constr<a0>',
      ].every((name) => sourceInventory.eras.conway.definitions[name])
    ).toBe(true);
    expect(manifest.inventory.conway.reservedBodyKeys).toEqual([6, 10, 12]);
    expect(
      sourceInventory.eras.dijkstra.maps.subTransactionBody.map(
        ({ key }) => key
      )
    ).toContain(24);
    expect(
      sourceInventory.eras.dijkstra.maps.auxiliaryDataMap.map(({ key }) => key)
    ).toContain(5);
    expect(
      sourceInventory.eras.dijkstra.maps.protocolParameterUpdate
        .map(({ key }) => key)
        .filter((key) => key >= 34)
    ).toEqual(manifest.inventory.dijkstraDeltas.addedProtocolParameterKeys);
    expect(
      sourceInventory.eras.dijkstra.definitions.protocol_version
    ).toContain('uint .size 4');
    expect(sourceInventory.eras.dijkstra.definitions.native_script).toContain(
      'script_require_guard'
    );
    expect(sourceInventory.eras.dijkstra.definitions.redeemer_tag).toContain(
      '/ 6 ; guarding'
    );
    expect(
      sourceInventory.eras.conway.definitions.transaction_output
    ).toContain('alonzo_transaction_output/ babbage_transaction_output');
    const wirePolicyFamilies = new Set(
      manifest.wirePolicy.map(({ family }) => family)
    );
    expect(wirePolicyFamilies.size).toBe(manifest.wirePolicy.length);
    expect(
      manifest.protocolContextRules.every(({ owners }) => owners.length > 0)
    ).toBe(true);
  });

  it('matches fixed spans to independently hashed cbor-diag annotations', () => {
    const annotations = JSON.parse(
      fs.readFileSync(
        path.join(fixtureDirectory, manifest.spanAnnotationsFile),
        'utf8'
      )
    );
    for (const annotation of annotations.fixtures) {
      const expected = manifest.fixtures.find(({ id }) => id === annotation.id)
        ?.expected;
      if (!expected)
        throw new Error(`missing fixture annotation ${annotation.id}`);
      expect(annotation.ranges).toEqual({
        body: [expected.body.start, expected.body.end],
        witnessSet: [expected.witnessSet.start, expected.witnessSet.end],
        isValid: [expected.isValid.start, expected.isValid.end],
        auxiliaryData: [
          expected.auxiliaryData.start,
          expected.auxiliaryData.end,
        ],
        outputs: expected.outputs.map(({ start, end }) => [start, end]),
        collateralReturn: expected.collateralReturn
          ? [expected.collateralReturn.start, expected.collateralReturn.end]
          : null,
      });
      expect(annotation.annotatedOutputSha256).toMatch(/^[0-9a-f]{64}$/);
    }
  });

  it('records normalized results for the same installed and candidate cases', () => {
    const expectedCaseIds = manifest.sdkCases.map(({ id }) => id).sort();
    for (const expected of manifest.sdkResults) {
      const result = JSON.parse(
        fs.readFileSync(path.join(fixtureDirectory, expected.file), 'utf8')
      );
      expect(result.schemaVersion).toBe(1);
      expect(result.sdk.version).toBe(expected.version);
      expect(result.cases.map(({ id }) => id)).toEqual(expectedCaseIds);
      expect(result.summary.strictRejectsAcceptedBySdk).toBe(
        expected.strictRejectsAcceptedBySdk
      );
      expect(
        result.cases.find(({ id }) => id === 'trailing-root-byte')
          .measuredFullConsumption
      ).toBe(false);
      expect(
        result.cases.find(({ id }) => id === 'ledger-conway-positive')
          .representedFields
      ).toEqual([
        'collateralReturn',
        'collaterals',
        'fee',
        'inputs',
        'outputs',
        'scriptIntegrityHash',
        'totalCollateral',
      ]);
      expect(result.sdk.dependencyLockSha256).toBe(
        expected.version === '0.47.0'
          ? '4856d1faeb85ff6e6cb90df2a100003bb0017e1f175f7ebd77498f0a4365e9d4'
          : null
      );
      expect(
        result.cases.every(
          ({ errorClass }) =>
            typeof errorClass !== 'string' || !errorClass.includes('/')
        )
      ).toBe(true);
    }
  });
});
