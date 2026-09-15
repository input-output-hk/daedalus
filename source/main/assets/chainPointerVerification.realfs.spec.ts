/**
 * The local confirmation, against a real block and against synthetic ones.
 *
 * `.realfs.` because it writes an immutable database to a temporary directory
 * and reads it back through the same code path the application uses. Nothing
 * here reaches the network: the transaction the index would have supplied is
 * reassembled from the block, and the recording says why that is the same bytes.
 *
 * The negative cases carry the weight. A positive case proves the happy path
 * agrees with itself; what has to be true is that a tampered byte anywhere in
 * the chain of hashes produces no row.
 *
 * @jest-environment node
 */
import fs from 'fs';
import os from 'os';
import path from 'path';
import blake2b from 'blake2b';
import * as cbor from 'cbor';

import { confirmChainPointer } from './chainPointerVerification';
import type { PointerCandidate } from './chainPointerVerification';
import {
  IMMUTABLE_PRIMARY_INDEX_VERSION,
  IMMUTABLE_SECONDARY_ENTRY_BYTES,
  ImmutableBlockReader,
} from './immutableBlockReader';
import { arraySpans, mapSpans, readHead } from './cborSpan';
import { PREPROD_BLOCK, PREPROD_BLOCK_HEX } from './chainPointer.fixture';

const CHUNK_SIZE = 21600;

const digest = (input: Uint8Array): string =>
  Buffer.from(blake2b(32).update(Uint8Array.from(input)).digest()).toString(
    'hex'
  );

/**
 * A miniature immutable database holding one block.
 *
 * The layout is the one `immutableBlockReader` documents: chunk zero's primary
 * index is what says how many slots a chunk holds, so it is written at full
 * length even though it holds no blocks.
 */
const writeDatabase = (
  directory: string,
  block: Buffer,
  slot: number,
  headerHash: string,
  options: { primaryVersion?: number } = {}
) => {
  fs.mkdirSync(directory, { recursive: true });
  const version = options.primaryVersion ?? IMMUTABLE_PRIMARY_INDEX_VERSION;

  const emptyPrimary = Buffer.alloc(1 + (CHUNK_SIZE + 2) * 4);
  emptyPrimary[0] = version;
  fs.writeFileSync(path.join(directory, '00000.primary'), emptyPrimary);
  fs.writeFileSync(path.join(directory, '00000.secondary'), Buffer.alloc(0));
  fs.writeFileSync(path.join(directory, '00000.chunk'), Buffer.alloc(0));

  const chunk = Math.floor(slot / CHUNK_SIZE);
  const relative = (slot % CHUNK_SIZE) + 1;
  const primary = Buffer.alloc(1 + (CHUNK_SIZE + 2) * 4);
  primary[0] = version;
  for (let index = relative + 1; index <= CHUNK_SIZE + 1; index += 1) {
    primary.writeUInt32BE(IMMUTABLE_SECONDARY_ENTRY_BYTES, 1 + index * 4);
  }

  const secondary = Buffer.alloc(IMMUTABLE_SECONDARY_ENTRY_BYTES);
  secondary.writeBigUInt64BE(BigInt(0), 0);
  secondary.writeUInt16BE(3, 8);
  secondary.writeUInt16BE(0, 10);
  secondary.writeUInt32BE(0, 12);
  Buffer.from(headerHash, 'hex').copy(secondary, 16);
  secondary.writeBigUInt64BE(BigInt(slot), 48);

  const name = String(chunk).padStart(5, '0');
  fs.writeFileSync(path.join(directory, `${name}.primary`), primary);
  fs.writeFileSync(path.join(directory, `${name}.secondary`), secondary);
  fs.writeFileSync(path.join(directory, `${name}.chunk`), block);
};

/**
 * The transaction at `transactionIndex`, reassembled from the block the way the
 * index returns it. The header recording says this is byte-identical to what
 * `tx_cbor` answered for the same transaction.
 */
const transactionFromBlock = (block: Buffer, transactionIndex: number) => {
  const outer = arraySpans(block, 0);
  const inner = arraySpans(block, outer[1].start);
  const bodies = arraySpans(block, inner[1].start);
  const witnesses = arraySpans(block, inner[2].start);
  const auxiliary = mapSpans(block, inner[3].start).find(
    (entry) =>
      Number(readHead(block, entry.key.start).argument) === transactionIndex
  );
  const invalid =
    inner.length > 4
      ? arraySpans(block, inner[4].start).map((span) =>
          Number(readHead(block, span.start).argument)
        )
      : [];
  const isValid = !invalid.includes(transactionIndex);
  return Buffer.concat([
    Buffer.from([0x84]),
    block.subarray(
      bodies[transactionIndex].start,
      bodies[transactionIndex].end
    ),
    block.subarray(
      witnesses[transactionIndex].start,
      witnesses[transactionIndex].end
    ),
    Buffer.from([isValid ? 0xf5 : 0xf4]),
    block.subarray(auxiliary.value.start, auxiliary.value.end),
  ]);
};

const REAL_BLOCK = Buffer.from(PREPROD_BLOCK_HEX, 'hex');
const REAL_TX = transactionFromBlock(
  REAL_BLOCK,
  PREPROD_BLOCK.transactionIndex
);

let directories: Array<string> = [];

const makeReader = (
  block: Buffer,
  slot: number,
  headerHash: string,
  options: { primaryVersion?: number } = {}
) => {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'immutable-'));
  directories.push(directory);
  writeDatabase(directory, block, slot, headerHash, options);
  return new ImmutableBlockReader(directory);
};

const realCandidate = (
  overrides: Partial<PointerCandidate> = {}
): PointerCandidate => ({
  subject: `${PREPROD_BLOCK.policyId}${PREPROD_BLOCK.assetName}`,
  policyId: PREPROD_BLOCK.policyId,
  assetName: PREPROD_BLOCK.assetName,
  txHash: PREPROD_BLOCK.transactionHash,
  blockHash: PREPROD_BLOCK.hash,
  absoluteSlot: PREPROD_BLOCK.slot,
  cbor: REAL_TX.toString('hex'),
  ...overrides,
});

// ---------------------------------------------------------------------------
// Synthetic transactions, for the auxiliary-data shapes the recorded block does
// not cover and for tampering that has to leave the rest of the bytes valid.
// ---------------------------------------------------------------------------

const SYNTHETIC_POLICY = 'a'.repeat(56);
const SYNTHETIC_NAME = Buffer.from('Fixture', 'utf8').toString('hex');
const SYNTHETIC_SLOT = 43_200;

type AuxiliaryShape = 'shelley' | 'mary' | 'alonzo';

const cip25Payload = (name: string) =>
  new Map<number, unknown>([
    [
      721,
      new Map<string, unknown>([
        [
          SYNTHETIC_POLICY,
          new Map<string, unknown>([
            [
              Buffer.from(SYNTHETIC_NAME, 'hex').toString('utf8'),
              new Map([['name', name]]),
            ],
          ]),
        ],
      ]),
    ],
  ]);

const auxiliaryData = (shape: AuxiliaryShape, name: string): Buffer => {
  const metadata = cip25Payload(name);
  if (shape === 'shelley') return cbor.encode(metadata);
  if (shape === 'mary') return cbor.encode([metadata, []]);
  return cbor.encode(
    new cbor.Tagged(259, new Map<number, unknown>([[0, metadata]]))
  );
};

/**
 * A native script, and the policy id it hashes to.
 *
 * The mint has to be under the script's own policy id for the closure check to
 * find it, so the id is derived rather than chosen.
 */
const nativeScript = (script: unknown) => {
  const bytes = cbor.encode(script);
  const prefixed = Buffer.concat([Buffer.from([0]), bytes]);
  const policyId = Buffer.from(
    blake2b(28).update(Uint8Array.from(prefixed)).digest()
  ).toString('hex');
  return { bytes, policyId };
};

const KEY_HASH = Buffer.alloc(28, 7);

/** `all [sig, invalid_hereafter slot]`, the ordinary closing policy. */
const closingScript = (slot: number) => [
  1,
  [
    [0, KEY_HASH],
    [5, slot],
  ],
];

const syntheticTransaction = (
  shape: AuxiliaryShape,
  options: {
    name?: string;
    quantity?: number;
    policyId?: string;
    withAuxiliary?: boolean;
    script?: unknown;
  } = {}
) => {
  const name = options.name ?? 'Fixture Token';
  const quantity = options.quantity ?? 1;
  const witnessScript = options.script ? nativeScript(options.script) : null;
  const policyId =
    options.policyId ?? witnessScript?.policyId ?? SYNTHETIC_POLICY;
  const withAuxiliary = options.withAuxiliary ?? true;
  const auxiliary = auxiliaryData(shape, name);

  const mint = new Map<Buffer, Map<Buffer, number>>([
    [
      Buffer.from(policyId, 'hex'),
      new Map([[Buffer.from(SYNTHETIC_NAME, 'hex'), quantity]]),
    ],
  ]);
  const body = new Map<number, unknown>([
    [0, []],
    [1, []],
    [2, 200000],
    [9, mint],
  ]);
  if (withAuxiliary) {
    body.set(7, Buffer.from(digest(auxiliary), 'hex'));
  }
  const bodyBytes = cbor.encode(body);
  // Built from bytes rather than through `cbor.encode`, because the script has
  // to appear in the witness set as the exact bytes that were hashed into the
  // policy id. Encoding a Buffer would wrap it in a byte string instead.
  // `a1 01 81` is a one-entry map, key 1, holding a one-element array.
  const witnessBytes = witnessScript
    ? Buffer.concat([Buffer.from([0xa1, 0x01, 0x81]), witnessScript.bytes])
    : cbor.encode(new Map());

  const transaction =
    shape === 'alonzo'
      ? Buffer.concat([
          Buffer.from([0x84]),
          bodyBytes,
          witnessBytes,
          Buffer.from([0xf5]),
          withAuxiliary ? auxiliary : Buffer.from([0xf6]),
        ])
      : Buffer.concat([
          Buffer.from([0x83]),
          bodyBytes,
          witnessBytes,
          withAuxiliary ? auxiliary : Buffer.from([0xf6]),
        ]);

  // The block is assembled from bytes rather than through `cbor.encode`,
  // because the transaction bodies have to appear in it as the exact bytes that
  // were hashed. `82 07` is the era wrapper, `85` the five-part block, `66` a
  // six-character header stand-in, `81` each one-element list, `a1 00` the
  // auxiliary-data map keyed by transaction index, and `80` the empty list of
  // invalid transactions.
  const block = Buffer.concat([
    Buffer.from([0x82, 0x07, 0x85, 0x66]),
    Buffer.from('header', 'utf8'),
    Buffer.from([0x81]),
    bodyBytes,
    Buffer.from([0x81]),
    witnessBytes,
    Buffer.from([0xa1, 0x00]),
    withAuxiliary ? auxiliary : Buffer.from([0xf6]),
    Buffer.from([0x80]),
  ]);

  const headerHash = digest(Buffer.from('header', 'utf8'));
  return {
    transaction,
    block,
    headerHash,
    txHash: digest(bodyBytes),
    policyId,
    candidate: (
      overrides: Partial<PointerCandidate> = {}
    ): PointerCandidate => ({
      subject: `${policyId}${SYNTHETIC_NAME}`,
      policyId,
      assetName: SYNTHETIC_NAME,
      txHash: digest(bodyBytes),
      blockHash: headerHash,
      absoluteSlot: SYNTHETIC_SLOT,
      cbor: transaction.toString('hex'),
      ...overrides,
    }),
  };
};

afterEach(() => {
  directories.forEach((directory) => {
    try {
      fs.rmSync(directory, { recursive: true, force: true });
    } catch {
      // A directory the test never created is not a failure.
    }
  });
  directories = [];
});

describe('confirmChainPointer', () => {
  it('confirms a recorded pointer against the block it names', () => {
    const reader = makeReader(
      REAL_BLOCK,
      PREPROD_BLOCK.slot,
      PREPROD_BLOCK.hash
    );
    const result = confirmChainPointer(realCandidate(), { reader });
    expect(result.status).toBe('confirmed');
    if (result.status !== 'confirmed') return;
    expect(result.slot).toBe(PREPROD_BLOCK.slot);
    expect(result.cip25).not.toBeNull();
    expect(result.cip25.name).toEqual(['Northwind Demo']);
  });

  it('rejects the asset the same transaction burns', () => {
    const reader = makeReader(
      REAL_BLOCK,
      PREPROD_BLOCK.slot,
      PREPROD_BLOCK.hash
    );
    const result = confirmChainPointer(
      realCandidate({
        assetName: PREPROD_BLOCK.burnedAssetName,
        subject: `${PREPROD_BLOCK.policyId}${PREPROD_BLOCK.burnedAssetName}`,
      }),
      { reader }
    );
    expect(result).toEqual({
      status: 'rejected',
      reason: 'mint-does-not-name-subject',
    });
  });

  it('rejects a tampered transaction body', () => {
    const reader = makeReader(
      REAL_BLOCK,
      PREPROD_BLOCK.slot,
      PREPROD_BLOCK.hash
    );
    const tampered = Buffer.from(REAL_TX);
    // The fee is inside the body, so changing it changes the transaction id.
    tampered[20] ^= 0xff;
    const result = confirmChainPointer(
      realCandidate({ cbor: tampered.toString('hex') }),
      { reader }
    );
    expect(result.status).toBe('rejected');
  });

  it('rejects tampered auxiliary data', () => {
    const reader = makeReader(
      REAL_BLOCK,
      PREPROD_BLOCK.slot,
      PREPROD_BLOCK.hash
    );
    const tampered = Buffer.from(REAL_TX);
    // The last byte of the transaction is inside the auxiliary data, so the
    // body still hashes to its claimed id and only key 7 disagrees.
    tampered[tampered.length - 1] ^= 0xff;
    const result = confirmChainPointer(
      realCandidate({ cbor: tampered.toString('hex') }),
      { reader }
    );
    expect(result).toEqual({
      status: 'rejected',
      reason: 'auxiliary-data-hash',
    });
  });

  it('rejects a pointer to a block the chain does not hold', () => {
    const reader = makeReader(REAL_BLOCK, PREPROD_BLOCK.slot, 'b'.repeat(64));
    const result = confirmChainPointer(realCandidate(), { reader });
    expect(result).toEqual({
      status: 'rejected',
      reason: 'block-not-in-chain',
    });
  });

  it('rejects a transaction that is not in the block it names', () => {
    const other = syntheticTransaction('alonzo');
    const reader = makeReader(
      REAL_BLOCK,
      PREPROD_BLOCK.slot,
      PREPROD_BLOCK.hash
    );
    const result = confirmChainPointer(
      other.candidate({
        blockHash: PREPROD_BLOCK.hash,
        absoluteSlot: PREPROD_BLOCK.slot,
      }),
      { reader }
    );
    expect(result).toEqual({
      status: 'rejected',
      reason: 'transaction-not-in-block',
    });
  });

  it('leaves a pointer newer than the immutable database unresolved', () => {
    const reader = makeReader(
      REAL_BLOCK,
      PREPROD_BLOCK.slot,
      PREPROD_BLOCK.hash
    );
    const result = confirmChainPointer(
      realCandidate({ absoluteSlot: PREPROD_BLOCK.slot + 1 }),
      { reader }
    );
    expect(result).toEqual({
      status: 'pending',
      reason: 'beyond-immutable-tip',
      tipSlot: PREPROD_BLOCK.slot,
    });
  });

  it('resolves at the tip and is pending one slot past it', () => {
    const reader = makeReader(
      REAL_BLOCK,
      PREPROD_BLOCK.slot,
      PREPROD_BLOCK.hash
    );
    expect(confirmChainPointer(realCandidate(), { reader }).status).toBe(
      'confirmed'
    );
    expect(
      confirmChainPointer(
        realCandidate({ absoluteSlot: PREPROD_BLOCK.slot + 1 }),
        { reader }
      ).status
    ).toBe('pending');
  });

  it('fails closed on a primary index version it does not know', () => {
    const reader = makeReader(
      REAL_BLOCK,
      PREPROD_BLOCK.slot,
      PREPROD_BLOCK.hash,
      { primaryVersion: 2 }
    );
    const result = confirmChainPointer(realCandidate(), { reader });
    expect(result.status).toBe('unavailable');
  });

  it('fails closed when there is no immutable database at all', () => {
    const reader = new ImmutableBlockReader(
      path.join(os.tmpdir(), 'immutable-does-not-exist')
    );
    const result = confirmChainPointer(realCandidate(), { reader });
    expect(result.status).toBe('unavailable');
  });

  it('rejects a pointer whose subject does not match its parts', () => {
    const reader = makeReader(
      REAL_BLOCK,
      PREPROD_BLOCK.slot,
      PREPROD_BLOCK.hash
    );
    const result = confirmChainPointer(
      realCandidate({ subject: 'ff'.repeat(30) }),
      { reader }
    );
    expect(result).toEqual({ status: 'rejected', reason: 'subject-mismatch' });
  });

  it('rejects a transaction that is not CBOR at all', () => {
    const reader = makeReader(
      REAL_BLOCK,
      PREPROD_BLOCK.slot,
      PREPROD_BLOCK.hash
    );
    const result = confirmChainPointer(realCandidate({ cbor: 'ff' }), {
      reader,
    });
    expect(result.status).toBe('rejected');
  });
});

describe('confirmChainPointer across the auxiliary-data shapes', () => {
  const shapes: Array<[AuxiliaryShape, string]> = [
    ['shelley', 'the bare Shelley metadata map'],
    ['mary', 'the Allegra and Mary two-element array'],
    ['alonzo', 'the Alonzo tag 259 map'],
  ];

  shapes.forEach(([shape, description]) => {
    it(`reads a CIP-25 name out of ${description}`, () => {
      const built = syntheticTransaction(shape, { name: `${shape} token` });
      const reader = makeReader(built.block, SYNTHETIC_SLOT, built.headerHash);
      const result = confirmChainPointer(built.candidate(), { reader });
      expect(result.status).toBe('confirmed');
      if (result.status !== 'confirmed') return;
      expect(result.cip25).toEqual({ name: `${shape} token` });
    });
  });

  it('rejects a mint of zero and a burn', () => {
    [0, -1].forEach((quantity) => {
      const built = syntheticTransaction('alonzo', { quantity });
      const reader = makeReader(built.block, SYNTHETIC_SLOT, built.headerHash);
      expect(confirmChainPointer(built.candidate(), { reader })).toEqual({
        status: 'rejected',
        reason: 'mint-does-not-name-subject',
      });
    });
  });

  it('rejects a mint under a different policy', () => {
    const built = syntheticTransaction('alonzo');
    const other = 'c'.repeat(56);
    const reader = makeReader(built.block, SYNTHETIC_SLOT, built.headerHash);
    expect(
      confirmChainPointer(
        built.candidate({
          policyId: other,
          subject: `${other}${SYNTHETIC_NAME}`,
        }),
        { reader }
      )
    ).toEqual({
      status: 'rejected',
      reason: 'mint-does-not-name-subject',
    });
  });

  it('confirms a mint carrying no auxiliary data, with no CIP-25 name', () => {
    const built = syntheticTransaction('alonzo', { withAuxiliary: false });
    const reader = makeReader(built.block, SYNTHETIC_SLOT, built.headerHash);
    const result = confirmChainPointer(built.candidate(), { reader });
    expect(result.status).toBe('confirmed');
    if (result.status !== 'confirmed') return;
    expect(result.cip25).toBeNull();
  });
});

describe('confirmChainPointer and policy closure', () => {
  // The immutable tip in these cases is SYNTHETIC_SLOT, so a lock at a lower
  // slot has passed and one at a higher slot has not.
  const confirmWith = (script: unknown) => {
    const built = syntheticTransaction('alonzo', { script });
    const reader = makeReader(built.block, SYNTHETIC_SLOT, built.headerHash);
    return confirmChainPointer(built.candidate(), { reader });
  };

  it('reports a policy whose time lock has passed as closed', () => {
    const result = confirmWith(closingScript(SYNTHETIC_SLOT - 1));
    expect(result.status).toBe('confirmed');
    if (result.status !== 'confirmed') return;
    expect(result.policyClosed).toBe(true);
  });

  it('reports a policy whose time lock is still ahead as open', () => {
    const result = confirmWith(closingScript(SYNTHETIC_SLOT + 10_000));
    expect(result.status).toBe('confirmed');
    if (result.status !== 'confirmed') return;
    expect(result.policyClosed).toBe(false);
  });

  it('reports a policy with no time lock as open', () => {
    const result = confirmWith([0, KEY_HASH]);
    expect(result.status).toBe('confirmed');
    if (result.status !== 'confirmed') return;
    expect(result.policyClosed).toBe(false);
  });

  // An `any` branch that never expires keeps the whole policy open, however
  // many of its siblings have.
  it('reports an any policy with one unexpiring branch as open', () => {
    const result = confirmWith([
      2,
      [
        [5, SYNTHETIC_SLOT - 1],
        [0, KEY_HASH],
      ],
    ]);
    expect(result.status).toBe('confirmed');
    if (result.status !== 'confirmed') return;
    expect(result.policyClosed).toBe(false);
  });

  it('reports a policy it cannot find a native script for as open', () => {
    const built = syntheticTransaction('alonzo');
    const reader = makeReader(built.block, SYNTHETIC_SLOT, built.headerHash);
    const result = confirmChainPointer(built.candidate(), { reader });
    expect(result.status).toBe('confirmed');
    if (result.status !== 'confirmed') return;
    expect(result.policyClosed).toBe(false);
  });

  // The recorded transaction mints under a Plutus V3 script, which is the case
  // that cannot be decided statically at all.
  it('reports a Plutus minting policy as open', () => {
    const reader = makeReader(
      REAL_BLOCK,
      PREPROD_BLOCK.slot,
      PREPROD_BLOCK.hash
    );
    const result = confirmChainPointer(realCandidate(), { reader });
    expect(result.status).toBe('confirmed');
    if (result.status !== 'confirmed') return;
    expect(result.policyClosed).toBe(false);
  });
});
