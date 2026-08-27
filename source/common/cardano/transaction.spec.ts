import fs from 'fs';
import path from 'path';
import { blake2b } from 'blakejs';
import cbor from 'cbor';

import {
  parseConwayTransactionEnvelope,
  TransactionEnvelopeError,
} from './transactionEnvelope';
import {
  decodeConwayTransaction,
  hardwareRepresentation,
  TransactionSemanticError,
} from './transaction';

const hash = (value: Buffer) =>
  Buffer.from(blake2b(value, undefined, 32)).toString('hex');
const decode = (hex: string) =>
  decodeConwayTransaction(
    parseConwayTransactionEnvelope(Buffer.from(hex, 'hex'))
  );
const body = (extra: string) => `84a4${'008001818240000200'}${extra}a0f4f6`;
const encode = (value: unknown): Buffer => cbor.encodeCanonical(value);
const parse = (
  bodyValue: Map<number, unknown>,
  witnesses: Map<number, unknown> = new Map(),
  auxiliary: unknown = null
) =>
  parseConwayTransactionEnvelope(
    encode([bodyValue, witnesses, false, auxiliary])
  );
const keyHash = Buffer.alloc(28, 0x11);
const input = (byte: number) => [Buffer.alloc(32, byte), 0];
const rewardAccount = Buffer.concat([Buffer.from([0xe1]), Buffer.alloc(28)]);
const semanticFixture = JSON.parse(
  fs.readFileSync(
    path.join(__dirname, 'fixtures', 'exact-cbor', 'semantic-conway-v1.json'),
    'utf8'
  )
) as {
  cborHex: string;
  expected: {
    body: { start: number; end: number };
    outputs: Array<{ start: number; end: number }>;
  };
  vectors: {
    scriptDataV1: {
      redeemersHex: string;
      datumsHex: string;
      languageViewMapHex: string;
      preimageHex: string;
      blake2b256: string;
    };
  };
};

describe('Conway semantic transaction', () => {
  it('keeps task-302 fixture bytes and transaction ID unchanged', () => {
    const fixture = JSON.parse(
      fs.readFileSync(
        path.join(
          __dirname,
          'fixtures',
          'exact-cbor',
          'conway-regression.json'
        ),
        'utf8'
      )
    ) as { cborHex: string };
    const envelope = parseConwayTransactionEnvelope(
      Buffer.from(fixture.cborHex, 'hex')
    );
    expect(envelope.transactionId).toBe(
      'b327eaa52a6cce81b367951a19a7fb72807419461606ddf6c7e09ab7c7b3d327'
    );
    expect(
      envelope.outputs.map((output) =>
        Buffer.from(
          envelope.cbor.subarray(output.span.start, output.span.end)
        ).toString('hex')
      )
    ).toHaveLength(2);
  });

  it('returns a complete device-neutral model while preserving semantic fixture spans', () => {
    const transaction = decode(semanticFixture.cborHex);
    expect(transaction.review).toEqual({
      complete: true,
      signable: true,
      requirements: [],
    });
    expect(transaction.envelope.spans.body).toEqual(
      semanticFixture.expected.body
    );
    expect(transaction.envelope.spans.outputs).toEqual(
      semanticFixture.expected.outputs
    );
  });

  it('makes unresolved inputs an explicit non-signable review requirement', () => {
    const input = `825820${'00'.repeat(32)}00`;
    const transaction = decode(`84a30081${input}01818240000200a0f4f6`);
    expect(transaction.review).toMatchObject({
      complete: false,
      signable: false,
    });
    expect(transaction.review.requirements[0]).toMatchObject({
      kind: 'resolved-input',
    });
  });

  it('normalizes every Conway body field without consulting hardware support', () => {
    const nativeScript = [0, keyHash];
    const policyId = Buffer.from(
      blake2b(
        Buffer.concat([Buffer.from([0]), encode(nativeScript)]),
        undefined,
        28
      )
    );
    const auxiliary = new Map([[0, 1]]);
    const normal = input(1);
    const collateral = input(2);
    const reference = input(3);
    const bodyValue = new Map<number, unknown>([
      [0, [normal]],
      [
        1,
        [
          [
            Buffer.alloc(0),
            [2, new Map([[policyId, new Map([[Buffer.alloc(0), 3]])]])],
          ],
        ],
      ],
      [2, 1],
      [3, 100],
      [
        4,
        [
          [0, [0, keyHash]],
          [9, [0, keyHash], [2]],
        ],
      ],
      [5, new Map([[rewardAccount, 4]])],
      [7, Buffer.from(hash(encode(auxiliary)), 'hex')],
      [8, 50],
      [9, new Map([[policyId, new Map([[Buffer.alloc(0), 1]])]])],
      [13, [collateral]],
      [14, [keyHash]],
      [15, 1],
      [16, [Buffer.alloc(0), 1]],
      [17, 2],
      [18, [reference]],
      [
        19,
        new Map([
          [
            [0, keyHash],
            new Map([
              [
                [Buffer.alloc(32, 4), 0],
                [1, null],
              ],
            ]),
          ],
        ]),
      ],
      [
        20,
        [
          [
            5,
            rewardAccount,
            [6],
            ['https://example.test', Buffer.alloc(32, 5)],
          ],
        ],
      ],
      [21, 10],
      [22, 1],
    ]);
    const envelope = parse(
      bodyValue,
      new Map([[1, [nativeScript]]]),
      auxiliary
    );
    const unresolved = decodeConwayTransaction(envelope);
    const transaction = decodeConwayTransaction(envelope, {
      resolvedInputs: [
        ...unresolved.inputs.normal,
        ...unresolved.inputs.collateral,
        ...unresolved.inputs.reference,
      ].map((outpoint) => ({
        outpoint,
        value:
          outpoint.transactionId ===
          unresolved.inputs.collateral[0].transactionId
            ? { coin: BigInt(3), assets: [] }
            : { coin: BigInt(0), assets: [] },
      })),
    });

    expect(transaction.review.complete).toBe(true);
    expect(transaction.validityInterval).toEqual({
      invalidBefore: BigInt(50),
      invalidHereafter: BigInt(100),
    });
    expect(transaction.mint).toHaveLength(1);
    expect(transaction.certificates.map(({ value }) => value.kind)).toEqual([
      0,
      9,
    ]);
    expect(transaction.withdrawals).toHaveLength(1);
    expect(transaction.requiredSigners).toEqual([keyHash.toString('hex')]);
    expect(transaction.collateral).toMatchObject({
      total: BigInt(2),
      maximumLoss: { coin: BigInt(2), assets: [] },
    });
    expect(transaction.governance).toMatchObject({
      treasuryValue: BigInt(10),
      donation: BigInt(1),
    });
    expect(transaction.governance.votes).toHaveLength(1);
    expect(transaction.governance.proposals).toHaveLength(1);
    expect(hardwareRepresentation(transaction).representable).toBe(false);
  });

  it('preserves negative mint quantities as burns', () => {
    const nativeScript = [0, keyHash];
    const policyId = Buffer.from(
      blake2b(
        Buffer.concat([Buffer.from([0]), encode(nativeScript)]),
        undefined,
        28
      )
    );
    const bodyValue = new Map<number, unknown>([
      [0, []],
      [1, [[Buffer.alloc(0), 0]]],
      [2, 0],
      [9, new Map([[policyId, new Map([[Buffer.alloc(0), -1]])]])],
    ]);
    const transaction = decodeConwayTransaction(
      parse(bodyValue, new Map([[1, [nativeScript]]]))
    );

    expect(transaction.mint[0].quantity).toBe(BigInt(-1));
    expect(transaction.effects).toContainEqual(
      expect.objectContaining({ kind: 'burn' })
    );
  });

  it('recognizes every Conway certificate and governance-action alternative', () => {
    const credential = [0, keyHash];
    const drep = [2];
    const interval = new cbor.Tagged(30, [1, 2]);
    const anchor = ['https://example.test', Buffer.alloc(32, 7)];
    const poolParameters = [
      keyHash,
      Buffer.alloc(32, 8),
      1,
      1,
      interval,
      rewardAccount,
      [],
      [],
      null,
    ];
    const certificates = [
      [0, credential],
      [1, credential],
      [2, credential, keyHash],
      [3, poolParameters],
      [4, keyHash, 1],
      [7, credential, 1],
      [8, credential, 1],
      [9, credential, drep],
      [10, credential, keyHash, drep],
      [11, credential, keyHash, 1],
      [12, credential, drep, 1],
      [13, credential, keyHash, drep, 1],
      [14, credential, credential],
      [15, credential, null],
      [16, credential, 1, null],
      [17, credential, 1],
      [18, credential, null],
    ];
    const actions = [
      [0, null, new Map(), null],
      [1, null, [12, 0]],
      [2, new Map([[rewardAccount, 1]]), null],
      [3, null],
      [4, null, [], new Map(), interval],
      [5, null, [anchor, null]],
      [6],
    ];
    const bodyValue = new Map<number, unknown>([
      [0, []],
      [1, [[Buffer.alloc(0), 0]]],
      [2, 0],
      [4, certificates],
      [20, actions.map((action) => [1, rewardAccount, action, anchor])],
    ]);
    const transaction = decodeConwayTransaction(parse(bodyValue));

    expect(transaction.certificates.map(({ value }) => value.kind)).toEqual([
      0,
      1,
      2,
      3,
      4,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
    ]);
    expect(
      transaction.certificates
        .slice(-3)
        .map(({ value }) => value.credentialIdentities)
    ).toEqual([
      [`key:${keyHash.toString('hex')}`],
      [`key:${keyHash.toString('hex')}`],
      [`key:${keyHash.toString('hex')}`],
    ]);
    expect(
      transaction.certificates[7].value.targetCredentialIdentities
    ).toEqual(['always-abstain']);
    expect(
      transaction.certificates.every(({ decoded }) => decoded.kind === 'array')
    ).toBe(true);
    expect(transaction.governance.proposals).toHaveLength(7);
    expect(
      transaction.governance.proposals.every(
        ({ decoded }) => decoded.kind === 'array'
      )
    ).toBe(true);
  });

  it('reviews script delegatees and committee-hot targets without demanding their proof', () => {
    const targetHash = Buffer.alloc(28, 0x44);
    const bodyValue = new Map<number, unknown>([
      [0, []],
      [1, [[Buffer.alloc(0), 0]]],
      [2, 0],
      [
        4,
        [
          [9, [0, keyHash], [1, targetHash]],
          [14, [0, keyHash], [1, targetHash]],
        ],
      ],
    ]);
    const transaction = decodeConwayTransaction(parse(bodyValue));

    expect(transaction.review.complete).toBe(true);
    expect(
      transaction.certificates.map(({ value }) => value.scriptCredentialHashes)
    ).toEqual([[], []]);
    expect(
      transaction.certificates.map(({ value }) => value.targetScriptHashes)
    ).toEqual([[targetHash.toString('hex')], [targetHash.toString('hex')]]);
  });

  it('rejects semantic duplicates in Conway ordered sets', () => {
    const certificate = [0, [0, keyHash]];
    const bodyValue = new Map<number, unknown>([
      [0, []],
      [1, [[Buffer.alloc(0), 0]]],
      [2, 0],
      [4, [certificate, certificate]],
    ]);

    expect(() => decodeConwayTransaction(parse(bodyValue))).toThrow(
      TransactionSemanticError
    );
  });

  it('verifies exact redeemer, datum, and pinned language-view script data', () => {
    const plutusV1 = Buffer.alloc(8, 0x22);
    const plutusV1Hash = Buffer.from(
      blake2b(Buffer.concat([Buffer.from([1]), plutusV1]), undefined, 28)
    ).toString('hex');
    const normal = input(6);
    const datums = [0];
    const redeemers = [[0, 0, 0, [0, 0]]];
    const languageView = Buffer.from('439f01ff', 'hex');
    const material = Buffer.concat([
      encode(redeemers),
      encode(datums),
      Buffer.concat([Buffer.from('a14100', 'hex'), languageView]),
    ]);
    expect(encode(redeemers).toString('hex')).toBe(
      semanticFixture.vectors.scriptDataV1.redeemersHex
    );
    expect(encode(datums).toString('hex')).toBe(
      semanticFixture.vectors.scriptDataV1.datumsHex
    );
    expect(material.toString('hex')).toBe(
      semanticFixture.vectors.scriptDataV1.preimageHex
    );
    expect(hash(material)).toBe(
      semanticFixture.vectors.scriptDataV1.blake2b256
    );
    const bodyValue = new Map<number, unknown>([
      [0, [normal]],
      [1, [[Buffer.alloc(0), 0]]],
      [2, 0],
      [11, Buffer.from(semanticFixture.vectors.scriptDataV1.blake2b256, 'hex')],
    ]);
    const envelope = parse(
      bodyValue,
      new Map<number, unknown>([
        [3, [plutusV1]],
        [4, datums],
        [5, redeemers],
      ])
    );
    const unresolved = decodeConwayTransaction(envelope);
    const context = {
      languageViews: new Map([[0, languageView]]),
      usedPlutusLanguages: [0] as const,
      resolvedInputs: [
        {
          outpoint: unresolved.inputs.normal[0],
          datum: {
            kind: 'hash' as const,
            hash: hash(Buffer.from([0])),
            cbor: '00',
          },
          scriptHash: plutusV1Hash,
          value: { coin: BigInt(0), assets: [] },
        },
      ],
      redeemerScriptHashes: new Map([
        [`${Buffer.alloc(32, 6).toString('hex')}:0`, plutusV1Hash],
      ]),
    };

    expect(decodeConwayTransaction(envelope, context).review.complete).toBe(
      true
    );
    const noDatumMaterial = Buffer.concat([
      encode(redeemers),
      Buffer.from('a14100439f01ff', 'hex'),
    ]);
    bodyValue.set(11, Buffer.from(hash(noDatumMaterial), 'hex'));
    const missingDatum = decodeConwayTransaction(
      parse(
        bodyValue,
        new Map<number, unknown>([
          [3, [plutusV1]],
          [5, redeemers],
        ])
      ),
      context
    );
    expect(missingDatum.review.complete).toBe(false);
    expect(missingDatum.review.requirements).toContainEqual(
      expect.objectContaining({ kind: 'datum' })
    );
    bodyValue.set(11, Buffer.alloc(32));
    expect(() =>
      decodeConwayTransaction(
        parse(
          bodyValue,
          new Map<number, unknown>([
            [3, [plutusV1]],
            [4, datums],
            [5, redeemers],
          ])
        ),
        context
      )
    ).toThrow(TransactionSemanticError);
  });

  it('binds governance policy scripts to the canonical proposal target', () => {
    const script = Buffer.alloc(9, 0x55);
    const policy = Buffer.from(
      blake2b(Buffer.concat([Buffer.from([1]), script]), undefined, 28)
    );
    const redeemers = [[5, 0, 0, [0, 0]]];
    const languageView = Buffer.from('439f01ff', 'hex');
    const material = Buffer.concat([
      encode(redeemers),
      Buffer.from('a14100439f01ff', 'hex'),
    ]);
    const anchor = ['https://example.test', Buffer.alloc(32, 0x56)];
    const proposal = [1, rewardAccount, [0, null, new Map(), policy], anchor];
    const bodyValue = new Map<number, unknown>([
      [0, []],
      [1, [[Buffer.alloc(0), 0]]],
      [2, 0],
      [11, Buffer.from(hash(material), 'hex')],
      [20, [proposal]],
    ]);
    const witnesses = new Map<number, unknown>([
      [3, [script]],
      [5, redeemers],
    ]);
    const context = {
      languageViews: new Map([[0, languageView]]),
      usedPlutusLanguages: [0] as const,
      redeemerScriptHashes: new Map([['proposal:0', policy.toString('hex')]]),
    };

    expect(
      decodeConwayTransaction(parse(bodyValue, witnesses), context).review
        .complete
    ).toBe(true);

    const missingBody = new Map(bodyValue);
    missingBody.delete(11);
    const missing = decodeConwayTransaction(
      parse(missingBody, new Map([[3, [script]]])),
      {}
    );
    expect(missing.review).toMatchObject({ complete: false, signable: false });
    expect(missing.review.requirements).toContainEqual(
      expect.objectContaining({
        kind: 'script',
        target: 'proposal:0',
      })
    );
    expect(() =>
      decodeConwayTransaction(parse(missingBody, new Map([[3, [script]]])), {
        redeemerScriptHashes: new Map([['proposal:0', '00'.repeat(28)]]),
      })
    ).toThrow('redeemer script binding mismatch');
  });

  it('hashes the script inside a tag-24 reference-script wrapper', () => {
    const nativeScript = [0, keyHash];
    const referenceScript = new cbor.Tagged(24, encode([0, nativeScript]));
    const bodyValue = new Map<number, unknown>([
      [0, []],
      [
        1,
        [
          new Map([
            [0, Buffer.alloc(0)],
            [1, 0],
            [3, referenceScript],
          ]),
        ],
      ],
      [2, 0],
    ]);
    const transaction = decodeConwayTransaction(parse(bodyValue));
    const expected = Buffer.from(
      blake2b(
        Buffer.concat([Buffer.from([0]), encode(nativeScript)]),
        undefined,
        28
      )
    ).toString('hex');

    expect(transaction.outputs[0].referenceScript).toMatchObject({
      language: 'native',
      hash: expected,
      bytes: encode(nativeScript).toString('hex'),
    });
  });
  it('accepts ordinary non-fixture Plutus script lengths for V1, V2, and V3', () => {
    const scripts = [
      Buffer.alloc(9, 1),
      Buffer.alloc(17, 2),
      Buffer.alloc(33, 3),
    ];
    const policies = scripts.map((script, index) =>
      Buffer.from(
        blake2b(
          Buffer.concat([Buffer.from([index + 1]), script]),
          undefined,
          28
        )
      )
    );
    const bodyValue = new Map<number, unknown>([
      [0, []],
      [1, [[Buffer.alloc(0), 0]]],
      [2, 0],
      [
        9,
        new Map(
          policies.map((policy) => [policy, new Map([[Buffer.alloc(0), 1]])])
        ),
      ],
    ]);
    const transaction = decodeConwayTransaction(
      parse(
        bodyValue,
        new Map<number, unknown>([
          [3, [scripts[0]]],
          [6, [scripts[1]]],
          [7, [scripts[2]]],
        ])
      )
    );

    expect(
      transaction.witnesses.plutusScripts.map(({ bytes }) => bytes.length / 2)
    ).toEqual([9, 17, 33]);
    expect(transaction.review.complete).toBe(false);
  });

  it('uses ledger ordering for spending redeemer targets', () => {
    const higher = input(9);
    const lower = input(8);
    const script = Buffer.alloc(9, 4);
    const scriptHashValue = Buffer.from(
      blake2b(Buffer.concat([Buffer.from([1]), script]), undefined, 28)
    ).toString('hex');
    const redeemers = [[0, 0, 0, [0, 0]]];
    const languageView = Buffer.from('439f01ff', 'hex');
    const material = Buffer.concat([
      encode(redeemers),
      Buffer.from('a14100439f01ff', 'hex'),
    ]);
    const bodyValue = new Map<number, unknown>([
      [0, [higher, lower]],
      [1, [[Buffer.alloc(0), 0]]],
      [2, 0],
      [11, Buffer.from(hash(material), 'hex')],
    ]);
    const envelope = parse(
      bodyValue,
      new Map<number, unknown>([
        [3, [script]],
        [5, redeemers],
      ])
    );
    const unresolved = decodeConwayTransaction(envelope);
    const lowerTarget = `${Buffer.alloc(32, 8).toString('hex')}:0`;
    const transaction = decodeConwayTransaction(envelope, {
      languageViews: new Map([[0, languageView]]),
      usedPlutusLanguages: [0],
      resolvedInputs: unresolved.inputs.normal.map((outpoint) => ({
        outpoint,
        value: { coin: BigInt(0), assets: [] },
        scriptHash:
          outpoint.transactionId === Buffer.alloc(32, 8).toString('hex')
            ? scriptHashValue
            : undefined,
      })),
      redeemerScriptHashes: new Map([[lowerTarget, scriptHashValue]]),
    });

    expect(transaction.witnesses.redeemers[0].target).toBe(lowerTarget);
  });

  it('indexes voting redeemers by voter, not by each action', () => {
    const voterHash = Buffer.alloc(28, 6);
    const actions = new Map([
      [
        [Buffer.alloc(32, 1), 0],
        [1, null],
      ],
      [
        [Buffer.alloc(32, 2), 0],
        [0, null],
      ],
    ]);
    const bodyValue = new Map<number, unknown>([
      [0, []],
      [1, [[Buffer.alloc(0), 0]]],
      [2, 0],
      [19, new Map([[[1, voterHash], actions]])],
    ]);
    const redeemer = [[4, 1, 0, [0, 0]]];

    expect(() =>
      decodeConwayTransaction(parse(bodyValue, new Map([[5, redeemer]])))
    ).toThrow('unbound redeemer');
  });

  it('does not demand spending proofs for reference inputs', () => {
    const reference = input(10);
    const envelope = parse(
      new Map<number, unknown>([
        [0, []],
        [1, [[Buffer.alloc(0), 0]]],
        [2, 0],
        [18, [reference]],
      ])
    );
    const unresolved = decodeConwayTransaction(envelope);
    const transaction = decodeConwayTransaction(envelope, {
      resolvedInputs: [
        {
          outpoint: unresolved.inputs.reference[0],
          value: { coin: BigInt(1), assets: [] },
          scriptHash: '11'.repeat(28),
          datum: { kind: 'inline', cbor: '00' },
        },
      ],
    });

    expect(transaction.review.complete).toBe(true);
  });

  it('rejects collateral returns that do not preserve non-ADA assets', () => {
    const collateral = input(11);
    const policyId = '33'.repeat(28);
    const envelope = parse(
      new Map<number, unknown>([
        [0, []],
        [1, [[Buffer.alloc(0), 0]]],
        [2, 0],
        [13, [collateral]],
        [16, [Buffer.alloc(0), 1]],
        [17, 1],
      ])
    );
    const unresolved = decodeConwayTransaction(envelope);

    expect(() =>
      decodeConwayTransaction(envelope, {
        resolvedInputs: [
          {
            outpoint: unresolved.inputs.collateral[0],
            value: {
              coin: BigInt(2),
              assets: [{ policyId, assetName: '', quantity: BigInt(1) }],
            },
          },
        ],
      })
    ).toThrow('collateral return must preserve every non-ADA asset');
  });

  it('accepts admitted high constructor tags in indefinite tag-24 bytes', () => {
    const encoded =
      '84a300800181a300400100028201d8185f42d905420080ff0200a0f4f6';
    const transaction = decode(encoded);

    expect(transaction.outputs[0].datum?.kind).toBe('inline');
    expect(transaction.outputs[0].datum?.data).toMatchObject({
      kind: 'tag',
      tag: BigInt(1280),
    });
  });

  it('fails closed on auxiliary data hash mismatch and validates its exact outer bytes', () => {
    const auxiliary = Buffer.from('a10000', 'hex');
    const valid = body(`075820${hash(auxiliary)}`).replace(
      /f6$/,
      auxiliary.toString('hex')
    );
    expect(decode(valid).auxiliaryData?.value).toBe(auxiliary.toString('hex'));
    expect(() => decode(valid.replace(/a10000$/, 'a10001'))).toThrow(
      TransactionSemanticError
    );
  });

  it('distinguishes unavailable pinned language views from a bad script-data commitment', () => {
    const material = Buffer.from('a0a0', 'hex');
    const encoded = body(`0b5820${hash(material)}`);
    expect(decode(encoded).review).toMatchObject({
      complete: false,
      signable: false,
    });
    expect(
      decodeConwayTransaction(
        parseConwayTransactionEnvelope(Buffer.from(encoded, 'hex')),
        { languageViews: new Map() }
      ).review.complete
    ).toBe(true);
    const digest = hash(material);
    const mismatch = body(
      `0b5820${digest[0] === '0' ? '1' : '0'}${digest.slice(1)}`
    );
    expect(() =>
      decodeConwayTransaction(
        parseConwayTransactionEnvelope(Buffer.from(mismatch, 'hex')),
        { languageViews: new Map() }
      )
    ).toThrow(TransactionSemanticError);
  });

  it('reports Conway governance as semantically valid but hardware-unrepresentable', () => {
    const governance = '84a40080018182400002001501a0f4f6';
    const transaction = decode(governance);
    expect(transaction.governance.treasuryValue).toBe(BigInt(1));
    expect(hardwareRepresentation(transaction)).toEqual({
      representable: false,
      unsupported: ['Conway CIP-95 governance'],
    });
  });

  it('rejects reserved and future body fields before semantic or hardware handling', () => {
    expect(() =>
      parseConwayTransactionEnvelope(
        Buffer.from('84a40080018182400002000600a0f4f6', 'hex')
      )
    ).toThrow(TransactionEnvelopeError);
    expect(() =>
      parseConwayTransactionEnvelope(
        Buffer.from('84a40080018182400002001700a0f4f6', 'hex')
      )
    ).toThrow(TransactionEnvelopeError);
  });

  it('rejects an unbound redeemer target before any hardware decision', () => {
    const redeemer = '84a3008001818240000200a1058184000000820000f4f6';
    expect(() => decode(redeemer)).toThrow(TransactionSemanticError);
  });
});
